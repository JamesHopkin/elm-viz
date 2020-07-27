module Baba.Graphics exposing ( Model, Msg, init, update, view, subscription,
                                setGrid )

import Basics exposing ( toFloat, floor, round )

import Browser.Events
import Dict

import Html exposing ( div, text )
import Html.Attributes as Attrs

import Time exposing ( Posix )

import Canvas
import Canvas.Settings
import Canvas.Settings.Text as Text
import Canvas.Settings.Advanced exposing ( Transform(..), scale, transform, translate )
import Canvas.Texture as Texture exposing ( loadFromImageUrl )
import Color

import Baba.Cell as Cell
import Baba.LinkedGrid as LinkedGrid exposing ( Direction(..) )

animDurationMillis = 350

spritesLoader msg = loadFromImageUrl "images/blah.png" (msg << TextureLoaded)

cellDimensionInt = 20
cellDimension = toFloat cellDimensionInt

spriteWidthInt = 24
spriteWidth = 24.0
halfSpriteWidth = spriteWidth / 2.0

makeSprite x y reflect =
    { sprite = Texture.sprite { x = x, y = y, width = spriteWidth, height = 29 }
    , reflect = reflect
    }


floorf = floor >> toFloat

-- probably pass this through to all render functions
gt = transform [scale 2.0 2.0]


renderWip rowY =
    let
        frame delta = Texture.sprite
            { x = if delta > 0.95 then 0 else spriteWidth * floorf (8 * delta)
            , y = rowY
            , width = spriteWidth
            , height = 32
            }

        render delta x y alpha spriteSheet =
            Canvas.texture
                [ gt, Canvas.Settings.Advanced.alpha alpha
                , transform [translate (x + halfSpriteWidth) y]
                ]
                ( -halfSpriteWidth, 0 ) ((frame delta) spriteSheet)
    in
    render

noAnimSprite x y = Texture.sprite { x = x, y = y, width = 24, height = 32 }

renderNoAnimSprite spriteX spriteY x y alpha spriteSheet = Canvas.texture
    [ gt, Canvas.Settings.Advanced.alpha alpha
    , transform [translate (x + halfSpriteWidth) y]
    ]
    ( -halfSpriteWidth, 0 ) (noAnimSprite spriteX spriteY  spriteSheet)

renderRock = renderNoAnimSprite 0 128
renderKey = renderNoAnimSprite 24 128

--renderRock x y alpha spriteSheet = Canvas.texture
--    [ Canvas.Settings.Advanced.alpha alpha
--    , transform [translate (x + halfSpriteWidth) y]
--    ]
--    ( -halfSpriteWidth, 0 ) (rockSprite spriteSheet)


renderLeft = renderWip 0
renderRight = renderWip 32
renderUp = renderWip 64
renderDown = renderWip 96


renderSprite sprite x y alpha spriteSheet =
    let
        reflectScale = scale (if sprite.reflect then -1 else 1) 1
    in
    Canvas.texture
        [ Canvas.Settings.Advanced.alpha alpha
        , transform [translate (x + halfSpriteWidth) (y + 1), reflectScale]
        ]
        (-halfSpriteWidth, 0) (sprite.sprite spriteSheet)

setGrid grid model =
    { model
    | grid = Just grid
    , previousGrid = model.grid
    , lastUpdateTime = model.lastAnimTime
    }

type Msg
    = TextureLoaded (Maybe Texture.Texture)
    | AnimationFrame Posix

type alias Model =
    { texture : Maybe Texture.Texture
    , grid : Maybe Cell.Grid
    , previousGrid : Maybe Cell.Grid
    , lastAnimTime : Posix
    , lastUpdateTime : Posix
    }

init : Model
init = 
    { texture = Nothing
    , grid = Nothing
    , previousGrid = Nothing
    , lastAnimTime = Time.millisToPosix 0
    , lastUpdateTime = Time.millisToPosix 0
    }

update msg model =
    case msg of 
        TextureLoaded texture ->
            { model | texture = texture }

        AnimationFrame time ->
            { model | lastAnimTime = time }

--(( Int, Int, Object ) -> a -> a) -> a -> Grid -> a

font = Text.font { size = 24, family = "sans-serif" }

renderObject spriteSheet obj delta animX animY alpha =
    let
        x = animX * cellDimension
        y = animY * cellDimension + 5
    in
    case Cell.objectDebugChar obj of
        'n' -> 
            let
                func = case Cell.getObjectDirection obj of
                    Left -> renderLeft
                    Right -> renderRight
                    Up -> renderUp
                    _ -> renderDown
            in
            func delta x y alpha spriteSheet

        'c' ->
            renderRock x y alpha spriteSheet

        'f' ->
            renderKey x y alpha spriteSheet

        '=' ->
            Canvas.text [gt, Text.font { size = 12, family = "sans-serif" }] ( x + 10, y + 20 ) "is"

        c ->
            Canvas.text [gt, font] ( x + 5, y + 24 ) (String.fromChar c)

renderGrid spriteSheet dicts delta grid =
    let
        foldFunc : ( Int, Int, Cell.Object ) -> List Canvas.Renderable -> List Canvas.Renderable
        foldFunc ( objX, objY, obj ) acc =
            let

                ( animX, animY, animDelta ) = 
                    case dicts of
                        Just ( prev, _ ) ->
                            case Dict.get (Cell.getObjectId obj) prev of
                                Just ( prevX, prevY, _ ) ->
                                    ( toFloat prevX * (1 - delta) + toFloat objX * delta
                                    , toFloat prevY * (1 - delta) + toFloat objY * delta
                                    , if prevX == objX && prevY == objY then 0 else delta
                                    )

                                _ ->
                                    ( toFloat objX, toFloat objY, 0 )

                        _ ->
                            ( toFloat objX, toFloat objY, 0 )

            in
            (renderObject spriteSheet obj animDelta animX animY 1.0) :: acc
    in
        Cell.foldObjects foldFunc [] grid

millisBetween from to = Time.posixToMillis to - Time.posixToMillis from


gridToIdMap grid = 
    let
        foldFunc ( x, y, obj ) acc = ( Cell.getObjectId obj, ( x, y, obj ) ) :: acc
    in
    Cell.foldObjects foldFunc [] grid
        |> Dict.fromList

view : (Msg -> msg) -> Model -> Html.Html msg
view msg model =
    case model.grid of
        Just grid -> 
            let
                ( gridWidth, gridHeight ) = LinkedGrid.getDimensions grid
                canvasWidth = (gridWidth + 1) * cellDimensionInt * 2
                canvasHeight = (gridHeight + 1) * cellDimensionInt * 2

                delta = toFloat (millisBetween model.lastUpdateTime model.lastAnimTime)
                            / toFloat animDurationMillis

                objectsInPreviousGrid =
                    if delta < 0 || delta >= 1 then
                        Nothing
                    else
                        case model.previousGrid of
                            Nothing ->
                               Nothing

                            Just prev ->
                                let
                                    prevMap = gridToIdMap prev
                                    gridMap = gridToIdMap grid
                                in
                                    Just ( prevMap, Dict.diff prevMap gridMap )
            in
            Canvas.toHtmlWith


              { width = canvasWidth, height = canvasHeight
              , textures = [spritesLoader msg]
              } [] 
              ([ Canvas.shapes  
                    [Canvas.Settings.fill (Color.rgb (74.0 / 255.0) (156.0 / 255.0) (74.0 / 255.0))]
                    [Canvas.rect (0, 0) (toFloat canvasWidth) (toFloat canvasHeight)]
              ] ++ 
                (case model.texture of
                    Just texture ->
                        renderGrid texture objectsInPreviousGrid delta grid
                        ++
                        (case objectsInPreviousGrid of
                            Just ( _, destroyed ) ->
                                destroyed
                                    |> Dict.toList
                                    |> List.map (\(_, ( x, y, obj )) -> renderObject texture obj 0 (toFloat x) (toFloat y) (1.0 - delta))

                            _ ->
                                []
                        )


                    _ -> []
                )
              )


        _ -> div [] [ text "no grid" ]

--transform [scale 1.0 1.0]
subscription : (Msg -> msg) -> Sub msg
subscription msg =
    Browser.Events.onAnimationFrame (AnimationFrame >> msg)