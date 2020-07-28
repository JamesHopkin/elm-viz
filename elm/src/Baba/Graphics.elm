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
gt = transform [scale 2.0 2.0, translate (cellDimension/2.0) (cellDimension/2.0)]


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
                , transform [translate x y]
                ]
                ( -halfSpriteWidth, -16 ) ((frame delta) spriteSheet)
    in
    render

noAnimSprite x y = Texture.sprite { x = x, y = y, width = 24, height = 32 }

renderNoAnimSprite s scl x y alpha spriteSheet = Canvas.texture
    [gt, Canvas.Settings.Advanced.alpha alpha
    , transform ([translate x y] ++ (if scl == 1.0 then 
        []
      else [scale scl scl]))
    ]
    ( -s.width / 2.0, -s.height / 2.0 ) ((Texture.sprite s) spriteSheet)

renderRock = renderNoAnimSprite { x = 0, y = 128, width = 24, height = 32 } 1.0
renderKey = renderNoAnimSprite { x = 24, y = 128, width = 24, height = 32 } 1.0
renderShrub = renderNoAnimSprite { x = 48, y = 128, width = 24, height = 32 } 1.0
renderWater = renderNoAnimSprite { x = 72, y = 128, width = 24, height = 32 } 1.0
renderFence = renderNoAnimSprite { x = 96, y = 128, width = 24, height = 32 } 1.0

renderIs = renderNoAnimSprite { x = 0, y = 208, width = 12, height = 16 } 0.6

renderTextBG = renderNoAnimSprite { x = 40, y = 160, width = 18, height = 14 } 1.0
renderTextBG2 = renderNoAnimSprite { x = 86, y = 160, width = 18, height = 14 } 1.0


renderMessageBox = renderNoAnimSprite { x = 0, y = 160, width = 40, height = 32 } 0.5

renderShrubText = renderNoAnimSprite { x = 16, y = 208, width = 36, height = 16 } 0.6
renderPushText = renderNoAnimSprite { x = 56, y = 208, width = 29, height = 16 } 0.6

--renderRock x y alpha spriteSheet = Canvas.texture
--    [ Canvas.Settings.Advanced.alpha alpha
--    , transform [translate (x + halfSpriteWidth) y]
--    ]
--    ( -halfSpriteWidth, 0 ) (rockSprite spriteSheet)


renderLeft = renderWip 0
renderRight = renderWip 32
renderUp = renderWip 64
renderDown = renderWip 96


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
            [func delta x y alpha spriteSheet]

        'a' ->
            [renderFence x y alpha spriteSheet]

        'b' ->
            [renderWater x y alpha spriteSheet]

        'c' ->
            [renderRock x y alpha spriteSheet]

        'd' ->
            [renderShrub x y alpha spriteSheet]

        'f' ->
            [renderKey x y alpha spriteSheet]

        'D' ->
            [renderTextBG2 x y alpha spriteSheet, renderShrubText x y alpha spriteSheet]

        '=' ->
            [renderTextBG x y alpha spriteSheet, renderIs x y alpha spriteSheet]

        'P' ->
            [renderMessageBox x y alpha spriteSheet, renderPushText x y alpha spriteSheet]
            --Canvas.text [gt, Text.font { size = 12, family = "sans-serif" }] ( x + 10, y + 20 ) "is"

        c ->
            [Canvas.text [gt, font] ( x - 8, y + 8 ) (String.fromChar c)]

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
            (renderObject spriteSheet obj animDelta animX animY 1.0) ++ acc

        --scl = 0.5 * (1 - delta) + 1.5 * delta
        --messageTest = Canvas.texture
        --    [ gt
        --    , transform [translate 100 100, scale scl scl]
        --    ]
        --    ( -40 / 2.0, -32 / 2.0 ) 
        --    ((Texture.sprite { x = 0, y = 160, width = 40, height = 32 }) spriteSheet)

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
                                    |> List.concatMap (\(_, ( x, y, obj )) -> renderObject texture obj 0 (toFloat x) (toFloat y) (1.0 - delta))

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