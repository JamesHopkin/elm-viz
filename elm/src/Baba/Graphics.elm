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

cellDimensionInt = 16
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
gt = transform [scale 2.0 2.0, translate cellDimension (cellDimension/2.0)]


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

instanceSprites = Dict.fromList
    [ ( 'a',    { x = 96, y = 128, width = 24, height = 32 } ) -- fence
    , ( 'b',    { x = 72, y = 128, width = 24, height = 32 } ) -- water
    , ( 'c',    { x = 0, y = 128, width = 24, height = 32 } ) -- rock
    , ( 'd',    { x = 48, y = 128, width = 24, height = 32 } ) -- shrub
    , ( 'f',    { x = 24, y = 128, width = 24, height = 32 } ) -- key
    ]

textSprites = Dict.fromList
    [ ( 'A',    { bg = 0, sprite = { x = 144, y = 208, width = 31, height = 16 } } ) -- fence
    , ( 'B',    { bg = 1, sprite = { x = 0, y = 224, width = 32, height = 16 } } ) -- water
    , ( 'C',    { bg = 0, sprite = { x = 72, y = 224, width = 26, height = 16 } } ) -- rock
    , ( 'D',    { bg = 1, sprite = { x = 16, y = 208, width = 32, height = 16 } } ) -- shrub
    , ( 'F',    { bg = 0, sprite = { x = 136, y = 224, width = 21, height = 16 } } ) -- key
    , ( 'N',    { bg = 1, sprite = { x = 88, y = 208, width = 23, height = 16 } } ) -- link
    , ( 'X',    { bg = 0, sprite = { x = 104, y = 224, width = 27, height = 16 } } ) -- text
    , ( '=',    { bg = 1, sprite = { x = 0, y = 208, width = 11, height = 16 } } ) -- is

    , ( 'K',    { bg = 2, sprite = { x = 40, y = 224, width = 23, height = 16 } } ) -- sink
    , ( 'L',    { bg = 2, sprite = { x = 184, y = 224, width = 20, height = 16 } } ) -- pull
    , ( 'M',    { bg = 2, sprite = { x = 208, y = 224, width = 28, height = 16 } } ) -- move
    , ( 'P',    { bg = 2, sprite = { x = 56, y = 208, width = 26, height = 16 } } ) -- push
    , ( 'S',    { bg = 2, sprite = { x = 184, y = 208, width = 26, height = 16 } } ) -- stop
    , ( 'W',    { bg = 2, sprite = { x = 160, y = 224, width = 18, height = 16 } } ) -- win
    , ( 'Y',    { bg = 2, sprite = { x = 120, y = 208, width = 21, height = 16 } } ) -- you
    ]

renderTextBG = renderNoAnimSprite { x = 40, y = 160, width = 18, height = 14 } 1.0
renderTextBG2 = renderNoAnimSprite { x = 86, y = 160, width = 18, height = 14 } 1.0


renderMessageBox = renderNoAnimSprite { x = 0, y = 160, width = 40, height = 32 } 0.5


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

        objChar = Cell.objectDebugChar obj
    in
        if objChar == 'n' then
            let
                func = case Cell.getObjectDirection obj of
                    Left -> renderLeft
                    Right -> renderRight
                    Up -> renderUp
                    _ -> renderDown
            in
            [func delta x y alpha spriteSheet]

        else
            case Dict.get objChar instanceSprites of
                Just instanceSprite ->
                    [renderNoAnimSprite instanceSprite 1.0 x y alpha spriteSheet]

                _ ->
                    case Dict.get objChar textSprites of
                        Just { bg, sprite } ->
                            [ (case bg of
                                0 -> renderTextBG
                                1 -> renderTextBG2
                                _ -> renderMessageBox
                              ) x y alpha spriteSheet
                            , renderNoAnimSprite sprite 0.6 x y alpha spriteSheet
                            ]

                        _ ->
                            [Canvas.text [gt, font] ( x - 8, y + 8 ) (String.fromChar objChar)]


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