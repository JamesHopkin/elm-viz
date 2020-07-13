module Baba.Graphics exposing ( Model, Msg, init, update, view, subscription,
                                setGrid )

import Basics exposing ( toFloat, round )

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

animDurationMillis = 200

spritesLoader msg = loadFromImageUrl "links.png" (msg << TextureLoaded)

spriteWidth = 30.0
halfSpriteWidth = spriteWidth / 2.0

makeSprite x y reflect =
    { sprite = Texture.sprite { x = x, y = y, width = spriteWidth, height = 29 }
    , reflect = reflect
    }

link                = makeSprite 0   0 False
linkUp              = makeSprite 60  0 False
linkSwordLeft       = makeSprite 235 0 False
linkSwordRight      = makeSprite 235 0 True

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

renderObject spriteSheet obj animX animY alpha =
    let
        x = animX * 32
        y = animY * 32 + 5
    in
    case Cell.objectDebugChar obj of
        'z' -> 
            let
                sprite = case Cell.getObjectDirection obj of
                    Left -> linkSwordLeft
                    Right -> linkSwordRight
                    Up -> linkUp
                    _ -> link
            in
                renderSprite sprite x y alpha spriteSheet

        '=' ->
            Canvas.text [Text.font { size = 12, family = "sans-serif" }] ( x + 10, y + 20 ) "is"

        c ->
            Canvas.text [font] ( x + 5, y + 24 ) (String.fromChar c)

renderGrid spriteSheet dicts delta grid =
    let
        foldFunc : ( Int, Int, Cell.Object ) -> List Canvas.Renderable -> List Canvas.Renderable
        foldFunc ( objX, objY, obj ) acc =
            let

                ( animX, animY ) = 
                    case dicts of
                        Just ( prev, _ ) ->
                            case Dict.get (Cell.getObjectId obj) prev of
                                Just ( prevX, prevY, _ ) ->
                                    ( toFloat prevX * (1 - delta) + toFloat objX * delta
                                    , toFloat prevY * (1 - delta) + toFloat objY * delta
                                    )

                                _ ->
                                    ( toFloat objX, toFloat objY )

                        _ ->
                            ( toFloat objX, toFloat objY )


            in
            (renderObject spriteSheet obj animX animY 1.0) :: acc
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
                canvasWidth = gridWidth * 32
                canvasHeight = gridHeight * 32

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
                    [Canvas.Settings.fill (Color.rgba 0.7 0.7 0.7 1.0)]
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
                                    |> List.map (\(_, ( x, y, obj )) -> renderObject texture obj (toFloat x) (toFloat y) (1.0 - delta))

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