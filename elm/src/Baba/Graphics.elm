module Baba.Graphics exposing ( Model, Msg, init, update, view, subscription,
                                setGrid )

import Basics exposing ( toFloat, round )

import Browser.Events

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

renderSprite sprite x y spriteSheet =
    let
        reflectScale = scale (if sprite.reflect then -1 else 1) 1
    in
    Canvas.texture [transform [translate (x + halfSpriteWidth) (y + 1), reflectScale]] (-halfSpriteWidth, 0) (sprite.sprite spriteSheet)

setGrid grid model =
    { model
    | grid = Just grid
    }

type Msg
    = TextureLoaded (Maybe Texture.Texture)
    | AnimationFrame Posix

type alias Model =
    { texture : Maybe Texture.Texture
    , grid : Maybe Cell.Grid
    }

init : Model
init = 
    { texture = Nothing
    , grid = Nothing
    }

update msg model =
    case msg of 
        TextureLoaded texture ->
            { model | texture = texture }

        AnimationFrame time ->
            model

--(( Int, Int, Object ) -> a -> a) -> a -> Grid -> a

font = Text.font { size = 24, family = "sans-serif" }

render spriteSheet grid =
    let
        foldFunc : ( Int, Int, Cell.Object ) -> List Canvas.Renderable -> List Canvas.Renderable
        foldFunc ( objX, objY, obj ) acc =
            let
                c = Cell.objectDebugChar obj

                x = toFloat (objX * 32) 
                y = toFloat (objY * 32 + 5) 

                el = case c of
                    'z' -> 
                        let
                            sprite = case Cell.getObjectDirection obj of
                                Left -> linkSwordLeft
                                Right -> linkSwordRight
                                Up -> linkUp
                                _ -> link
                        in
                            renderSprite sprite x y spriteSheet


                    _ ->
                        Canvas.text [font] ( x + 5, y + 24 ) (String.fromChar c)
            in
            el :: acc
    in
        Cell.foldObjects foldFunc [] grid

view msg model =
    case model.grid of
        Just grid -> 
            let
                ( gridWidth, gridHeight ) = LinkedGrid.getDimensions grid
                canvasWidth = gridWidth * 32
                canvasHeight = gridHeight * 32
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
                    Just texture -> render texture grid

                    _ -> []
                )
              )


        _ -> div [] [ text "no grid" ]

--transform [scale 1.0 1.0]
subscription : (Msg -> msg) -> Sub msg
subscription msg =
    Browser.Events.onAnimationFrame (AnimationFrame >> msg)