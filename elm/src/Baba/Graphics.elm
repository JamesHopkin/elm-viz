module Baba.Graphics exposing ( Model, Msg, init, update, view, subscription,
                                setGrid )

import Basics exposing ( toFloat, floor )

import Browser.Events
import Dict

import Html exposing ( div, text )

import Time exposing ( Posix )

import Canvas
import Canvas.Settings
import Canvas.Settings.Text as Text
import Canvas.Settings.Advanced exposing ( Transform(..), scale, transform, translate )
import Canvas.Texture as Texture exposing ( loadFromImageUrl )
import Color

import Baba.Cell as Cell
import Baba.LinkedGrid as LinkedGrid exposing ( Direction(..) )
import Baba.Types as Types
import Baba.Util exposing (..)

animDurationMillis = 350

spriteLoaders msg =
    [ loadFromImageUrl "images/blah.png" (msg << SpritesLoaded)
    , loadFromImageUrl "images/test.png" (msg << GlyphsLoaded)
    ]

cellDimensionInt = 16
cellDimension = toFloat cellDimensionInt

spriteWidth = 24.0
halfSpriteWidth = spriteWidth / 2.0

floorf = floor >> toFloat

-- probably pass this through to all render functions
gt = transform [scale 2.0 2.0, translate cellDimension (cellDimension/2.0)]


renderSprite baseX baseY numFrames direction =
    let
        yoff = case direction of
                    Left -> 0
                    Right -> 32
                    Up -> 64
                    Down -> 96

        frame delta = Texture.sprite
            { x = baseX + (if delta > 0.95 then 0 else spriteWidth * floorf (numFrames * delta))
            , y = baseY + yoff
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


renderNoAnimSprite s scl x y alpha spriteSheet = Canvas.texture
    [gt, Canvas.Settings.Advanced.alpha alpha
    , transform (translate x y :: (if scl == 1.0 then 
        []
      else [scale scl scl]))
    ]
    ( -s.width / 2.0, -s.height / 2.0 ) (Texture.sprite s spriteSheet)

renderConnectedSprite location obj s =
    let
        sprite = case ( Cell.getObjectWord obj, location ) of
            ( Cell.Instance noun, Just loc ) ->
                let
                    hasItemToInt m = case Maybe.map (LinkedGrid.getContents >> (Cell.cellHasNoun noun)) m of
                        Just True -> 1
                        _ -> 0

                    xoffset = (LinkedGrid.above loc |> hasItemToInt) + (LinkedGrid.right loc |> hasItemToInt) * 2
                    yoffset = (LinkedGrid.left loc |> hasItemToInt) + (LinkedGrid.below loc |> hasItemToInt) * 2

                    --dummy = Debug.log "connected" [LinkedGrid.right loc |> maybeToInt, xoffset, yoffset, s.x, s.y, s.width, s.height]
                in
                { x = s.x + xoffset * s.width
                , y = s.y + yoffset * s.height
                , width = s.width, height = s.height
                }

            _ ->
                s
    in
    renderNoAnimSprite sprite

animatedSprites = Dict.fromList
    [ ( 'i', ( 0, 0, 8 ) ) -- link
    , ( 'a', ( 24 * 8, 0, 2 ) ) -- zelda
    ]

getNounSprite sprites noun = case noun of
    Types.Noun c -> Dict.get c sprites

getAnimatedSprite = getNounSprite animatedSprites

instanceSprites = Dict.fromList
    [ ( 'e',    { x = 232, y = 328, width = 16, height = 16 } ) -- fence
    , ( 'b',    { x = 232, y = 256, width = 16, height = 16 } ) -- water
    , ( 'c',    { x = 0, y = 128, width = 24, height = 32 } ) -- rock
    , ( 'd',    { x = 48, y = 128, width = 24, height = 32 } ) -- shrub
    , ( 'f',    { x = 24, y = 128, width = 24, height = 32 } ) -- key
    , ( 'g',    { x = 120, y = 128, width = 24, height = 32 } ) -- statue
    , ( 'h',    { x = 96, y = 128, width = 24, height = 32 } ) -- sign
    ]

getInstanceSprite = getNounSprite instanceSprites


getTextSprite text =
    let
        bg =
            case text of
                Types.StativeText _ -> 
                    2

                _ ->
                    0
    in
    {bg = bg, sprite = (Types.getTextInfo text).glyph}


renderTextBG = renderNoAnimSprite { x = 40, y = 160, width = 16, height = 16 } 1.0
renderTextBG2 = renderNoAnimSprite { x = 88, y = 160, width = 16, height = 16 } 1.0


renderMessageBox = renderNoAnimSprite { x = 0, y = 160, width = 40, height = 32 } 0.5


--renderLeft = renderWip 0
--renderRight = renderWip 32
--renderUp = renderWip 64
--renderDown = renderWip 96


setGrid grid model =
    { model
    | grid = Just grid
    , previousGrid = model.grid
    , lastUpdateTime = model.lastAnimTime
    }

type Msg
    = SpritesLoaded (Maybe Texture.Texture)
    | GlyphsLoaded (Maybe Texture.Texture)
    | AnimationFrame Posix

type alias Model =
    { textures : List Texture.Texture
    , grid : Maybe Cell.Grid
    , previousGrid : Maybe Cell.Grid
    , lastAnimTime : Posix
    , lastUpdateTime : Posix
    }

init : Model
init = 
    { textures = []
    , grid = Nothing
    , previousGrid = Nothing
    , lastAnimTime = Time.millisToPosix 0
    , lastUpdateTime = Time.millisToPosix 0
    }

update msg model =
    case msg of 
        SpritesLoaded maybeTexture ->
            case maybeTexture of
                Just spritesImage ->
                    { model | textures = spritesImage :: model.textures }

                _ ->
                    let
                        logFail = Debug.log "texture" ["no sprites"]
                    in
                    model

        GlyphsLoaded maybeTexture ->
            case maybeTexture of
                Just glyphImage ->
                    let
                        textures = case model.textures of
                            sprites :: _ ->
                                [sprites, glyphImage]

                            _ ->
                                [glyphImage]

                    in
                    { model | textures = textures }

                _ ->
                    let
                        logFail = Debug.log "texture" ["no glyphs"]
                    in
                    model

        AnimationFrame time ->
            { model | lastAnimTime = time }

--(( Int, Int, Object ) -> a -> a) -> a -> Grid -> a

font = Text.font { size = 24, family = "sans-serif" }

-- spriteSheet obj delta animX animY alpha
renderObject info =
    let
        x = info.animX * cellDimension
        y = info.animY * cellDimension + 5

        word = Cell.getObjectWord info.obj

    in
    case word of
        Cell.Instance noun ->

            case getAnimatedSprite noun of
                Just animatedSprite ->
                    [curry3 renderSprite animatedSprite 
                        (Cell.getObjectDirection info.obj) info.delta x y info.alpha info.spriteSheet]

                _ ->
                    case ( noun, getInstanceSprite noun ) of
                        ( Types.Noun objChar, Just instanceSprite ) ->
                            if objChar == 'e' || objChar == 'b' then
                                [renderConnectedSprite info.location info.obj instanceSprite 1.0 x y info.alpha info.spriteSheet]
                            else
                                [renderNoAnimSprite instanceSprite 1.0 x y info.alpha info.spriteSheet]

                        ( Types.Noun objChar, _ ) ->
                            [Canvas.text [gt, font] ( x - 8, y + 8 ) (String.fromChar objChar)]

        Cell.Text text ->
            case getTextSprite text of
                { bg, sprite } ->
                    [ (case bg of
                        0 -> renderTextBG
                        1 -> renderTextBG2
                        _ -> renderMessageBox
                      ) x y info.alpha info.spriteSheet
                    , renderNoAnimSprite sprite 0.6 x y info.alpha info.glyphSheet
                    ]



renderGrid spriteSheet glyphSheet dicts delta grid =
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
            renderObject
                { spriteSheet = spriteSheet
                , glyphSheet = glyphSheet
                , obj = obj
                , delta = animDelta
                , animX = animX
                , animY = animY
                , alpha = 1.0
                , location = LinkedGrid.at objX objY grid
                } ++ acc

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
              , textures = spriteLoaders msg
              } [] 
              ( Canvas.shapes  
                    [Canvas.Settings.fill (Color.rgb (74.0 / 255.0) (156.0 / 255.0) (74.0 / 255.0))]
                    [Canvas.rect (0, 0) (toFloat canvasWidth) (toFloat canvasHeight)]
                :: 
                (case model.textures of
                    sprites :: glyphs :: [] ->
                        renderGrid sprites glyphs objectsInPreviousGrid delta grid
                        ++
                        (case objectsInPreviousGrid of
                            Just ( _, destroyed ) ->
                                destroyed
                                    |> Dict.toList
                                    |> List.concatMap (\(_, ( x, y, obj )) -> renderObject
                                        { spriteSheet = sprites
                                        , glyphSheet = glyphs
                                        , obj = obj
                                        , delta = 0
                                        , animX = toFloat x
                                        , animY = toFloat y
                                        , alpha = 1.0 - delta
                                        , location = LinkedGrid.at x y grid
                                        })

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