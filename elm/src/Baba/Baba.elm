module Baba.Baba exposing ( Model, Msg(..), init, update, subscription,
                            turn, wait, countChars)
-- remember to remove exposure of Msg constructors!

import Bitwise

import Dict exposing ( Dict )
import Json.Decode as Decode
import Browser.Events
import List.Extra

import Baba.LinkedGrid as LinkedGrid exposing ( Direction (..) )
import Baba.Cell as Cell
import Baba.Destroy as Destroy
import Baba.Graphics as Graphics
import Baba.Move as Move
import Baba.Rules as Rules
import Baba.Types as Types




isJust maybe = case maybe of
    Just _ -> True
    _ -> False

{- little bit of thought needed for moving multiple
    some cases:
        multiple things pushing in same direction
            - first will push stuff, rest will move without pushing
        multiple things moving in different directions
            - some might not be able to move, so need to keep track
            - set new contents to be those that were stuck

    Ignore stuck ones in first attempt, i.e. just clear 
-}





--maybeAxisToString : Maybe Axis -> String
--maybeAxisToString axis =
--  axis
--    |> Maybe.map LinkedGrid.gridFromAxis
--    |> Maybe.map (LinkedGrid.toDebugString cellDebugString)
--    |> Maybe.withDefault "!" 



applyRules : Cell.Grid -> Cell.Grid
applyRules grid =
    let
        rules = Rules.lookForRules grid

        foldFunc : ( Int, Int, Cell.Object ) -> Cell.Grid -> Cell.Grid
        foldFunc ( x, y, object ) gridToUpdate =
            -- just doing statives
                    case Cell.getObjectWord object of 
                        Cell.Instance (Types.Noun objectChar) ->
                            let
                                ruleFoldFunc : Rules.Rule -> Int -> Int
                                ruleFoldFunc rule flags =
                                    case rule of
                                        Rules.Is (Types.NounSubject (Types.Noun c)) (Types.Stative stative) ->
                                            if c == objectChar then
                                                Bitwise.or flags (Types.flagFor stative)
                                            else
                                                flags
                                        _ -> flags

                                calculatedFlags = List.foldr ruleFoldFunc 0 rules 

                            in
                                if calculatedFlags == Cell.getObjectFlags object then
                                    gridToUpdate
                                else
                                    let
                                        updatedObject = Cell.setObjectFlags calculatedFlags object
                                    in
                                    LinkedGrid.at x y gridToUpdate
                                        |> Maybe.map (Cell.updateObjectInCell updatedObject)
                                        |> Maybe.map LinkedGrid.gridFromLocation
                                        |> Maybe.withDefault gridToUpdate 
                        _ -> gridToUpdate

    in
    Cell.foldObjects foldFunc grid grid

-- single step of the grid
wait : Model -> Model
wait = turnAndUpdateGraphics Nothing
    --let
    --    shouldMove obj =
    --        if Cell.objectIs Types.Move obj then
    --            Just (Cell.getObjectDirection obj)

    --        else
    --            Nothing

    --    chainDestroys maybeGrid =
    --        case maybeGrid of
    --            Just grid ->

    --in
    --grid
    --    |> applyRules
    --    |> Move.doMovesAndPushes shouldMove True
    --    |> Maybe.map Destroy.doDestroys
    --    |> Maybe.withDefault grid


turn : Maybe Direction -> List Cell.Grid -> List Cell.Grid
turn youDirection undoStack =
    case List.head undoStack of
        Just currentGrid ->
            let

                -- rules
                gridWithUpToDateRules = applyRules currentGrid

                -- you
                ( numberOfYousMoved, afterYousMoved ) =
                    case youDirection of
                        Just direction ->
                            let
                                youMoveFunc obj = 
                                    if Cell.objectIs Types.You obj then
                                        Just direction 
                                    else
                                        Nothing
                            in
                            Move.doMovesAndPushes youMoveFunc False gridWithUpToDateRules

                        _ ->
                            ( 0, gridWithUpToDateRules )

                -- moves
                shouldMove obj =
                    if Cell.objectIs Types.Move obj then
                        Just (Cell.getObjectDirection obj)

                    else
                        Nothing

                ( numberOfOthersMoved, afterAllMoves ) = Move.doMovesAndPushes shouldMove True afterYousMoved

                --dummy = Debug.log "move counts" [numberOfYousMoved, numberOfOthersMoved]

                updatedGrid =
                    if numberOfYousMoved + numberOfOthersMoved > 0 then
                        Just (Destroy.doDestroys afterAllMoves)
                    else
                        Nothing
            in
            case updatedGrid of
                Just grid ->
                    grid :: undoStack

                _ ->
                    undoStack

        _ -> []

updateGraphics model = 
    case List.head model.undoStack of
        Just grid ->
            { model
            | graphics = Graphics.setGrid grid model.graphics
            }
     
        _ ->
            model

turnAndUpdateGraphics maybeDirection model =
    { model
    | undoStack = turn maybeDirection model.undoStack
    }
    |> updateGraphics



countChars grid = 
    let
        addContentsToDict : Cell.Object -> Dict Char Int -> Dict Char Int
        addContentsToDict obj dict =
            let
                key = Cell.objectDebugChar obj
                count = Maybe.withDefault 0 (Dict.get key dict)
            in
            Dict.insert key (count + 1) dict

        addLocationsToDict : Cell.Location -> Dict Char Int -> Dict Char Int
        addLocationsToDict loc counts =
            List.foldr addContentsToDict counts (LinkedGrid.getContents loc)
    in
    LinkedGrid.foldLocations addLocationsToDict Dict.empty grid
        |> Dict.toList



-- keyboard
keyDecoder : (Msg -> msg) -> Decode.Decoder msg
keyDecoder msg =
    Decode.map (interpretKey >> msg) (Decode.field "key" Decode.string)


interpretKey : String -> Msg
interpretKey string =
    case String.uncons string of
        Just ( 'w', "" ) -> MoveYou Up
        Just ( 'd', "" ) -> MoveYou Right
        Just ( 's', "" ) -> MoveYou Down
        Just ( 'a', "" ) -> MoveYou Left
        Just ( 'u', "" ) -> Undo

        _ ->
            Dummy

type Msg
    = MoveYou Direction
    | Undo
    | Dummy
    | GraphicsMsg Graphics.Msg

type alias Model =
    { undoStack : List Cell.Grid
    , debugStr : String
    , graphics : Graphics.Model
    }

init : (Msg -> msg) -> ( Model, Cmd msg )
init _ = (
    { undoStack =
        [ LinkedGrid.fromLists Cell.emptyCell 9 9
        (Cell.stringListToCells
            [ " z      C"
            , "aaa  ww ="
            , "A=S cww M"
            , "Z=Y"
            , "W=K r"
            , "R=S   P"
            , "aaa R="
            ]
        )]
    , debugStr = ""
    , graphics = Graphics.init
    }, Cmd.none )

update : Msg -> Model -> Model 
update msg model =
    case msg of
        MoveYou direction ->
            let
                newModel = turnAndUpdateGraphics (Just direction) model

                debugStr = case newModel.undoStack of
                    grid :: _ -> 
                        --let
                        --    dummy = Debug.log "game objects" (countChars grid)
                        --in
                        List.map Rules.ruleDebugString (Rules.lookForRules grid)
                            |> String.join "\n"
                    _ -> ""
            in
            { newModel
            | debugStr = debugStr
            }

        Undo ->
            case model.undoStack of
                _ :: _ :: _ ->
                    { model
                    | undoStack = List.drop 1 model.undoStack
                    }
                    |> updateGraphics

                _ ->
                    model

        -- keyboard nonsense
        Dummy -> model

        GraphicsMsg graphicsMsg -> 
            { model 
            | graphics = Graphics.update graphicsMsg model.graphics
            }

subscription : (Msg -> msg) -> Sub msg
subscription msg = 
    Sub.batch
        [ Browser.Events.onKeyDown (keyDecoder msg)
        , Graphics.subscription (GraphicsMsg >> msg)
        ]
