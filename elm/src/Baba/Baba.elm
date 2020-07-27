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
import Baba.Make as Make
import Baba.Move as Move
import Baba.Rules as Rules
import Baba.Types as Types

import Baba.Util as Util

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



applyRules : Cell.Grid -> ( List Rules.Rule, Cell.Grid )
applyRules grid =
    let
        rules = Rules.lookForRules grid

        foldFunc : ( Int, Int, Cell.Object ) -> Cell.Grid -> Cell.Grid
        foldFunc ( x, y, object ) gridToUpdate =
            -- just doing statives
                    case Cell.getObjectWord object of 
                        Cell.Instance objectNoun ->
                            let
                                ruleFoldFunc : Rules.Rule -> Int -> Int
                                ruleFoldFunc rule flags =
                                    case rule of
                                        Rules.Is (Types.NounSubject noun) (Types.Stative stative) ->
                                            if Types.nounsEqual noun objectNoun then
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
    ( rules, Cell.foldObjects foldFunc grid grid )

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

curry2 f ( a, b ) = f a b

turn : Bool -> Maybe Direction -> Cell.Grid -> Maybe Cell.Grid
turn forceTransform youDirection currentGrid =
    let

        -- rules
        ( initialRules, gridWithUpToDateRules ) = applyRules currentGrid

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

        transform rules grid =
            let
                transformRules = List.filter (\r -> Util.isJust (Rules.getTransform r)) rules
            in
            if List.isEmpty transformRules then
                grid
            else
                Make.doTransformations transformRules grid

    in
    if forceTransform || numberOfYousMoved + numberOfOthersMoved > 0 then
        afterAllMoves
            |> applyRules |> Tuple.second
            |> Destroy.doDestroys 
            |> applyRules
            |> curry2 transform
            |> Just
    else
        Nothing

updateGraphics : Model -> Model
updateGraphics model = 
    case List.head model.undoStack of
        Just grid ->
            { model
            | graphics = Graphics.setGrid grid model.graphics
            }
     
        _ ->
            model

turnAndUpdateGraphics : Maybe Direction -> Model -> Model
turnAndUpdateGraphics maybeDirection model =
    case model.undoStack of
        currentGrid :: _ ->
            case turn (List.length model.undoStack == 1) maybeDirection currentGrid of
                Just newGrid ->

                    { model
                    | undoStack = newGrid :: model.undoStack
                    }
                    |> updateGraphics

                _ ->
                    model
        _ ->
            model



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

initialModel = 
    let

        grid = LinkedGrid.fromLists Cell.emptyCell 14 14
            <| Cell.stringListToCells
                [ "ZA aaaaaaaa"
                , "== a      a"
                , "YS a z  r a"
                , "  Ta   R  a"
                , "  =a A= r a"
                , "  Ka      a"
                , "aaaatttaaaaaaa"
                , "a      a     a"
                , "a      a R=P a"
                , "attt       L a"
                , "attt   a F=W a"
                , "aftt   a     a"
                , "aaaaaaaaaaaaaa"
                ]
    in
    { undoStack = [grid]
    , debugStr = ""
    , graphics = Graphics.init 
    }
    |> updateGraphics

init : (Msg -> msg) -> ( Model, Cmd msg )
init _ = ( initialModel, Cmd.none )

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
            { model | graphics = Graphics.update graphicsMsg model.graphics }

subscription : (Msg -> msg) -> Sub msg
subscription msg = 
    Sub.batch
        [ Browser.Events.onKeyDown (keyDecoder msg)
        , Graphics.subscription (GraphicsMsg >> msg)
        ]
