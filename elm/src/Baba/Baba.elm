module Baba.Baba exposing ( Model, Msg, init, update, subscription,
                            wait, countChars )

import Bitwise

import Dict exposing ( Dict )
import Json.Decode as Decode
import Browser.Events
import List.Extra

import Baba.LinkedGrid as LinkedGrid exposing ( Direction (..) )
import Baba.Cell as Cell
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
wait : Cell.Grid -> Cell.Grid
wait grid =
    grid
        |> applyRules
        |> Move.doMovesAndPushes




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
    Decode.map (toDirectionMessage >> msg) (Decode.field "key" Decode.string)


toDirectionMessage : String -> Msg
toDirectionMessage string =
    case String.uncons string of
        Just ( 'w', "" ) -> OnKeyDown Up
        Just ( 'd', "" ) -> OnKeyDown Right
        Just ( 's', "" ) -> OnKeyDown Down
        Just ( 'a', "" ) -> OnKeyDown Left

        _ ->
            Dummy

type Msg
    = OnKeyDown Direction
    | Dummy

type alias Model = Cell.Grid

init : (Msg -> msg) -> ( Model, Cmd msg )
init _ = ( LinkedGrid.fromLists Cell.emptyCell 7 7
    (Cell.stringListToCells
        [ ""
        , "···P"
        , "···↑"
        , "xP→·←PS"
        , "···↓"
        , "···P"
        ]
    ), Cmd.none )

update : Msg -> Model -> Model 
update msg model =
    case msg of
        OnKeyDown direction -> model
        _ -> model

subscription : (Msg -> msg) -> Sub msg
subscription msg = Browser.Events.onKeyDown (keyDecoder msg)
