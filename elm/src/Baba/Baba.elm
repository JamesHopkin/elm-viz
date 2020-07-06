module Baba.Baba exposing ( countChars )

import Dict exposing ( Dict )

import List.Extra

import Baba.LinkedGrid as LinkedGrid exposing ( Direction (..) )
import Baba.Cell exposing (..)







testRows =
    [ ""
    , "·a=P·"
    , "b=c"
    , "··<"
    , "··d→"
    ]


--rulesTestResult =
--    let
--      ruleStrings = List.map ruleDebugString (lookForRules testGrid)
--    in 
--      String.join "\n" ruleStrings



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





countChars grid = 
    let
        addContentsToDict : Object -> Dict Char Int -> Dict Char Int
        addContentsToDict obj dict =
            let
                key = getObjectWord obj
                count = Maybe.withDefault 0 (Dict.get key dict)
            in
            Dict.insert key (count + 1) dict

        addLocationsToDict : Location -> Dict Char Int -> Dict Char Int
        addLocationsToDict loc counts =
            List.foldr addContentsToDict counts (LinkedGrid.getContents loc)
    in
    LinkedGrid.foldLocations addLocationsToDict Dict.empty grid
        |> Dict.toList


--   ←
--   ↑
--   →
--   ↓
