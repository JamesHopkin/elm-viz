module Baba.Destroy exposing ( doDestroys )

import Bitwise
import Debug

import Baba.Cell as Cell exposing ( Cell, Grid, Location )
import Baba.LinkedGrid as LinkedGrid exposing ( Direction (..) )

import Baba.Types as Types

-- levels of masks to skip work at various stages


unilateralDestructionMask = Types.flagsFor
    [ Types.Hot, Types.Melt
    , Types.Weak
    , Types.You, Types.Defeat
    ]

mutualDestructionMask = Types.flagsFor
    [ Types.Open, Types.Shut
    , Types.Sink
    ]

allDestructionMask = Bitwise.or unilateralDestructionMask mutualDestructionMask

or_ f g x = f x || g x 
and_ f g x = f x && g x 
not_ f x = not (f x)

doMutualDestroys : Cell -> Maybe Cell
doMutualDestroys cell = 
    let
        -- sink!
        sinkObjects = List.filter (Cell.objectIs Types.Sink) cell
        numSinkObjects = List.length sinkObjects
    in
    if numSinkObjects > 0 && numSinkObjects < List.length cell then
        Just []

    else
        -- open/closed
        if and_ (Cell.cellHas Types.Open) (Cell.cellHas Types.Shut) cell then
            List.filter (\obj -> not (Cell.objectIsAny [Types.Open, Types.Shut] obj)) cell
                |> Just

        else
            Nothing



doUnilateralDestroys : Cell -> Maybe Cell
doUnilateralDestroys cell =
    -- idea: build a list of statives to filter out
    let
        filterOut =
            (if List.length cell > 1 then [Types.Weak] else [])
         ++ (if Cell.cellHas Types.Hot cell then [Types.Melt] else [])
         ++ (if Cell.cellHas Types.Defeat cell then [Types.You] else [])

        filter = not_ (Cell.objectIsAny filterOut)
        filtered = List.filter filter cell
    in
    -- hot/melt
    if List.length filtered /= List.length cell then
        Just filtered

    else
        Nothing

foldDestroy : (Cell -> Maybe Cell) -> Location -> Location
foldDestroy f location =
    let
        contents = LinkedGrid.getContents location
    in
    case f contents of
        Just cell ->
            let
                newLoc = LinkedGrid.setContents cell location
                --dummy = Debug.log "check contents" (LinkedGrid.getContents newLoc)
            in
                newLoc
        _ ->
            location



doDestroys : Grid -> Grid
doDestroys initialGrid =
    let
        foldFunc : Location -> Grid -> Grid
        foldFunc locForCoords grid =
            let
                ( x, y ) = LinkedGrid.getLocationCoordinates locForCoords

            in
            case LinkedGrid.at x y grid of
                Just loc ->
                    let
                        orObjFlags obj n = Bitwise.or (Cell.getObjectFlags obj) n

                        contents = LinkedGrid.getContents loc
                        combinedFlags = List.foldr orObjFlags 0 contents

                        destroysToProcess =
                            if Bitwise.and combinedFlags allDestructionMask == 0 then
                                []
                            else
                                (if Bitwise.and combinedFlags mutualDestructionMask == 0 then
                                    []
                                else
                                    [doMutualDestroys]
                                )
                                ++
                                (if Bitwise.and combinedFlags unilateralDestructionMask == 0 then
                                    []
                                else
                                    [doUnilateralDestroys]
                                )

                    in
                    List.foldl foldDestroy loc destroysToProcess 
                        |> LinkedGrid.gridFromLocation

                -- just to keep compiler happy
                _ -> grid

    in
    LinkedGrid.foldLocations foldFunc initialGrid initialGrid
