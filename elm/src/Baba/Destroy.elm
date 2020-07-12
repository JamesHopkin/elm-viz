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
    [ Types.Open, Types.Closed
    , Types.Sink
    ]

allDestructionMask = Bitwise.or unilateralDestructionMask mutualDestructionMask

doMutualDestroys : Cell -> Maybe Cell
doMutualDestroys cell = 
    let
        -- sink!
        sinkObjects = List.filter (Cell.objectIs Types.Sink) cell
        numSinkObjects = List.length sinkObjects
    in
    if numSinkObjects == 0 || numSinkObjects == List.length cell then
        Nothing
    else
        Just []

doUnilateralDestroys : Cell -> Maybe Cell
doUnilateralDestroys cell = 
    let

--destroy and you?
    --    -- sink!
    --    sinkObjects = List.filter (Cell.objectIs Types.Sink) cell
    --    numSinkObjects = List.length sinkObjects
    --in
    --if numSinkObjects == 0 || numSinkObjects == List.length cell then
    --    Nothing


        dummy = 0 --Debug.log "unilateral" []
    in
        Nothing

foldDestroy : (Cell -> Maybe Cell) -> Location -> Location
foldDestroy f location =
    let
        contents = LinkedGrid.getContents location
    in
    case f contents of
        Just cell ->
            let
                newLoc = LinkedGrid.setContents location cell
                dummy = Debug.log "check contents" (LinkedGrid.getContents newLoc)
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
