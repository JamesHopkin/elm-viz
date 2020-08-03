module Baba.Move exposing ( doMovesAndPushes )

import Debug

import Baba.Cell exposing (..)
import Baba.LinkedGrid as LinkedGrid exposing ( Direction (..) )
import Baba.Types as Types

import Baba.Util exposing ( ensure )


flipCellDirection : Axis -> Axis
flipCellDirection loc =
    loc
        |> LinkedGrid.axisGet
        |> List.map flipDir
        |> (\cell -> LinkedGrid.axisSet cell loc)
        |> LinkedGrid.flipAxis



flipObjectAndAxis : Int -> Axis -> Axis
flipObjectAndAxis objectId axis =
    let
        flip obj =
            if getObjectId obj == objectId then
                    flipDir obj

            else
                    obj
    in
    axis
        |> LinkedGrid.axisGet
        |> List.map flip
        |> (\cell -> LinkedGrid.axisSet cell axis)
        |> LinkedGrid.flipAxis


maybeOrLazy ma f =
    case ma of
        Just a ->
            Just a

        _ ->
            f ()

maybeOrElseLazy f ma = maybeOrLazy ma f


moveOnAxis : List Int -> Axis -> Axis
moveOnAxis ids axis = moveToCell ids 0 1 (Just (LinkedGrid.getAxisDirection axis)) axis
    |> ensure axis [axis]



push : Axis -> Maybe (Axis)
push axisWithMoveAtOrigin =
    let
        doPush : List Int -> Axis -> Axis 
        doPush ids axis =
            moveOnAxis ids axis
                |> LinkedGrid.axisForward -1
                |> ensure axis [axis]

        followPushes fromAxis = 
            case LinkedGrid.axisForward 1 fromAxis of
                Just axis ->
                    let
                        -- look what's in the cell
                        contents = LinkedGrid.axisGet axis
                        pushIds = List.filter (objectIs Types.Push) contents
                            |> List.map getObjectId
                    in
                    -- found a(nother) push Cell, keep going
                    if not (List.isEmpty pushIds) then
                        followPushes axis |> Maybe.map (doPush pushIds)

                    -- found stop, so nothing moves
                    else if cellHasAny [Types.Pull, Types.Stop] contents

                        -- special case to allow keys to open doors
                        && not (cellHas Types.Shut contents && (cellHas Types.Open <| LinkedGrid.axisGet fromAxis)) then

                        Nothing

                    else
                        Just fromAxis

                -- treat boundary as stop
                _ ->
                    Nothing

    in
    followPushes axisWithMoveAtOrigin


pull : Axis -> Axis
pull axisWithMoveAtOrigin =
    let
        doPull : List Int -> Axis -> Axis
        doPull ids axis =
            moveOnAxis ids axis
                |> LinkedGrid.axisForward 1
                |> ensure axis [axis]

        followPulls : Axis -> Axis
        followPulls fromAxis = 
            case LinkedGrid.axisForward -1 fromAxis of
                Just axis ->
                    let
                        -- look what's in the cell
                        contents = LinkedGrid.axisGet axis
                        pullIds = List.filter (objectIs Types.Pull) contents
                            |> List.map getObjectId
                    in
                    -- found a(nother) pull Cell, keep going
                    if not (List.isEmpty pullIds) then
                        followPulls axis |> doPull pullIds

                    else
                        fromAxis

                -- boundary case
                _ -> fromAxis
    in
    followPulls axisWithMoveAtOrigin

type alias MoveEntry =
    { x : Int
    , y : Int
    , objects : List ( Object, Direction )
    }

doMovesAndPushes : (Object -> Maybe Direction) -> Bool -> Grid -> ( Int, Grid )
doMovesAndPushes shouldMove bounce initialGrid =
    let
        -- find moves
        moveCoords : List MoveEntry
        moveCoords =
            let
                addIfMove : Location -> List MoveEntry -> List MoveEntry
                addIfMove loc acc =
                    let
                        shouldMoveWrapper obj =
                            Maybe.map (\dir -> ( obj, dir )) (shouldMove obj)

                        movingContent = 
                            LinkedGrid.getContents loc
                                |> List.filterMap shouldMoveWrapper

                        ( x, y ) = LinkedGrid.getLocationCoordinates loc
                    in
                    case movingContent of 
                        [] ->
                            acc

                        _ ->
                            { x = x, y = y
                            , objects = movingContent
                            }
                            :: acc

            in
            LinkedGrid.foldLocations addIfMove [] initialGrid

        impl : MoveEntry -> Grid -> Grid
        impl entry grid =
            let

                -- list of objects, grid is accumulator (will need additional state to record what got stuck)
                moveFoldFunc : ( Object, Direction ) -> Grid -> Grid
                moveFoldFunc ( object, direction ) innerGrid =
                    let
                        maybeUpdatedAxis = case LinkedGrid.at entry.x entry.y innerGrid of
                            Just location ->
                                let
                                    axis = LinkedGrid.makeAxis location direction
-- should really be flipping only the object in question
                                    flipThenPush () = flipCellDirection axis |> push
                                in
                                if bounce then
                                    axis
                                        |> push
                                        |> maybeOrElseLazy flipThenPush
                                        |> Maybe.andThen (moveToCell [getObjectId object] 0 1 Nothing)
                                        |> Maybe.map pull
                                else
                                    axis
                                        |> push
                                        |> Maybe.andThen (moveToCell [getObjectId object] 0 1 (Just direction))
                                        |> Maybe.map pull

                            _ ->
                                Nothing
                    in
                    case maybeUpdatedAxis of 
                        Just updatedAxis -> LinkedGrid.gridFromAxis updatedAxis
                        _ -> innerGrid

            in
            List.foldr moveFoldFunc grid entry.objects

    in
    ( List.length moveCoords, List.foldr impl initialGrid moveCoords )
