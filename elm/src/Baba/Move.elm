module Baba.Move exposing ( doMovesAndPushes )

import Debug

import Baba.Cell exposing (..)
import Baba.LinkedGrid as LinkedGrid exposing ( Direction (..) )
import Baba.Types as Types

ensure default ls maybeResult =
    case maybeResult of
        Just result ->
            result

        _ ->
            --let
            --    dummy = Debug.log "ensure" ls
            --in
            default

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

moveOnAxisByStative : Types.Stative -> Axis -> Axis
moveOnAxisByStative stative axis = 
    let
        moveFunc : Object -> Axis -> Axis
        moveFunc object toAxis =
            if objectIs stative object then
                axis
                    |> moveToCell (getObjectId object) 0 1 (Just (LinkedGrid.getAxisDirection axis))
                    |> ensure axis [object]
            else
                toAxis
    in
        LinkedGrid.axisGet axis
            |> List.foldr moveFunc axis


push : Axis -> Maybe (Axis)
push axisWithMoveAtOrigin =
    let
        doPush : Axis -> Axis 
        doPush axis =
            moveOnAxisByStative Types.Push axis
                |> LinkedGrid.axisForward -1
                |> ensure axis [axis]

        followPushes fromAxis = 
            case LinkedGrid.axisForward 1 fromAxis of
                Just axis ->
                    let
                        -- look what's in the cell
                        contents = LinkedGrid.axisGet axis
                    in
                    -- found a(nother) push Cell, keep going
                    if cellHas Types.Push contents then
                        followPushes axis |> Maybe.map doPush

                    -- found stop, so nothing moves
                    else if cellHasAny [Types.Pull, Types.Stop] contents then
                        Nothing

                    else
                        Just fromAxis

                -- treat boundary as stop
                _ -> Nothing

    in
    followPushes axisWithMoveAtOrigin


pull : Axis -> Axis
pull axisWithMoveAtOrigin =
    let
        doPull : Axis -> Axis
        doPull axis =
            moveOnAxisByStative Types.Pull axis
                |> LinkedGrid.axisForward 1
                |> ensure axis [axis]

        followPulls : Axis -> Axis
        followPulls fromAxis = 
            case LinkedGrid.axisForward -1 fromAxis of
                Just axis ->
                    let
                        -- look what's in the cell
                        contents = LinkedGrid.axisGet axis
                    in
                    -- found a(nother) pull Cell, keep going
                    if cellHas Types.Pull contents then
                        followPulls axis |> doPull

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
                                        |> Maybe.andThen (moveToCell (getObjectId object) 0 1 Nothing)
                                        |> Maybe.map pull
                                else
                                    axis
                                        |> push
                                        |> Maybe.andThen (moveToCell (getObjectId object) 0 1 (Just direction))
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
