module Baba.Move exposing ( doMovesAndPushes )

import Debug

import Baba.Cell exposing (..)
import Baba.LinkedGrid as LinkedGrid exposing ( Direction (..) )
import Baba.Types exposing (..)

ensure default ls maybeResult =
    case maybeResult of
        Just result ->
            result

        _ ->
            let
                dummy = Debug.log "ensure" ls
            in
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

type NextAxisResult = PushNext Axis | StopNext | SomethingElse



moveAndPush : Int -> Axis -> Maybe (Axis)
moveAndPush objectId axisWithMoveAtOrigin =
    let
        doPush : Axis -> Maybe Axis 
        doPush adjoiningAxis =
            let
                moveFunc : Object -> Axis -> Axis
                moveFunc object axis =
                    if objectIs Push object then
                        ensure axis [object] <| moveToCell (getObjectId object) 0 1 axis
                    else
                        let
                            dummy = Debug.log "push" ["non-push thing in push cell"]
                        in
                        axis

            in
            LinkedGrid.axisGet adjoiningAxis
                |> List.foldr moveFunc adjoiningAxis
                |> LinkedGrid.axisForward -1

        pushChain : Axis -> Maybe (Axis)
        pushChain axis = followPushes axis |> Maybe.andThen doPush

        followPushes fromAxis = 
            case LinkedGrid.axisForward 1 fromAxis of
                Just axis ->
                    let
                        -- look what's in the cell
                        contents = LinkedGrid.axisGet axis
                    in
                    -- found a(nother) push Cell, keep going
                    if cellHas Push contents then
                        pushChain axis

                    -- found stop, so nothing moves
                    else if cellHas Stop contents then
                        Nothing

                    else
                        Just fromAxis

                -- treat boundary as stop
                _ -> Nothing


        pushResult = 
            case followPushes axisWithMoveAtOrigin of
                Nothing -> 
                    flipCellDirection axisWithMoveAtOrigin
                        |> followPushes

                updatedAxis -> updatedAxis

    in
    -- finally move the move object itself
    pushResult |> Maybe.andThen (moveToCell objectId 0 1)

-- TRY TO REMOVE shouldPush!!!

doMovesAndPushes : Grid -> Grid
doMovesAndPushes initialGrid =
    let
        -- find moves
        moveCoords : List ( Int, Int, List Object )
        moveCoords =
            let
                addIfMove : Location -> List ( Int, Int, List Object ) -> List ( Int, Int, List Object )
                addIfMove loc acc =
                    let
                        movingContent = List.filter (objectIs Move) (LinkedGrid.getContents loc)
                        ( x, y ) = LinkedGrid.getLocationCoordinates loc

                    in
                    case movingContent of 
                        [] -> acc
                        _ -> ( x, y, movingContent ) :: acc

            in
            LinkedGrid.foldLocations addIfMove [] initialGrid

        impl : ( Int, Int, List Object ) -> Grid -> Grid
        impl ( x, y, movingObjects ) grid =
            let

                -- list of objects, grid is accumulator (will need additional state to record what got stuck)
                moveFoldFunc : Object -> Grid -> Grid
                moveFoldFunc object innerGrid =
                    let
                        -- get location and direction, will never use Nothing case
                        direction = getObjectDirection object
                        maybeUpdatedAxis = case LinkedGrid.at x y innerGrid of
                            Just location ->
                                LinkedGrid.makeAxis location direction
                                    |> moveAndPush (getObjectId object)

                            _ ->
                                Nothing
                    in
                    case maybeUpdatedAxis of 
                        Just updatedAxis -> LinkedGrid.gridFromAxis updatedAxis
                        _ -> innerGrid

            in
            List.foldr moveFoldFunc grid movingObjects

    in
    List.foldr impl initialGrid moveCoords
