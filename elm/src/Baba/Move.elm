module Baba.Move exposing ( doMovesAndPushes )

import Debug

import Baba.Cell exposing (..)
import Baba.LinkedGrid as LinkedGrid exposing ( Direction (..) )
import Baba.Types exposing (..)

ensure = Maybe.withDefault (Debug.todo "ensure failure")

flipCellDirection : Axis -> Axis
flipCellDirection loc =
  loc
    |> LinkedGrid.axisGet
    |> List.map (\obj -> flipDir obj |> Maybe.withDefault obj)
    |> (\cell -> LinkedGrid.axisSet cell loc)
    |> LinkedGrid.flipAxis



flipObjectAndAxis : Int -> Axis -> Axis
flipObjectAndAxis objectId axis =
  let
    flip obj =
      if getObjectId obj == objectId then
          ensure (flipDir obj)

      else
          obj

  in
    axis
      |> LinkedGrid.axisGet
      |> List.map flip
      |> (\cell -> LinkedGrid.axisSet cell axis)
      |> LinkedGrid.flipAxis


moveAndPush : Int -> Axis -> Maybe (Axis)
moveAndPush objectId axisWithMoveAtOrigin =
  let
    doPush : Axis -> Maybe Axis 
    doPush adjoiningAxis =
      let
        moveFunc : Object -> Axis -> Axis
        moveFunc ((id, _) as obj) axis =
          if isPush obj
            then
              Maybe.withDefault axis (moveToCell id -1 0 axis)
              --addToCellUnique obj axis
            else
              axis

      in
        LinkedGrid.axisGetAt -1 adjoiningAxis
          |> List.foldr moveFunc adjoiningAxis
          |> LinkedGrid.axisForward -1


    pushChain : Axis -> Bool -> Maybe (Axis)
    pushChain prevAxis shouldPush = 
      case LinkedGrid.axisForward 1 prevAxis of
        Just axis ->
          let
            -- look what's in the cell
            contents = LinkedGrid.axisGet axis
          in
            -- found a(nother) push Cell, keep going
            if hasPush contents then
              Maybe.andThen
                (if shouldPush then doPush else LinkedGrid.axisForward -1)
                (pushChain axis True)

            -- found stop, so nothing moves
            else if hasStop contents then Nothing

            -- done searching (calling code will do move)
            else if shouldPush then doPush axis

            else LinkedGrid.axisForward -1 axis

        -- treat boundary as stop
        _ -> Nothing

    followPushes : Axis -> Maybe (Axis)
    followPushes prevAxis = pushChain prevAxis False

    pushResult = 
      case followPushes axisWithMoveAtOrigin of
        Nothing -> 
          flipCellDirection axisWithMoveAtOrigin
            |> followPushes

        updatedAxis -> updatedAxis

  in
    -- finally move the move object itself
    pushResult |> Maybe.andThen (moveToCell objectId 0 1)

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
            movingContent = List.filter isMove (LinkedGrid.getContents loc)
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
        getDirection obj = Maybe.withDefault Up (moveDir obj)

        -- list of objects, grid is accumulator (will need additional state to record what got stuck)
        moveFoldFunc : Object -> Grid -> Grid
        moveFoldFunc (( id, c ) as object) innerGrid =
          let
            -- get location and direction, will never use Nothing case
            maybeUpdatedAxis = case ( LinkedGrid.at x y innerGrid, moveDir object ) of
              ( Just location, Just direction )
                -> moveAndPush id (LinkedGrid.makeAxis location direction)
              _ -> Nothing
          in
            case maybeUpdatedAxis of 
              Just updatedAxis -> LinkedGrid.gridFromAxis updatedAxis
              _ -> innerGrid

      in
        List.foldr moveFoldFunc grid movingObjects

  in
    List.foldr impl initialGrid moveCoords
