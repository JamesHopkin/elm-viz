module Baba.Cell exposing ( Object, Cell, Location, Grid, Axis, emptyCell, moveToCell,
                          isMove, hasMove, isPush, hasPush, isStop, hasStop, asText, asVerb,
                          getObjectId, flipDir, moveDir,
                          cellDebugString, stringListToCells, verbFromOccupant )

import List.Extra

import Baba.Types exposing (..)
import Baba.LinkedGrid as LinkedGrid exposing ( Direction (..) )

type alias Object = ( Int, Char )
type alias Cell = List Object
type alias Location = LinkedGrid.Location Cell

type alias Grid = LinkedGrid.LinkedGrid Cell
type alias Axis = LinkedGrid.Axis Cell

emptyCell : Cell
emptyCell = []

clearCellAt offset = LinkedGrid.axisSetAt offset []
clearCell = clearCellAt 0


getObjectId ( id, _ ) = id


addToCellAt offset object axis =
  let
    newContents = object :: (LinkedGrid.axisGetAt offset axis)
  in
    LinkedGrid.axisSet newContents axis
addToCell = addToCellAt 0


moveToCell : Int -> Int -> Int -> Axis -> Maybe Axis
moveToCell id from to axis =
  let
    fromContent = LinkedGrid.axisGetAt from axis
  
  in
    case List.Extra.find (Tuple.first >> ((==) id)) fromContent of
        Just obj -> 
          let
            newFromContent = List.filter (Tuple.first >> ((/=) id)) fromContent
            newToContent = obj :: LinkedGrid.axisGetAt to axis
          in
            Just
              ( axis
  -- could optimise to not replace grid twice
                  |> LinkedGrid.axisSetAt from newFromContent
                  |> LinkedGrid.axisSetAt to newToContent
              )

        _ -> Nothing


verbFromOccupant c = case c of
  'P' -> Push
  'M' -> Move
  'D' -> Defeat
  'W' -> Win
  'O' -> Open
  'C' -> Closed
  'F' -> Float_
  _ -> You

moveDir ( _, c ) = case c of
  '←' -> Just Left
  '↑' -> Just Up
  '→' -> Just Right
  '↓' -> Just Down
  _ -> Nothing

flipDir ( id, c ) = case c of
  '←' -> Just ( id, '→' )
  '↑' -> Just ( id, '↓' )
  '→' -> Just ( id, '←' )
  '↓' -> Just ( id, '↑' )
  _ -> Nothing

isMove obj = case moveDir obj of
  Nothing -> False
  _ -> True

hasMove = List.any isMove

isPush ( _, c ) = c == 'P'
hasPush cell = List.any isPush cell

isStop ( _, c ) = c == 'S'
hasStop cell = List.any isStop cell

asText cell =
  let
    filtered = cell
      |> List.map Tuple.second
      |> List.filter Char.isAlpha
  in
    case filtered of
      first :: rest -> Just first
      _ -> Nothing

asVerb cell =
  let
    filtered = cell
      |> List.map Tuple.second
      |> List.filter Char.isUpper
  in
    case filtered of
      first :: rest -> Just first
      _ -> Nothing




showIds = False
showAllContents = False

-- char of arbitrary item at the moment
cellDebugString cell =
  if showIds then
    List.map (\( id, c ) -> String.fromInt id) cell
     |> String.join ""
  else
    let
      toString c = String.fromChar (if c == '·' then '!' else c)
      chars = List.map Tuple.second cell
    in
      if showAllContents then
        case chars of
          [] -> "·"
          _ -> String.fromList chars

      else
        case chars of
          first :: second :: _ -> (toString first) ++ (toString second)
          first :: _           -> (toString first) ++ " "
          _ -> "· "


stringListToCells : List String -> List (List Cell)
stringListToCells rows =
  let
    makeCell : Char -> ( Int, List Cell ) -> ( Int, List Cell )
    makeCell c ( index, outRow ) =
      if c == '·' then ( index, [] :: outRow )
      else ( index + 1, [( index, c )] :: outRow )

    makeRow : String -> ( Int, List (List Cell) ) -> ( Int, List (List Cell) )
    makeRow s ( initialIndex, outRows ) =
        s
          |> String.toList
          |> List.foldr makeCell ( initialIndex, [] )
          |> \( index, cells ) -> ( index, cells :: outRows)

  in
    --List.foldr (String.toList >> (List.foldr makeCell) >> Tuple.second) ( 0, [] ) rows
    List.foldr makeRow ( 0, [] ) rows |> Tuple.second

