module Baba.Cell exposing ( Object, Cell, Location, Grid, Axis, emptyCell, moveToCell,
                            objectIs, cellHas, asText, asVerb,
                            getObjectId, getObjectWord, getObjectDirection, makeObject, makeDirectedObject,
                            flipDir,
                            cellDebugString, stringListToCells, verbFromOccupant )

import List.Extra

import Baba.Types exposing (..)
import Baba.LinkedGrid as LinkedGrid exposing ( Direction (..) )

type alias Cell = List Object
type alias Location = LinkedGrid.Location Cell

type alias Grid = LinkedGrid.LinkedGrid Cell
type alias Axis = LinkedGrid.Axis Cell


type Object = Object Int Char Direction Int

emptyCell : Cell
emptyCell = []



clearCellAt offset = LinkedGrid.axisSetAt offset []
clearCell = clearCellAt 0

-- use id to pick direction
makeObject id c =
    let
        dir = case remainderBy 4 id of
            0 -> Up
            1 -> Right
            2 -> Down
            _ -> Left
    in
    Object id c dir 0

makeDirectedObject id c direction =
    Object id c direction 0

getObjectId object = case object of
    Object id _ _ _ -> id

getObjectWord object = case object of
    Object _ word _ _ -> word

getObjectDirection object = case object of
    Object _ _ direction _ -> direction

getObjectFlags object = case object of
    Object _ _ _ flags -> flags

setObjectIs verb object = case object of
    Object id word direction _ ->
        Object id word direction (flagFor verb)

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
        case List.Extra.find (getObjectId >> ((==) id)) fromContent of
                Just obj -> 
                    let
                        newFromContent = List.filter (getObjectId >> ((/=) id)) fromContent
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


flipDir : Object -> Object
flipDir object = case object of
    Object id word direction flags ->
        Object id word (LinkedGrid.flipDir direction) flags

objectIs : Verb -> Object -> Bool
objectIs verb object = is verb (getObjectFlags object)

cellHas : Verb -> Cell -> Bool
cellHas verb cell = List.any (objectIs verb) cell


asText : Cell -> Maybe Char
asText cell =
    cell
        |> List.map getObjectWord
        |> List.filter Char.isAlpha
        |> List.head

asVerb : Cell -> Maybe Char
asVerb cell =
    cell
        |> List.map getObjectWord
        |> List.filter Char.isUpper
        |> List.head




showIds = False
showAllContents = False


objectDebugChar : Object -> Char
objectDebugChar object =
    if objectIs Move object then
        case getObjectDirection object of
            Up -> '↑'
            Right -> '→'
            Down -> '↓'
            Left -> '←'

    else
        getObjectWord object


cellDebugString : Cell -> String
cellDebugString cell =
    if showIds then
        List.map (getObjectId >> String.fromInt) cell
         |> String.join ""
    else
        let
            toString c = String.fromChar (if c == '·' then '!' else c)
            chars = List.map objectDebugChar cell
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
            if c == '·' then
                ( index, [] :: outRow )

            else
                let newObject = case c of
                        '↑' ->
                            makeDirectedObject index 'm' Up
                                |> setObjectIs Move

                        '→' ->
                            makeDirectedObject index 'm' Right
                                |> setObjectIs Move

                        '↓' ->
                            makeDirectedObject index 'm' Down
                                |> setObjectIs Move

                        '←' ->
                            makeDirectedObject index 'm' Left
                                |> setObjectIs Move

                        'S' ->
                            makeObject index c
                                |> setObjectIs Stop

                        'P' ->
                            makeObject index c
                                |> setObjectIs Push

                        _ ->
                            makeObject index c
                in
                ( index + 1, [newObject] :: outRow )

        makeRow : String -> ( Int, List (List Cell) ) -> ( Int, List (List Cell) )
        makeRow s ( initialIndex, outRows ) =
                s
                    |> String.toList
                    |> List.foldr makeCell ( initialIndex, [] )
                    |> \( index, cells ) -> ( index, cells :: outRows)

    in
        --List.foldr (String.toList >> (List.foldr makeCell) >> Tuple.second) ( 0, [] ) rows
        List.foldr makeRow ( 0, [] ) rows |> Tuple.second
