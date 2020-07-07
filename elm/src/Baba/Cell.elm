module Baba.Cell exposing ( Object, Cell, Location, Grid, Axis, emptyCell, moveToCell,
                            objectIs, objectIsAny, cellHas, cellHasAny, asText, asStative,
                            getObjectId, getObjectWord, getObjectDirection, makeObject, makeDirectedObject,
                            setObjectDirection, setObjectIs, 
                            flipDir,
                            cellDebugString, stringListToCells, stativeFromOccupant )

import List.Extra

import Baba.Types as Types
import Baba.LinkedGrid as LinkedGrid exposing ( Direction (..) )

type alias Cell = List Object
type alias Location = LinkedGrid.Location Cell

type alias Grid = LinkedGrid.LinkedGrid Cell
type alias Axis = LinkedGrid.Axis Cell


type ObjectKind
    = Instance Types.Noun
    | Text Types.Text



type alias ObjectState = 
    { word: ObjectKind
    , direction: Direction
    , flags: Int
    , lastMovedTick: Int
    }



type Object = Object Int ObjectState

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
    Object id
        { word = Instance (Types.Noun c)
        , direction = dir
        , flags = 0
        , lastMovedTick = -1
        }

makeDirectedObject id c direction =
    Object id
        { word = Instance (Types.Noun c)
        , direction = direction
        , flags = 0
        , lastMovedTick = -1
        }


getObjectId object = case object of
    Object id _ -> id

getObjectWord object = case object of
    Object _ state ->
        case state.word of
            Instance (Types.Noun c) -> c
            _ -> '!' -- to do! (callers of getObjectWord need to use ObjectKind)

getObjectDirection object = case object of
    Object _ state -> state.direction

setObjectDirection direction object = case object of
    Object id state ->
        Object id 
            { state
            | direction = direction
            }

getObjectFlags object = case object of
    Object id state -> state.flags

setObjectIs verb object = case object of
    Object id state ->
        Object id 
            { state
            | flags = Types.flagFor verb
            }

addToCellAt offset object axis =
    let
        newContents = object :: (LinkedGrid.axisGetAt offset axis)
    in
        LinkedGrid.axisSet newContents axis
addToCell = addToCellAt 0

isJust m = case m of
    Just _ -> True
    _ -> False

moveToCell : Int -> Int -> Int -> Maybe Direction -> Axis -> Maybe Axis
moveToCell id from to maybeDirection axis =
    let
        fromContent = LinkedGrid.axisGetAt from axis

        --dummy0 = Debug.log "move from content" fromContent
        maybeObj = List.Extra.find (getObjectId >> ((==) id)) fromContent
        --dummy = Debug.log "move ids" [id, from, to, if isJust maybeObj then 1 else 0]

    
        result = case maybeObj of
                Just obj -> 
                    let
                        objToAdd = case maybeDirection of
                            Just newDirection -> setObjectDirection newDirection obj
                            _ -> obj

                        newFromContent = List.filter (getObjectId >> ((/=) id)) fromContent
                        newToContent = objToAdd :: LinkedGrid.axisGetAt to axis
                    in
                        Just
                            ( axis
    -- could optimise to not replace grid twice
                                    |> LinkedGrid.axisSetAt from newFromContent
                                    |> LinkedGrid.axisSetAt to newToContent
                            )

                _ -> Nothing

        --dummy3 = Debug.log "move (after)"
        --  ( case result of 
        --    Just newAxis -> 
        --      [ LinkedGrid.axisOrigin newAxis |> LinkedGrid.getLocationCoordinates |> (\( x, y ) -> String.fromInt x ++ ", " ++ String.fromInt y)
        --      , LinkedGrid.axisGetAt -1 newAxis |> cellDebugString
        --      , LinkedGrid.axisGetAt 0 newAxis |> cellDebugString
        --      , LinkedGrid.axisGetAt 1 newAxis |> cellDebugString
        --      ]
        --    _ -> ["failed"]
        --  )
    in
    result

stativeFromOccupant c = case c of
    'P' -> Types.Push
    'M' -> Types.Move
    'D' -> Types.Defeat
    'W' -> Types.Win
    'O' -> Types.Open
    'C' -> Types.Closed
    'F' -> Types.Float_
    _ -> Types.You


flipDir : Object -> Object
flipDir object = case object of
    Object id state ->
        Object id 
            { state
            | direction = LinkedGrid.flipDir state.direction
            }

objectIs : Types.Stative -> Object -> Bool
objectIs stative object = Types.is stative (getObjectFlags object)

objectIsAny : List Types.Stative -> Object -> Bool
objectIsAny statives object = Types.isAny statives (getObjectFlags object)

cellHas : Types.Stative -> Cell -> Bool
cellHas stative cell = List.any (objectIs stative) cell

cellHasAny : List Types.Stative -> Cell -> Bool
cellHasAny statives cell = List.any (objectIsAny statives) cell


-- wip
asText : Cell -> Maybe Char
asText cell =
    cell
        |> List.map getObjectWord
        |> List.filter Char.isAlpha
        |> List.head

asStative : Cell -> Maybe Char
asStative cell =
    cell
        |> List.map getObjectWord
        |> List.filter Char.isUpper
        |> List.head




showIds = False
showAllContents = False
showDirections = True

objectDebugChar : Object -> Char
objectDebugChar object =
    if objectIs Types.Move object then
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
    else if showDirections then 
        Maybe.withDefault "· " (Maybe.map
            (\obj -> String.fromChar (getObjectWord obj) ++ (case getObjectDirection obj of
                Up -> "↑"
                Right -> "→"
                Down -> "↓"
                Left -> "←"
            ))
            (List.head cell)
            )

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
                                |> setObjectIs Types.Move

                        '→' ->
                            makeDirectedObject index 'm' Right
                                |> setObjectIs Types.Move

                        '↓' ->
                            makeDirectedObject index 'm' Down
                                |> setObjectIs Types.Move

                        '←' ->
                            makeDirectedObject index 'm' Left
                                |> setObjectIs Types.Move

                        'S' ->
                            makeObject index c
                                |> setObjectIs Types.Stop

                        'P' ->
                            makeObject index c
                                |> setObjectIs Types.Push

                        'L' ->
                            makeObject index c
                                |> setObjectIs Types.Pull

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
