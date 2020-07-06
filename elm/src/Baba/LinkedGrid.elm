module Baba.LinkedGrid exposing ( LinkedGrid, toDebugString, Location, foldLocations, getLocationCoordinates,
                          make, Direction (..),
                          fromLists, at, getContents, setContents, above, right, below, left,
                          Axis, makeAxis, gridFromAxis, axisOrigin, axisGetAt, axisGet, axisSetAt, axisSet,
                            axisForward, flipAxis, getAxisDirection )

import Array exposing ( Array )

import Debug


type LinkedGrid el = LinkedGrid el Int Int (Array (Array el))

type Location el = Location (LinkedGrid el) Int Int

getDimensions grid = case grid of
  LinkedGrid _ width height _ -> ( width, height )

foldRange : (Int -> a -> a) -> a -> Int -> a
foldRange f initial end = 
  let
    impl n acc =
      if n < 0 then acc
      else impl (n - 1) (f n acc)
  in
    impl (end - 1) initial 

foldLocations : (Location el -> a -> a) -> a -> LinkedGrid el -> a
foldLocations f initial grid =
  let
    ( width, height ) = getDimensions grid 
    rows y acc = 
      let
        rowFunc x = f (Location grid x y)
      in
        foldRange rowFunc acc width

  in
    foldRange rows initial height

toDebugString : (el -> String) -> LinkedGrid el -> String
toDebugString toString grid =
  let
    ( width, _ ) = getDimensions grid 
    f loc acc = 
      let
        suffix = case loc of
          Location _ x _ -> if x == width - 1 then "\n" else ""

        str = toString (getContents loc)
      in
        (str ++ suffix) :: acc
  in
    String.join "" <| foldLocations f [] grid

toDebugStringOld : (el -> String) -> LinkedGrid el -> String
toDebugStringOld toString grid = case grid of
  LinkedGrid _ _ _ arrays ->
    let

      rowString row = 
        Array.map toString row
          |> Array.toList
          |> String.join ""

      rowStrings : List String
      rowStrings = Array.foldr (rowString >> (::)) [] arrays

    in
      String.join "\n" rowStrings

make empty width height =
  LinkedGrid empty width height
    <| Array.repeat height
    <| Array.repeat width empty

fromLists : el -> Int -> Int -> List (List el) -> LinkedGrid el
fromLists empty width height outer =
  let
    setLength : a -> Int -> Array a -> Array a
    setLength default length arr = 
      Array.append (Array.slice 0 length arr) (Array.repeat (length - Array.length arr) default)
  in
    setLength (List.repeat width empty) height (Array.fromList outer)
      |> Array.map (\list -> setLength empty width (Array.fromList list))
      |> LinkedGrid empty width height

      --|> Array.map (\list -> setLength empty width (Array.fromList list))

at x y grid = case grid of
  LinkedGrid _ width height _ ->
    if x < 0 || x >= width || y < 0 || y >= height then
      Nothing
    else
      Just (Location grid x y)

getContents : Location el -> el
getContents loc = case loc of
  Location (LinkedGrid empty _ _ grid) x y -> 
    Array.get y grid
      |> Maybe.andThen (Array.get x)
      |> Maybe.withDefault empty

setContents : Location el -> el -> Location el
setContents loc content = case loc of
  Location (LinkedGrid empty width height grid) x y ->
    let
      replacementRow = 
        Array.get y grid
          |> Maybe.map (Array.set x content)
    in
      Maybe.map (\row -> Array.set y row grid) replacementRow
        |> Maybe.map (\rows -> LinkedGrid empty width height rows)
        |> Maybe.andThen (\newGrid -> at x y newGrid)
        |> Maybe.withDefault loc

above loc = case loc of
  Location grid x y ->
    if y == 0 then Nothing else Just (Location grid x (y - 1))

right loc = case loc of
  Location ((LinkedGrid _ width _ _) as grid) x y ->
    if x == width - 1 then Nothing else Just (Location grid (x + 1) y)

below loc = case loc of
  Location ((LinkedGrid _ _ height _) as grid) x y ->
    if y == height - 1 then Nothing else Just (Location grid x (y + 1))

left loc = case loc of
  Location grid x y ->
    if x == 0 then Nothing else Just (Location grid (x - 1) y)

getLocationCoordinates loc = case loc of
  Location _ x y -> ( x, y )

type Direction = Up | Right | Down | Left
type Axis el = Axis (Location el) Direction

makeAxis location direction = Axis location direction

applyOffset ( x, y ) offset direction = case direction of
  Up -> ( x, y - offset )
  Right -> ( x + offset, y )
  Down -> ( x, y + offset )
  Left -> ( x - offset, y )

axisGetAt offset axis = case axis of
  Axis (Location grid x y) direction -> 
    let
      ( newX, newY ) = applyOffset ( x, y ) offset direction
    in
      getContents (Location grid newX newY)
axisGet = axisGetAt 0

axisSetAt : Int -> el -> Axis el -> Axis el
axisSetAt offset content axis = case axis of
  Axis (Location grid x y) direction ->
    let
      ( newX, newY ) = applyOffset ( x, y ) offset direction
      newGrid = case setContents (Location grid newX newY) content of
        Location g _ _ -> g
    in
      makeAxis (Location newGrid x y) direction
axisSet = axisSetAt 0

axisForward : Int -> Axis el -> Maybe (Axis el)
axisForward offset axis = case axis of
  Axis (Location grid x y) direction ->
    let
      ( width, height ) = getDimensions grid 
      ( newX, newY ) = applyOffset ( x, y ) offset direction
    in
      Maybe.map (\loc -> Axis loc direction) (at newX newY grid)

axisOrigin axis = case axis of
  Axis location _ -> location

gridFromAxis axis = case axis of
  Axis (Location grid _ _) _ -> grid

flipAxis axis = case axis of
  Axis location Up -> Axis location Down
  Axis location Right -> Axis location Left
  Axis location Down -> Axis location Up
  Axis location Left -> Axis location Right

getAxisDirection axis = case axis of
  Axis _ direction -> direction