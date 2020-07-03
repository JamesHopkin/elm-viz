module Baba exposing ( testResult, testGridDebugStr, rulesTestResult, modifiedGridResultStrings )

import LinkedGrid exposing ( Direction (..) )


type alias Object = Char
type alias Tile = Char
emptySquare = ' '

type Rule
  = Is Char Verb
  | Becomes Char Char
  | Has Char Char

ruleDebugString rule = case rule of
  Is c v -> String.join " " [String.fromChar c, "is", verbDebugString v]
  Becomes l r -> String.join " " [String.fromChar l, "is", String.fromChar r]
  Has l r -> String.join " " [String.fromChar l, "has", String.fromChar r]

type Conjuction
  = And
  | Not

type Verb
  = Push
  | Move
  | Defeat
  | Win
  | Open
  | Closed
  | Float_
  | You

verbDebugString v = case v of
  Push -> "push"
  Move -> "move"
  Defeat -> "defeat"
  Win -> "win"
  Open -> "open"
  Closed -> "closed"
  Float_ -> "float"
  _ -> "you"


verbFromOccupant s = case s of
  'P' -> Push
  'M' -> Move
  'D' -> Defeat
  'W' -> Win
  'O' -> Open
  'C' -> Closed
  'F' -> Float_
  _ -> You

type alias Location = LinkedGrid.Location Tile

surrounding : 
  (Location -> Maybe Location)
  -> (Location -> Maybe Location)
  -> (Tile -> Tile -> Tile -> acc -> acc)
  -> acc
  -> Location
  -> acc
surrounding getPrev getNext fn a location =
  let
    impl = surrounding getPrev getNext fn a
  in
    case (getPrev location, getNext location) of
      ( Just prevLoc, Just nextLoc ) -> fn
        (LinkedGrid.getContents prevLoc)
        (LinkedGrid.getContents location)
        (LinkedGrid.getContents nextLoc)
        (impl nextLoc)

      ( Nothing, Just nextLoc ) -> impl nextLoc

      _ -> a

isText = Char.isAlpha
isVerb = Char.isUpper

lookForRulesInRow :
  (Location -> Maybe Location)
  -> (Location -> Maybe Location)
  -> Location
  -> List Rule
lookForRulesInRow prev next row = 
  let
    impl x y z a =
      if not (isText x && not (isVerb x) && isText z) then a
      else 
        case y of
          '=' ->
            if isVerb z then  Is x (verbFromOccupant z) :: a
            else              Becomes x z :: a
          '<' ->
            if isVerb z then  a
            else              Has x z :: a
          _ -> a
  in
    surrounding prev next impl [] row


testRow1 = ['·', 'a', '=', 'P', '·']
testRow2 = ['b', '=', 'c']
testRow3 = ['·', '·', '<']
testRow4 = ['·', '·', 'd']

fold :
  (Location -> Maybe Location)
  -> (Location -> acc -> acc)
  -> Location
  -> acc -> acc
fold nextFn f loc acc =
  let
    soFar = f loc acc
  in
    case nextFn loc of
      Just next -> fold nextFn f next soFar
      _ -> soFar

testGrid = LinkedGrid.fromLists '·' 5 5 [[], testRow1, testRow2, testRow3, testRow4]

testGridDebugStr = LinkedGrid.toDebugString String.fromChar testGrid

lookForRules : LinkedGrid.LinkedGrid Tile -> List Rule
lookForRules grid =
  case LinkedGrid.at 0 0 grid of
    Just origin ->
      let
        rowRules : List Rule
        rowRules = fold LinkedGrid.below
          (lookForRulesInRow LinkedGrid.left LinkedGrid.right >> (++))
          origin []

        columnRules : List Rule
        columnRules = fold LinkedGrid.right
          (lookForRulesInRow LinkedGrid.above LinkedGrid.below >> (++))
          origin []

      in
        rowRules ++ columnRules
    _ -> []


--rulesTestResult =
--    let
--      ruleStrings = List.map ruleDebugString (lookForRules testGrid)
--    in 
--      String.join "\n" ruleStrings


-----------
-- Axis tests

moveAndPush : LinkedGrid.Axis Char -> Maybe (LinkedGrid.Axis Char)
moveAndPush axisWithMoveAtOrigin =
  let
    doPush : LinkedGrid.Axis Char -> LinkedGrid.Axis Char 
    doPush adjoiningAxis =
      LinkedGrid.axisSet (LinkedGrid.axisGetAt -1 adjoiningAxis) adjoiningAxis
        |> LinkedGrid.axisForward -1
        |> Maybe.withDefault axisWithMoveAtOrigin -- should never use this default

    impl : LinkedGrid.Axis Char -> Maybe (LinkedGrid.Axis Char)
    impl prevAxis =
      let
        result : Maybe (LinkedGrid.Axis Char)
        result = 
          case LinkedGrid.axisForward 1 prevAxis of
            Just axis ->
              let
                contents = LinkedGrid.axisGet axis
              in
                if isPush contents then impl axis
                else if isStop contents then Nothing
                else Just axis
            _ -> Nothing -- treat boundary as stop
      in
        Maybe.map doPush result

  in
    impl axisWithMoveAtOrigin
      |> Maybe.map (LinkedGrid.axisSet '.')

--doMovesAndPushes : (LinkedGrid Char) -> (LinkedGrid Char)
--doMovesAndPushes state =
--  let
--    -- find moves
--    moveLocations : List ( Int, Int )
--    moveLocations =
--      String.toList state
--        |> List.indexedMap (\n -> \el -> ( n, el ))
--        |> List.filter (\(_, el) -> isMove el)
--        |> List.map Tuple.first 

--    impl n str =
--      case move (Axis str n) of
--        Just (Axis modified _) -> modified
--        _ -> str
--              --if String.length result == String.length str then impl (n + 1) str
--              --else "Error! " ++ result ++ " vs. " ++ str

--  in
--    List.foldr impl state moveIndices

testAxis direction = Maybe.map
  (\loc -> LinkedGrid.makeAxis loc direction)
  (LinkedGrid.at 1 1 testGrid)

maybeAxisToString axis = 
  axis
    |> Maybe.map LinkedGrid.gridFromAxis
    |> Maybe.map (LinkedGrid.toDebugString String.fromChar)
    |> Maybe.withDefault "!" 

axisTestResults = 
  [ Maybe.map (LinkedGrid.axisSet '!') (testAxis Down)
  , Maybe.map (LinkedGrid.axisSetAt 2 '!') (testAxis Down)
  , Maybe.map (LinkedGrid.axisSet '!') (testAxis Right)
  , Maybe.map (LinkedGrid.axisSetAt 2 '!') (testAxis Right)
  , Maybe.map (LinkedGrid.axisSetAt 1 '!') (testAxis Left)
  , Maybe.map (LinkedGrid.axisSetAt 1 '!') (testAxis Up)

  --, testAxis
  --    |> Maybe.map 
  ]

modifiedGridResultStrings = List.map maybeAxisToString axisTestResults


surroundingUsingAxis : 
  (el -> el -> el -> acc -> acc)
  -> acc
  -> LinkedGrid.Axis el
  -> acc
surroundingUsingAxis fn a axis =
    case ( LinkedGrid.axisForward -1 axis, LinkedGrid.axisForward 1 axis ) of
      ( Just prevAxis, Just nextAxis ) -> fn
        (LinkedGrid.axisGet prevAxis)
        (LinkedGrid.axisGet axis)
        (LinkedGrid.axisGet nextAxis)
        (surroundingUsingAxis fn a nextAxis)

      ( Nothing, Just nextAxis ) -> surroundingUsingAxis fn a nextAxis

      _ -> a

lookForRulesOnAxis : LinkedGrid.Axis Tile -> List Rule
lookForRulesOnAxis axis = 
  let
    impl x y z a =
      if not (isText x && not (isVerb x) && isText z) then a
      else 
        case y of
          '=' ->
            if isVerb z then  Is x (verbFromOccupant z) :: a
            else              Becomes x z :: a
          '<' ->
            if isVerb z then  a
            else              Has x z :: a
          _ -> a
  in
    surroundingUsingAxis impl [] axis


--fold2 :
--  (LinkedGrid.Location Tile -> Maybe (LinkedGrid.Location Tile))
--  -> (LinkedGrid.Location Tile -> (List Rule) -> (List Rule))
--  -> LinkedGrid.Location Tile
--  -> (List Rule) -> (List Rule)
--fold2 nextFn f loc acc =
--  let
--    soFar = f loc acc
--  in
--    case nextFn loc of
--      Just next -> fold2 nextFn f next soFar
--      _ -> soFar

fold2 :
  (loc -> Maybe loc)
  -> (loc -> acc -> acc)
  -> loc
  -> acc -> acc
fold2 nextFn f loc acc =
  let
    soFar = f loc acc
  in
    case nextFn loc of
      Just next -> fold2 nextFn f next soFar
      _ -> soFar

lookForRules2 : LinkedGrid.LinkedGrid Tile -> List Rule
lookForRules2 grid =
  case LinkedGrid.at 0 0 grid of
    Just origin ->
      let
        rowRules : List Rule
        rowRules = 
          let
            rowFunc : LinkedGrid.Location Tile -> List Rule
            rowFunc loc = lookForRulesOnAxis <| LinkedGrid.makeAxis loc Right

            prependRow : LinkedGrid.Location Tile -> List Rule -> List Rule
            prependRow = rowFunc >> (++)
          in
            fold2 LinkedGrid.below prependRow origin []


        columnRules : List Rule
        columnRules = 
          let
            columnFunc loc = lookForRulesOnAxis <| LinkedGrid.makeAxis loc Down
          in
            fold2 LinkedGrid.right (columnFunc >> (++)) origin []

      in
        rowRules ++ columnRules
    _ -> []

rulesTestResult =
    let
      ruleStrings = List.map ruleDebugString (lookForRules2 testGrid)
    in 
      String.join "\n" ruleStrings

-----------
-- 1D!


type Axis = Axis String Int

axisGetAt m axis = case axis of
  Axis s n ->
    s |> String.dropLeft (n + m)
      |> String.uncons
      |> Maybe.withDefault ( '!', "" )
      |> \( c, _ ) -> c
axisGet = axisGetAt 0

axisSetAt m char axis = case axis of
  Axis s n ->
    let
      index = n + m
      left = String.left index s
      right = String.cons char <| String.dropLeft (index + 1) s
    in
      Axis (left ++ right) n
axisSet = axisSetAt 0

axisForward m axis = case axis of
  Axis s n -> Axis s (n + m)

axisOriginalString axis = case axis of
  Axis s _ -> s

isMove c = c == 'M'
isPush c = c == 'P'
isStop c = c == 'S'

move axisWithMoveAtOrigin =
  let
    doPush adjoiningAxis =
      axisSet (axisGetAt -1 adjoiningAxis) adjoiningAxis
        |> axisForward -1

    impl prevAxis =
      let
        axis = axisForward 1 prevAxis
        contents = axisGet axis
        result = 
          if isPush contents then impl axis
            else if isStop contents then Nothing
            else Just axis
      in
        Maybe.map doPush result

  in
    impl axisWithMoveAtOrigin
      |> Maybe.map (axisSet '.')

doMoves : String -> String
doMoves state =
  let
    -- find moves
    moveIndices : List Int
    moveIndices =
      String.toList state
        |> List.indexedMap (\n -> \el -> ( n, el ))
        |> List.filter (\(_, el) -> isMove el)
        |> List.map Tuple.first 

    impl n str =
      case move (Axis str n) of
        Just (Axis modified _) -> modified
        _ -> str
              --if String.length result == String.length str then impl (n + 1) str
              --else "Error! " ++ result ++ " vs. " ++ str

  in
    List.foldr impl state moveIndices


testCases = [ "..M...", "..MPPP..", ".M.M...", ".MP.", "..MS", "MPPSP..", "..MS..MMM.." ]
testResult = List.map (\s -> s ++ " -> " ++ (doMoves s)) testCases |> String.join "\n"
