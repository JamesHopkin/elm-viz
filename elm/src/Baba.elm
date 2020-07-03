module Baba exposing ( testResult, testGridDebugStr, rulesTestResult, modifiedGridResultStrings,
    movesTestGridString, movesTestGridStep1String, movesTestGridStep2String )

import LinkedGrid exposing ( Direction (..) )


type alias Object = Char
type alias Cell = Char
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

type alias Location = LinkedGrid.Location Cell



isText = Char.isAlpha
isVerb = Char.isUpper


testRow1 = ['·', 'a', '=', 'P', '·']
testRow2 = ['b', '=', 'c']
testRow3 = ['·', '·', '<']
testRow4 = ['·', '·', 'd', '→']

testGrid = LinkedGrid.fromLists '·' 5 5 [[], testRow1, testRow2, testRow3, testRow4]

movesTestGrid = LinkedGrid.fromLists '·' 7 7
  (List.map String.toList
    [ ""
    , "···P"
    , "···↑"
    , "xP←·→PS"
    , "···↓"
    , "···P"
    ]
  )

movesTestGridStep1 = doMovesAndPushes movesTestGrid
movesTestGridStep2 = doMovesAndPushes movesTestGridStep1

gridToStr = LinkedGrid.toDebugString String.fromChar

testGridDebugStr = gridToStr testGrid

movesTestGridString = gridToStr movesTestGrid
movesTestGridStep1String = gridToStr movesTestGridStep1
movesTestGridStep2String = gridToStr movesTestGridStep2


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
                -- look what's in the cell
                contents = LinkedGrid.axisGet axis
              in
                -- found a(nother) push Cell, keep going
                if isPush contents then impl axis

                -- found stop, so nothing moves
                else if isStop contents then Nothing

                -- done searching (calling code will do move)
                else Just axis

            -- treat boundary as stop
            _ -> Nothing
      in
        Maybe.map doPush result

  in
    impl axisWithMoveAtOrigin
      |> Maybe.map (LinkedGrid.axisSet '·')

doMovesAndPushes : (LinkedGrid.LinkedGrid Char) -> (LinkedGrid.LinkedGrid Char)
doMovesAndPushes initialGrid =
  let
    -- find moves
    moveCoords : List ( Int, Int, Direction )
    moveCoords =
      let
        addIfMove loc acc =
          let
            contents = LinkedGrid.getContents loc
            direction = moveDir contents

            ( x, y ) = LinkedGrid.getLocationCoordinates loc
          in
            Maybe.map (\dir -> ( x, y, dir ) :: acc) direction
              |> Maybe.withDefault acc
      in
        LinkedGrid.foldLocations addIfMove [] initialGrid

    impl ( x, y, direction ) grid =
      let
        axis = LinkedGrid.at x y grid
          |> Maybe.map (\loc -> LinkedGrid.makeAxis loc direction)
      in
        case Maybe.andThen moveAndPush axis of
          Just updatedAxis -> LinkedGrid.gridFromAxis updatedAxis
          _ -> grid
              --if String.length result == String.length str then impl (n + 1) str
              --else "Error! " ++ result ++ " vs. " ++ str

  in
    List.foldr impl initialGrid moveCoords

movedGrid = doMovesAndPushes testGrid

testAxis direction = Maybe.map
  (\loc -> LinkedGrid.makeAxis loc direction)
  (LinkedGrid.at 1 1 movedGrid)

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

lookForRulesOnAxis : LinkedGrid.Axis Cell -> List Rule
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


fold :
  (loc -> Maybe loc)
  -> (loc -> acc -> acc)
  -> loc
  -> acc -> acc
fold nextFn f loc acc =
  let
    soFar = f loc acc
  in
    case nextFn loc of
      Just next -> fold nextFn f next soFar
      _ -> soFar

lookForRules : LinkedGrid.LinkedGrid Cell -> List Rule
lookForRules grid =
  case LinkedGrid.at 0 0 grid of
    Just origin ->
      let
        rowRules : List Rule
        rowRules = 
          let
            rowFunc : LinkedGrid.Location Cell -> List Rule
            rowFunc loc = lookForRulesOnAxis <| LinkedGrid.makeAxis loc Right

            prependRow : LinkedGrid.Location Cell -> List Rule -> List Rule
            prependRow = rowFunc >> (++)
          in
            fold LinkedGrid.below prependRow origin []


        columnRules : List Rule
        columnRules = 
          let
            columnFunc loc = lookForRulesOnAxis <| LinkedGrid.makeAxis loc Down
          in
            fold LinkedGrid.right (columnFunc >> (++)) origin []

      in
        rowRules ++ columnRules
    _ -> []

rulesTestResult =
    let
      ruleStrings = List.map ruleDebugString (lookForRules testGrid)
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

moveDir c = case c of
  '←' -> Just Left
  '↑' -> Just Up
  '→' -> Just Right
  '↓' -> Just Down
  _ -> Nothing


isMove c = case moveDir c of
  Nothing -> False
  _ -> True

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


testCases = [ "..→...", "..→PPP..", ".→.→...", ".→P.", "..→S", "→PPSP..", "..→S..→→→.." ]
testResult = List.map (\s -> s ++ " -> " ++ (doMoves s)) testCases |> String.join "\n"
