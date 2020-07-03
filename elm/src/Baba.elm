module Baba exposing ( testGridDebugStr, rulesTestResult, modifiedGridResultStrings,
    movesTestGridString, movesTestGridStep1String, movesTestGridStep2String )

import LinkedGrid exposing ( Direction (..) )

type alias Object = Char
type alias Cell = List Object
type alias Location = LinkedGrid.Location Cell

type alias Grid = LinkedGrid.LinkedGrid Cell
type alias Axis = LinkedGrid.Axis Cell

emptyCell : Cell
emptyCell = []

-- char of arbitrary item at the moment
cellDebugString cell = case cell of
  first :: rest -> String.fromChar first
  _ -> "·"

addToCellAt offset object axis =
  let
    newContents = object :: (LinkedGrid.axisGetAt offset axis)
  in
    LinkedGrid.axisSet newContents axis
addToCell = addToCellAt 0

clearCellAt offset = LinkedGrid.axisSetAt offset []
clearCell = clearCellAt 0

tempGetFirst cell = case cell of
  first :: rest -> first
  _ -> '$' -- need to fix if these start coming out

-- may need object IDs
removeFromCell object axis =
  let
    newContents = List.filter ((/=) object) (LinkedGrid.axisGet axis)
  in
    LinkedGrid.axisSet newContents axis

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

moveDir c = case c of
  '←' -> Just Left
  '↑' -> Just Up
  '→' -> Just Right
  '↓' -> Just Down
  _ -> Nothing

flipDir c = case c of
  '←' -> Just '→'
  '↑' -> Just '↓'
  '→' -> Just '←'
  '↓' -> Just '↑'
  _ -> Nothing


hasMove =  
  let
    isMove obj = case moveDir obj of
      Nothing -> False
      _ -> True
  in
    List.any isMove

hasPush cell = List.member 'P' cell
hasStop cell = List.member 'S' cell

asText cell =
  case List.filter Char.isAlpha cell of
    first :: rest -> Just first
    _ -> Nothing

asVerb cell =
  case List.filter Char.isUpper cell of
    first :: rest -> Just first
    _ -> Nothing


testRows =
  [ ""
  , "·a=P·"
  , "b=c"
  , "··<"
  , "··d→"
  ]

stringListToCells rows =
  let
    makeCell c = case c of 
      '·' -> []
      _ -> [c]

    makeRow s =
      s
        |> String.toList
        |> List.map makeCell
  in
    List.map makeRow rows

testGrid : Grid
testGrid = LinkedGrid.fromLists emptyCell 5 5 (stringListToCells testRows)

movesTestGrid = LinkedGrid.fromLists emptyCell 7 7
  (stringListToCells
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

gridToStr = LinkedGrid.toDebugString cellDebugString

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

flipCellDirection : Axis -> Axis
flipCellDirection loc =
  let
    result = loc
      |> LinkedGrid.axisGet
      |> List.map (flipDir >> (Maybe.withDefault '!')) -- default should never be used
      |> (\cell -> LinkedGrid.axisSet cell loc)
      |> LinkedGrid.flipAxis

    --dummy = Debug.log "flipCellDirection"
    --  [LinkedGrid.axisGet loc, LinkedGrid.axisGet result] 
  in
    result

moveAndPush : Axis -> Maybe (Axis)
moveAndPush axisWithMoveAtOrigin =
  let
    doPush : Axis -> Maybe Axis 
    doPush adjoiningAxis =
      LinkedGrid.axisSet (LinkedGrid.axisGetAt -1 adjoiningAxis) adjoiningAxis
        |> LinkedGrid.axisForward -1

    followPushes : Axis -> Maybe (Axis)
    followPushes prevAxis =
      let
        -- LOGGING --
        --( x, y ) = LinkedGrid.axisOrigin prevAxis |> LinkedGrid.getLocationCoordinates
        --dummy = Debug.log "followPushes"
        --  [ String.fromChar <| LinkedGrid.axisGet prevAxis
        --  , String.fromInt x
        --  , String.fromInt y
        --  ] 
        -------------

        pushChain : Maybe (Axis)
        pushChain = 
          case LinkedGrid.axisForward 1 prevAxis of
            Just axis ->
              let
                -- look what's in the cell
                contents = LinkedGrid.axisGet axis
              in
                -- found a(nother) push Cell, keep going
                if hasPush contents then followPushes axis

                -- found stop, so nothing moves
                else if hasStop contents then Nothing

                -- done searching (calling code will do move)
                else Just axis

            -- treat boundary as stop
            _ -> Nothing
      in
        Maybe.andThen doPush pushChain

    result = case followPushes axisWithMoveAtOrigin of
      Nothing -> followPushes (flipCellDirection axisWithMoveAtOrigin)
      updatedAxis -> updatedAxis
  in
    -- next step: need to do above for all moves in cell
    Maybe.map clearCell result

doMovesAndPushes : Grid -> Grid
doMovesAndPushes initialGrid =
  let
    -- find moves
    moveCoords : List ( Int, Int, Direction )
    moveCoords =
      let
        addIfMove : Location -> List ( Int, Int, Direction ) -> List ( Int, Int, Direction )
        addIfMove loc acc =
          let
            contents = LinkedGrid.getContents loc

            -- TEMP - until I process each item in the cell
            direction = case contents of
              c :: rest -> moveDir c
              _ -> Nothing

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




maybeAxisToString : Maybe Axis -> String
maybeAxisToString axis =
  axis
    |> Maybe.map LinkedGrid.gridFromAxis
    |> Maybe.map (LinkedGrid.toDebugString cellDebugString)
    |> Maybe.withDefault "!" 

axisTestResults = 
  [ Maybe.map (addToCell '!') (testAxis Down)
  , Maybe.map (addToCellAt 2 '!') (testAxis Down)
  , Maybe.map (addToCell '!') (testAxis Right)
  , Maybe.map (addToCellAt 2 '!') (testAxis Right)
  , Maybe.map (addToCellAt 1 '!') (testAxis Left)
  , Maybe.map (addToCellAt 1 '!') (testAxis Up)

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
      ( Just prevAxis, Just nextAxis )
        -> fn
          (LinkedGrid.axisGet prevAxis)
          (LinkedGrid.axisGet axis)
          (LinkedGrid.axisGet nextAxis)
          (surroundingUsingAxis fn a nextAxis)

      ( Nothing, Just nextAxis )
        -> surroundingUsingAxis fn a nextAxis

      _ -> a

lookForRulesOnAxis : Axis -> List Rule
lookForRulesOnAxis axis = 
  let
    impl : Cell -> Cell -> Cell -> List Rule -> List Rule
    impl x y z a =

--       if not (isText x && not (isVerb x) && isText z) then a
      --if not (isText x) || isVerb x || not (isText z) then a


      case ( asText x, asVerb x, asText z ) of
        ( Nothing, _, _ ) -> a
        ( _, Just _, _ )  -> a
        ( _, _, Nothing ) -> a
        _ ->
          let
            firstX = tempGetFirst x
            firstZ = tempGetFirst z
          in
            case ( tempGetFirst y, asVerb z ) of
              ( '=', Just zVerb ) -> Is      firstX (verbFromOccupant zVerb) :: a
              ( '=', Nothing )    -> Becomes firstX firstZ                   :: a
              ( '<', Nothing )    -> Has     firstX firstZ                   :: a
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

lookForRules : Grid -> List Rule
lookForRules grid =
  case LinkedGrid.at 0 0 grid of
    Just origin ->
      let
        rowRules : List Rule
        rowRules = 
          let
            rowFunc : Location -> List Rule
            rowFunc loc = lookForRulesOnAxis <| LinkedGrid.makeAxis loc Right

            prependRow : Location -> List Rule -> List Rule
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
