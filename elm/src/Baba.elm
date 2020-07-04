module Baba exposing ( testGridDebugStr, rulesTestResult, countChars, problemGraphEvo,
  Model, Msg, init, update, subscription, gridToStr )

import Dict exposing ( Dict )
import Random
import Time

import LinkedGrid exposing ( Direction (..) )

type alias Object = Char
type alias Cell = List Object
type alias Location = LinkedGrid.Location Cell

type alias Grid = LinkedGrid.LinkedGrid Cell
type alias Axis = LinkedGrid.Axis Cell

emptyCell : Cell
emptyCell = []


-- char of arbitrary item at the moment
cellDebugString cell = 
  if showAllContents then
    case cell of
      [] -> "·"
      _ -> String.fromList cell

  else
    case cell of
      first :: rest ->
        (String.fromChar (if first == '·' then '!' else first)) ++ " "
      _ -> "· "



addToCellAt offset object axis =
  let
    newContents = object :: (LinkedGrid.axisGetAt offset axis)
  in
    LinkedGrid.axisSet newContents axis
addToCell = addToCellAt 0

addToCellUniqueAt : Int -> Object -> Axis -> Axis
addToCellUniqueAt offset object axis =
  let
    contents = LinkedGrid.axisGetAt offset axis
  in
    if List.member object contents then axis
    else LinkedGrid.axisSet (object :: contents) axis
addToCellUnique = addToCellUniqueAt 0



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

isMove obj = case moveDir obj of
  Nothing -> False
  _ -> True

hasMove = List.any isMove

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
    , "xP→·←PS"
    , "···↓"
    , "···P"
    ]
  )

gridToStr = LinkedGrid.toDebugString cellDebugString

testGridDebugStr = gridToStr testGrid


roll : Random.Generator Int
roll =
  Random.int 1 6

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
      |> List.filter ((/=) '!') -- until we're probably tracking items moving, removing anything else when flipping move objects
      |> (\cell -> LinkedGrid.axisSet cell loc)
      |> LinkedGrid.flipAxis

    dummy = Debug.log "flipCellDirection"
      [LinkedGrid.axisGet loc, LinkedGrid.axisGet result] 
  in
    result

isJust maybe = case maybe of
  Just _ -> True
  _ -> False

{- little bit of thought needed for moving multiple
  some cases:
    multiple things pushing in same direction
      - first will push stuff, rest will move without pushing
    multiple things moving in different directions
      - some might not be able to move, so need to keep track
      - set new contents to be those that were stuck

  Ignore stuck ones in first attempt, i.e. just clear 
-}

moveAndPushNoClearCell : Axis -> Maybe (Axis)
moveAndPushNoClearCell axisWithMoveAtOrigin =
  let
    doPush : Axis -> Maybe Axis 
    doPush adjoiningAxis =
      let
        direction = LinkedGrid.getAxisDirection adjoiningAxis

        addFunc : Object -> Axis -> Axis
        addFunc obj axis =
          if Maybe.withDefault direction (moveDir obj) == direction
            then
              addToCellUnique obj axis
            else
              axis

      in
        LinkedGrid.axisGetAt -1 adjoiningAxis
          |> List.foldr addFunc adjoiningAxis
          |> LinkedGrid.axisForward -1

        -- LOGGING --
        --( x, y ) = LinkedGrid.axisOrigin prevAxis |> LinkedGrid.getLocationCoordinates
        --dummy = Debug.log "followPushes"
        --  [ String.fromChar <| LinkedGrid.axisGet prevAxis
        --  , String.fromInt x
        --  , String.fromInt y
        --  ] 
        -------------

    pushChain : Axis -> Maybe (Axis)
    pushChain prevAxis = 
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

    followPushes : Axis -> Maybe (Axis)
    followPushes prevAxis = pushChain prevAxis |> Maybe.andThen doPush 


-- temporarily clear out non-moves, until probably tracking objects between cells
    tempFilterOutNonMoves axis = 
      axis
        |> LinkedGrid.axisGet
        |> List.filter (moveDir >> isJust) 
        |> (\cell -> LinkedGrid.axisSet cell axis)

  in
    case followPushes axisWithMoveAtOrigin of
      Nothing -> 
        flipCellDirection axisWithMoveAtOrigin
          |> followPushes

          |> Maybe.map tempFilterOutNonMoves


      updatedAxis -> updatedAxis

doMovesAndPushes : Grid -> Grid
doMovesAndPushes initialGrid =
  let
    dummy = Debug.log "char counts" <| countChars initialGrid

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
        moveFoldFunc object innerGrid =
          let
            maybeMovedAxis = case ( LinkedGrid.at x y innerGrid, moveDir object ) of
              ( Just location, Just direction )
                -> moveAndPushNoClearCell (LinkedGrid.makeAxis location direction)
              _ -> Nothing
            
          in
            case maybeMovedAxis of 
              Just updatedAxis -> LinkedGrid.gridFromAxis updatedAxis
              _ -> innerGrid

        updatedGrid : Grid
        updatedGrid = List.foldr moveFoldFunc grid movingObjects

      in
        case LinkedGrid.at x y updatedGrid of
          Just ( location )
            -> LinkedGrid.makeAxis location Up    -- direction doesn't matter
              |> clearCell
              |> LinkedGrid.gridFromAxis
          _ -> updatedGrid
              --if String.length result == String.length str then impl (n + 1) str
              --else "Error! " ++ result ++ " vs. " ++ str

  in
    List.foldr impl initialGrid moveCoords




--maybeAxisToString : Maybe Axis -> String
--maybeAxisToString axis =
--  axis
--    |> Maybe.map LinkedGrid.gridFromAxis
--    |> Maybe.map (LinkedGrid.toDebugString cellDebugString)
--    |> Maybe.withDefault "!" 


surrounding : 
  (el -> el -> el -> acc -> acc)
  -> acc
  -> LinkedGrid.Axis el
  -> acc
surrounding fn a axis =
    case ( LinkedGrid.axisForward -1 axis, LinkedGrid.axisForward 1 axis ) of
      ( Just prevAxis, Just nextAxis )
        -> fn
          (LinkedGrid.axisGet prevAxis)
          (LinkedGrid.axisGet axis)
          (LinkedGrid.axisGet nextAxis)
          (surrounding fn a nextAxis)

      ( Nothing, Just nextAxis )
        -> surrounding fn a nextAxis

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
    surrounding impl [] axis


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

makeRandomGrid : List Char -> Grid
makeRandomGrid chars =
  let
    impl : List Char -> List (List Cell) -> List (List Cell)
    impl innerChars acc = case List.take randomGridSize innerChars of
      [] -> acc
      block -> (List.map (\c -> if c == '_' then [] else [c]) block) :: (impl (List.drop randomGridSize innerChars) acc)
  in
    LinkedGrid.fromLists emptyCell randomGridSize randomGridSize (impl chars [])

-- list of grids to update for now
type alias Model = List Grid

type Msg
  = Update Time.Posix
  | RandomGrid (List Char)

seed = Random.initialSeed 2
generator = 
  Random.list (randomGridSize * randomGridSize) <| Random.uniform '_' (String.toList "____←↑→↓PPS")

countChars grid = 
  let
    addContentsToDict : Object -> Dict Char Int -> Dict Char Int
    addContentsToDict obj dict =
      let
        key = case obj of
          '←' -> 'M'
          '↑' -> 'M'
          '→' -> 'M'
          '↓' -> 'M'
          _ -> obj
        count = Maybe.withDefault 0 (Dict.get key dict)
      in
        Dict.insert key (count + 1) dict

    addLocationsToDict : Location -> Dict Char Int -> Dict Char Int
    addLocationsToDict loc counts =
        List.foldr addContentsToDict counts (LinkedGrid.getContents loc)
  in
    LinkedGrid.foldLocations addLocationsToDict Dict.empty grid
      |> Dict.toList

-- random from seed
testGraph = 
    LinkedGrid.fromLists emptyCell 4 4
      (stringListToCells
        [ ""
        , "·↑"
        , "·↑"
        , "·↑"
        ]
      )


    --Random.step generator seed
    --  |> Tuple.first
    --  |> makeRandomGrid

problemGraphEvo =
  [ testGraph
  , testGraph |> doMovesAndPushes
  , testGraph |> doMovesAndPushes |> doMovesAndPushes
  , testGraph |> doMovesAndPushes |> doMovesAndPushes |> doMovesAndPushes
  ]

init : (Msg -> msg) -> ( Model, Cmd msg )
init msg =
  ( [ testGraph




    --, movesTestGrid
    --, LinkedGrid.fromLists emptyCell 7 7
    --  (stringListToCells
    --    [ ""
    --    , "···↑·↑"
    --    , "·P··↑"
    --    , "·P·↑"
    --    , "P↑"
    --    , "↑"
    --    ]
    --  )
    --, LinkedGrid.fromLists emptyCell 4 4
    --  (stringListToCells
    --    [ "·↓"
    --    , "··←"
    --    ]
    --  )
    ]
  , Cmd.none -- Random.generate (RandomGrid >> msg) generator
  )

update : Msg -> Model -> Model 
update msg model =
  case msg of
    Update _ -> model
      --let
      --  counts = case updated of
      --    first :: _ -> countChars first
      --    _ -> []
      --  dummy = Debug.log "char counts" counts

      --in
      --  List.map doMovesAndPushes model

    RandomGrid chars -> (makeRandomGrid chars) :: model

subscription : (Msg -> msg) -> Sub msg
subscription msg = Time.every 1000 (Update >> msg)

showAllContents = True
randomGridSize = 4


--   ←
--   ↑
--   →
--   ↓
