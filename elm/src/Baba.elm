module Baba exposing ( rulesTestGridDebugStr, rulesTestResult, countChars, problemGraphEvo,
  Model, Msg, init, update, subscription, gridToStr )

import Dict exposing ( Dict )
import Random
import Time

import List.Extra

import LinkedGrid exposing ( Direction (..) )

type alias Object = ( Int, Char )
type alias Cell = List Object
type alias Location = LinkedGrid.Location Cell

type alias Grid = LinkedGrid.LinkedGrid Cell
type alias Axis = LinkedGrid.Axis Cell

emptyCell : Cell
emptyCell = []


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
  
    --dummy0 = Debug.log "move from content" fromContent
    maybeObj = List.Extra.find (Tuple.first >> ((==) id)) fromContent
    --dummy = Debug.log "move ids" [id, from, to, if isJust maybeObj then 1 else 0]

    result =
       case maybeObj of
          Just obj -> 
            let
              newFromContent = List.filter (Tuple.first >> ((/=) id)) fromContent
              newToContent = obj :: LinkedGrid.axisGetAt to axis
              --dummy2 = Debug.log "move" [fromContent, newToContent]
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


-- say each object is (id, char), move from one cell to another:
    -- should be easy!



clearCellAt offset = LinkedGrid.axisSetAt offset []
clearCell = clearCellAt 0

tempGetFirst cell = case cell of
  first :: rest -> first
  _ -> ( -1, '$' ) -- need to fix if these start coming out

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


testRows =
  [ ""
  , "·a=P·"
  , "b=c"
  , "··<"
  , "··d→"
  ]

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

rulesTestGrid : Grid
rulesTestGrid = LinkedGrid.fromLists emptyCell 5 5 (stringListToCells testRows)

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

rulesTestGridDebugStr = gridToStr rulesTestGrid


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
      |> List.map (\obj -> flipDir obj |> Maybe.withDefault obj)
      |> (\cell -> LinkedGrid.axisSet cell loc)
      |> LinkedGrid.flipAxis

    --dummy = Debug.log "flipCellDirection"
    --  [LinkedGrid.axisGet loc, LinkedGrid.axisGet result] 
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

moveAndPush : Int -> Axis -> Maybe (Axis)
moveAndPush objectId axisWithMoveAtOrigin =
  let
    doPush : Axis -> Maybe Axis 
    doPush adjoiningAxis =
      let
        moveFunc : Object -> Axis -> Axis
        moveFunc ((id, _) as obj) axis =
          let
            dummy = Nothing -- Debug.log "push" [id, if isPush obj then 1 else 0]
          in
            if isPush obj
              then
                Maybe.withDefault axis (moveToCell id -1 0 axis)
                --addToCellUnique obj axis
              else
                axis

        updatedAxis = LinkedGrid.axisGetAt -1 adjoiningAxis
                        |> List.foldr moveFunc adjoiningAxis
        result = LinkedGrid.axisForward -1 updatedAxis

        ( x, y ) = LinkedGrid.axisOrigin updatedAxis |> LinkedGrid.getLocationCoordinates
        --dummy4 = Debug.log "doPush result"
        --  [ if isJust result then 1 else 0, x, y ]
      in
        result

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
              let
                result = pushChain axis True

                --dummy2 = Debug.log "follow pushes (after)"
                --  ( case result of 
                --    Just newAxis -> 
                --      [ LinkedGrid.axisOrigin newAxis |> LinkedGrid.getLocationCoordinates |> (\( x, y ) -> String.fromInt x ++ ", " ++ String.fromInt y)
                --      , LinkedGrid.axisGetAt -1 newAxis |> cellDebugString
                --      , LinkedGrid.axisGetAt 0 newAxis |> cellDebugString
                --      , LinkedGrid.axisGetAt 1 newAxis |> cellDebugString
                --      ]
                --    _ -> ["failed"]
                --  )
                --dummy1 = Debug.log "follow pushes (before)"
                --  [ LinkedGrid.axisOrigin axis |> LinkedGrid.getLocationCoordinates |> (\( x, y ) -> String.fromInt x ++ ", " ++ String.fromInt y)
                --  , LinkedGrid.axisGetAt -1 axis |> cellDebugString
                --  , LinkedGrid.axisGetAt 0 axis |> cellDebugString
                --  , LinkedGrid.axisGetAt 1 axis |> cellDebugString
                --  ]
              in
                Maybe.andThen (if shouldPush then doPush else LinkedGrid.axisForward -1) result

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

    --dummy4 = Debug.log "original axis"
    --        [ LinkedGrid.axisOrigin axisWithMoveAtOrigin |> LinkedGrid.getLocationCoordinates |> (\( x, y ) -> String.fromInt x ++ ", " ++ String.fromInt y)
    --        ]

    --dummy3 = Debug.log "push result"
    --    ( case pushResult of 
    --        Just newAxis -> 
    --          [ LinkedGrid.axisOrigin newAxis |> LinkedGrid.getLocationCoordinates |> (\( x, y ) -> String.fromInt x ++ ", " ++ String.fromInt y)
    --          , LinkedGrid.axisGetAt 0 newAxis |> cellDebugString
    --          , LinkedGrid.axisGetAt 1 newAxis |> cellDebugString
    --          , LinkedGrid.axisGetAt 2 newAxis |> cellDebugString
    --          ]
    --        _ -> ["failed"]
    --    )
  in
    -- finally move the move object itself
    pushResult |> Maybe.andThen (moveToCell objectId 0 1)

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
        moveFoldFunc (( id, c ) as object) innerGrid =
          let
            -- get location and direction, will never use Nothing case
            maybeUpdatedAxis = case ( LinkedGrid.at x y innerGrid, moveDir object ) of
              ( Just location, Just direction )
                -> moveAndPush id (LinkedGrid.makeAxis location direction)
              _ -> Nothing
            --dummy2 = Debug.log "doMovesAndPushes" [isJust maybeUpdatedAxis]
          in
            case maybeUpdatedAxis of 
              Just updatedAxis -> LinkedGrid.gridFromAxis updatedAxis
              _ -> innerGrid

      in
        List.foldr moveFoldFunc grid movingObjects

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

      case ( asText x, asVerb x, asText z ) of
        ( Nothing, _, _ ) -> a
        ( _, Just _, _ )  -> a
        ( _, _, Nothing ) -> a
        _ ->
          let
            ( _, firstX ) = tempGetFirst x
            ( _, firstZ ) = tempGetFirst z
          in
            case ( tempGetFirst y, asVerb z ) of
              ( ( _, '=' ), Just zVerb ) -> Is      firstX (verbFromOccupant zVerb) :: a
              ( ( _, '='), Nothing )     -> Becomes firstX firstZ                   :: a
              ( ( _, '<'), Nothing )     -> Has     firstX firstZ                   :: a
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
      ruleStrings = List.map ruleDebugString (lookForRules rulesTestGrid)
    in 
      String.join "\n" ruleStrings


-- use stringListToCells!
makeRandomGrid : List Char -> Grid
makeRandomGrid chars =
  let
    impl : List Char -> List String -> List String
    impl innerChars acc = case List.take randomGridSize innerChars of
      [] -> acc
      block -> (String.fromList block) :: (impl (List.drop randomGridSize innerChars) acc)
  in
    LinkedGrid.fromLists emptyCell randomGridSize randomGridSize (stringListToCells (impl chars []))

-- list of grids to update for now
type alias Model = List Grid

type Msg
  = Update Time.Posix
  | RandomGrid (List Char)

seed = Random.initialSeed 4
generator = 
  Random.list (randomGridSize * randomGridSize) <| Random.uniform '·' (String.toList "························←↑→↓PPPPSS")

countChars grid = 
  let
    addContentsToDict : Object -> Dict Char Int -> Dict Char Int
    addContentsToDict ( id, obj ) dict =
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
testGrid = 
    --LinkedGrid.fromLists emptyCell 4 4
    --  (stringListToCells
    --    [ "→PP→" ]
    --  )

    --LinkedGrid.fromLists emptyCell 4 4
    --  [ [ []
    --    , [ ( 0, '→' ), ( 1, '→' ) ]
    --    , [ ( 2, 'P') ]
    --    ]
    --  ]

    Random.step generator seed
      |> Tuple.first
      |> makeRandomGrid

problemGraphEvo =
  [ testGrid
  , testGrid |> doMovesAndPushes
  , testGrid |> doMovesAndPushes |> doMovesAndPushes
  , testGrid |> doMovesAndPushes |> doMovesAndPushes |> doMovesAndPushes
  ]

init : (Msg -> msg) -> ( Model, Cmd msg )
init msg =
  ( [ testGrid




    , movesTestGrid
    , LinkedGrid.fromLists emptyCell 7 7
      (stringListToCells
        [ ""
        , "···↑·↑"
        , "·P··↑"
        , "·P·↑"
        , "P↑"
        , "↑"
        ]
      )
    , LinkedGrid.fromLists emptyCell 4 4
      (stringListToCells
        [ "·↓"
        , "··←"
        ]
      )
    ]
  , Random.generate (RandomGrid >> msg) generator
  )

update : Msg -> Model -> Model 
update msg model =
  case msg of
    Update _ -> List.map doMovesAndPushes model
      --let
      --  counts = case model of
      --    first :: _ -> countChars first
      --    _ -> []
      --  dummy = Debug.log "char counts" counts

      --in
      --  List.map doMovesAndPushes model

    RandomGrid chars -> (makeRandomGrid chars) :: model

subscription : (Msg -> msg) -> Sub msg
subscription msg = Time.every 300 (Update >> msg)

showAllContents = False
showIds = False
randomGridSize = 20


--   ←
--   ↑
--   →
--   ↓
