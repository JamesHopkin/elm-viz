module Baba.BabaTest exposing ( Model, Msg, init, update, subscription,
                                problemGraphEvo, gridToStr,
                                rulesTestGridDebugStr, rulesTestResult )

import Baba.Cell exposing (..)
import Baba.Move exposing (..)
import Baba.Rules as Rules exposing (..)
import Baba.Types exposing (..)

import Baba.LinkedGrid as LinkedGrid

import Random
import Time


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


rulesTestResult =
    let
      ruleStrings = List.map ruleDebugString (lookForRules rulesTestGrid)
    in 
      String.join "\n" ruleStrings


makeRandomGrid : List Char -> Grid
makeRandomGrid chars =
  let
    impl : List Char -> List String -> List String
    impl innerChars acc = case List.take randomGridSize innerChars of
      [] -> acc
      block -> (String.fromList block) :: (impl (List.drop randomGridSize innerChars) acc)
  in
    LinkedGrid.fromLists emptyCell randomGridSize randomGridSize (stringListToCells (impl chars []))


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

testRows =
  [ ""
  , "·a=P·"
  , "b=c"
  , "··<"
  , "··d→"
  ]


rulesTestGridDebugStr = gridToStr rulesTestGrid


roll : Random.Generator Int
roll =
  Random.int 1 6



-- list of grids to update for now
type alias Model = List Grid


type Msg
  = Update Time.Posix
  | RandomGrid (List Char)

seed = Random.initialSeed 4
generator = 
  Random.list (randomGridSize * randomGridSize) <| Random.uniform '·' (String.toList "························←↑→↓PPPPSS")


update : Msg -> Model -> Model 
update msg model =
  case msg of
    Update _
      -> List.map doMovesAndPushes model

    RandomGrid chars
      -> (makeRandomGrid chars) :: model

subscription : (Msg -> msg) -> Sub msg
subscription msg = Time.every 300 (Update >> msg)

randomGridSize = 20

