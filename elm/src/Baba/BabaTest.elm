module Baba.BabaTest exposing ( Model, Msg, init, update, subscription,
                                problemGraphEvo, gridToStr,
                                rulesTestGridDebugStr, rulesTestResult )

import Baba.Baba as Baba exposing ( countChars )
import Baba.Cell exposing (..)
import Baba.Move exposing (..)
import Baba.Rules as Rules exposing (..)
import Baba.Types as Types

import Baba.LinkedGrid as LinkedGrid

import Random
import Time


ruleGridToOverlay = LinkedGrid.fromLists emptyCell 5 5
          (stringListToCells
            [ "A=M"
            , " B=P"
            , "  C"
            , "  ="
            , "  L"
            ]
          )


-- random from seed
testGrid = 
        --LinkedGrid.fromLists emptyCell 4 4
        --  (stringListToCells
        --    [ "→P"
        --    , "m=M"
        --    ]
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
        |> (\grid -> LinkedGrid.overlay grid 0 0 ruleGridToOverlay)

    --    withMove = random
    --        |> LinkedGrid.at 3 3
    --        |> Maybe.map (\loc -> LinkedGrid.setContents loc [
    --            makeTextObject 10000 (Types.NounText (Types.Noun 'm'))
    --                |> setObjectIs Types.Push
    --            ])
    --        |> Maybe.map LinkedGrid.gridFromLocation
    --        |> Maybe.andThen (LinkedGrid.at 4 3)
    --        |> Maybe.map (\loc -> LinkedGrid.setContents loc [
    --            makeTextObject 10001 (Types.LinkingWord Types.Is)
    --                |> setObjectIs Types.Push
    --            ])
    --        |> Maybe.map LinkedGrid.gridFromLocation
    --        |> Maybe.andThen (LinkedGrid.at 5 3)
    --        |> Maybe.map (\loc -> LinkedGrid.setContents loc [
    --            makeTextObject 10002 (Types.StativeText Types.Move)
    --                |> setObjectIs Types.Push
    --            ])
    --        |> Maybe.map LinkedGrid.gridFromLocation

    --in
    --Maybe.withDefault random withMove

shouldMove obj =
    if objectIs Types.Move obj then
        Just (getObjectDirection obj)

    else
        Nothing

move = doMovesAndPushes shouldMove True >> Tuple.second

problemGraphEvo =
    [ testGrid
    , testGrid |> move
    , testGrid |> move |> move
    , testGrid |> move |> move |> move
    ]

init : (Msg -> msg) -> ( Model, Cmd msg )
init msg =
    ( [ testGrid




        --, movesTestGrid
        --, LinkedGrid.fromLists emptyCell 7 7
        --    (stringListToCells
        --        [ ""
        --        , "···↑·↑"
        --        , "·P··↑"
        --        , "·P·↑"
        --        , "P↑"
        --        , "↑"
        --        ]
        --    )
        --, LinkedGrid.fromLists emptyCell 4 4
        --    (stringListToCells
        --        [ "·↓"
        --        , "··←"
        --        ]
        --    )
        ]
    , Random.generate (RandomGrid >> msg) generator
    )


rulesTestResult =
        let
            ruleStrings = List.map Rules.ruleDebugString (lookForRules rulesTestGrid)
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

gridToStr : Grid -> String
gridToStr = LinkedGrid.toDebugString cellDebugString

testRows =
    [ ""
    , "·A=X·"
    , "B=C"
    , "··<"
    , "··D→"
    ]


-- initial test
--   X = push


rulesTestGridDebugStr = gridToStr rulesTestGrid


roll : Random.Generator Int
roll =
    Random.int 1 6



-- list of grids to update for now
type alias Model = List Grid


type Msg
    = Update Time.Posix
    | RandomGrid (List Char)

seed = Random.initialSeed 1
generator = 
    Random.list (randomGridSize * randomGridSize) <| Random.uniform ' ' (String.toList "                    aaaabbbbccd")


update : Msg -> Model -> Model 
update msg model =
    case msg of
        Update _ ->
                let
                    dummy = Debug.log "object counts" <| case List.head model of
                        Just grid -> countChars grid
                        _ -> []
                in
                List.map Baba.wait model

        RandomGrid chars ->
                (makeRandomGrid chars
                    |> (\grid -> LinkedGrid.overlay grid 0 0 ruleGridToOverlay)
                    ) :: model

subscription : (Msg -> msg) -> Sub msg
subscription msg = Time.every 300 (Update >> msg)

randomGridSize = 12


