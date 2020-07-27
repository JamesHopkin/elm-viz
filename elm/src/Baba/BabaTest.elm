module Baba.BabaTest exposing ( Model, Msg, init, update, subscription,
                                problemGraphEvo, gridToStr,
                                rulesTestGridDebugStr, rulesTestResult, testResults )

import List.Extra

import Baba.Baba as Baba exposing ( countChars )
import Baba.Cell exposing (..)
import Baba.Move exposing (..)
import Baba.Rules as Rules exposing (..)
import Baba.Types as Types
import Baba.Util exposing (..)

import Baba.LinkedGrid as LinkedGrid exposing ( Direction(..) )

import Random
import Time


allTests = 
        -- pull multiple
    [ ( [ [ "B=LC=LA=M",  "B=LC=LA=M" ]
        , [ "@@@→",       " @@@→" ]
        ], Nothing )

        -- open/closed
    , ( [ [ "A=MA=UB=V", "A=MA=UB=V" ]
        , [ "→b",        "" ]
        ], Nothing )

        -- hot/melt
    , ( [ [ "A=MA=OB=Z", "A=MA=OB=Z" ]
        , [ "→b",        " →" ]
        ], Nothing )

        -- melt and defeat
    , ( [ [ "D=YA=M  ", "D=YA=M" ]
        , [ "A=Z BC  ", "A=Z BC" ]
        , [ " d@←==  ", "  @ ==" ]
        , [ "  →T  O←", "   ←TO→" ]
        ], Just Right )

        -- weak
    , ( [ [ "B=YC=R", "B=YC=R" ]
        , [ "bc", " b"]
        ], Just Right )

    , ( [ [ "B=C", "B=C" ]
        , [ "bbb", "ccc" ]
        ], Nothing )

    , ( [ [ "A=M",    "A=M" ]
        , [ " → ",    "  →" ]
        ], Nothing )
    , ( [ [ "A=M ",    "A=M" ]
        , [ " →→ ",    "  →→" ]
        ], Nothing )

    , ( [ [ "A=M ",    "A=M" ]
        , [ "  ←←",    " ←←" ]
        ], Nothing )

    , ( [ [ "B=KA=M",  "B=KA=M" ]
        , [ "b←    ",  "" ]
        ], Nothing )

    , ( [ [ "B=LA=M",  "B=LA=M" ]
        , [ "  ←bb",    " ←bb" ]
        ], Nothing )

    , ( [ [ "B=PA=M",  "B=PA=M" ]
        , [ " b←",    "b←" ]
        ], Nothing )

    , ( [ [ "B=SA=M",  "B=SA=M" ]
        , [ " b←",    " b →" ]
        ], Nothing )

    , ( [ [ "BA ",    "BA " ]
        , [ "==b",    "==b" ]
        , [ "SM↑",    "SM " ]
        , [ "",       "  ↓"]
        ], Nothing )

    , ( [ [" →e  ED", "← e  ED" ]
        , ["dC=L↓==", " C=L ==" ]
        , ["↑ b←bSK", "cb← ↓SK" ]
        , ["c B=PF",  "  B=bF" ]
        , ["A=M  =",  "A=M P=" ]
        , [" f   Y",  "  f  Y" ]
        ], Just Right )

    ]

tests = allTests -- [Maybe.withDefault [] (List.head allTests)]

testGrids : List ( List Grid, Maybe Direction )
testGrids = 
    let
        makeGrids : ( List (List String), Maybe Direction ) -> Maybe ( List Grid, Maybe Direction )
        makeGrids ( lists, direction ) = case List.head lists of
            Just first ->
                case List.head first of
                    Just firstString ->
                        let
                            makeGrid : List String -> Grid
                            makeGrid = 
                                LinkedGrid.fromLists emptyCell (String.length firstString) (List.length lists)
                                    << stringListToCells
                        in
                        Just ( List.map makeGrid (List.Extra.transpose lists), direction )
                    _ ->
                        Nothing
            _ ->
                Nothing
    in
    List.filterMap makeGrids tests

doTest : List Grid -> Maybe Direction -> ( Int, List ( Int, Int ) )
doTest grids direction = 
    case grids of
        a :: b :: _
            -> ( 0, mismatch (Maybe.withDefault a (Baba.turn True direction a)) b )

        _
            -> ( -1, [] )

testResults = 
    List.map
        (\tup ->
            let
                mismatches = Tuple.second (curry2 doTest tup)
                toStr ( x, y ) = String.fromInt x ++ ":" ++ String.fromInt y
            in
            case mismatches of
                [] ->
                    "✓"

                _ ->
                    --let
                    --    dummy = Debug.log "grid" (g |> List.head |> Maybe.andThen (\grid -> Baba.turn Nothing grid) |> Maybe.map gridToStr |> Maybe.withDefault "") 
                    --in
                    String.join ", " (List.map toStr mismatches)

        ) testGrids

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


rulesTestResult = case testGrids of 
    g :: [] ->
        curry2 doTest g
            |> Tuple.second
            |> List.map (\( x, y ) -> String.fromInt x ++ ":" ++ String.fromInt y)
            |> String.join ", "

    _ ->
        "no grids!"
        --let
        --    ruleStrings = List.map Rules.ruleDebugString (lookForRules rulesTestGrid)
        --in 
        --    String.join "\n" ruleStrings


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
        Update _ -> model
                --let
                --    --dummy = Debug.log "object counts" <| case List.head model of
                --        --Just grid -> countChars grid
                --        --_ -> []

                --    withPretendUndoStack grid =
                --        Baba.turn Nothing grid
                --in
                --List.filterMap withPretendUndoStack model


        RandomGrid chars ->
                (makeRandomGrid chars
                    |> (\grid -> LinkedGrid.overlay grid 0 0 ruleGridToOverlay)
                    ) :: model

subscription : (Msg -> msg) -> Sub msg
subscription msg = Time.every 300 (Update >> msg)

randomGridSize = 12


