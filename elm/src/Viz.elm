port module Viz exposing ( main )

import Browser
import Html exposing ( div, h3, pre, text, table, td, tr, textarea )
import Html.Attributes exposing ( class, style, attribute )
import Html.Events exposing ( onInput )

import Time

import Json.Encode as Encode
import Json.Decode as Decode

import Graph
import Dict exposing ( Dict )

import Baba.Baba
import Baba.BabaTest

-- how do I pipe this through?
import Baba.Graphics

type alias RenderData =
  { id: Int
  , data: String -- outgoing dot or incoming svg
  }

renderDataDecoder : Decode.Decoder RenderData
renderDataDecoder = 
  Decode.map2 RenderData
    (Decode.field "id"  Decode.int)
    (Decode.field "data" Decode.string)

encodeRenderData : RenderData -> Encode.Value
encodeRenderData data = 
  Encode.object
    [ ("id", Encode.int data.id)
    , ("data", Encode.string data.data)
    ]

dcp = Graph.defaultCommonProperties
dnp = Graph.defaultNodeProperties
dep = Graph.defaultEdgeProperties


render : Graph.Graph -> Cmd Msg
render graph = renderDot (encodeRenderData { id = 0, data = Graph.toDot graph })




--------------------------------------------

port renderDot : Encode.Value -> Cmd msg
port notifyGraphRendered : (() -> msg) -> Sub msg

type GraphState = Idle | InFlight | Pending

type alias Model =
  { selected: List String
  , graph: Graph.Graph
  , state: GraphState
  , baba: Baba.Baba.Model
  , babaTest: Baba.BabaTest.Model
  }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
  case msg of
    GraphReceived ->
      case model.state of
        Pending ->
          ( { model | state = InFlight }, render model.graph )
        _ ->
          ( { model | state = Idle }, Cmd.none )

    BabaMsg babaMsg ->
      ( { model | baba = Baba.Baba.update babaMsg model.baba }
        , Cmd.none
      )

-- not forwarding commands yet
    BabaTestMsg testMsg ->
      ( { model | babaTest = Baba.BabaTest.update testMsg model.babaTest }
        , Cmd.none
      )

    BabaInput gridStr ->
      ( { model | baba = Baba.Baba.replaceGrid gridStr model.baba }
        , Cmd.none
      )

type alias Flags = Int --Decode.Value

initialGraph = Graph.makeGraph
  [ Graph.Node "Baba" dnp
  , Graph.Node "IsYou"
      { dnp
      | common =
        { dcp
        | label = "is you!"
        }
      }
  ]
  [ ( "Baba", "IsYou", dep )
  ]



init : Flags -> ( Model, Cmd Msg )
init _ =
  let
    ( babaModel, babaCmd ) = Baba.Baba.init BabaMsg
    ( babaTestModel, babaTestCmd ) = Baba.BabaTest.init BabaTestMsg
  in 
    ( { selected = []
      , graph = initialGraph
      , state = InFlight
      , baba = babaModel
      , babaTest = babaTestModel
      }
    , Cmd.batch
        [ render initialGraph
        , babaCmd
        , babaTestCmd
        ]
    )

type Msg
  = GraphReceived
  | BabaMsg Baba.Baba.Msg
  | BabaTestMsg Baba.BabaTest.Msg
  | BabaInput String


-- update Baba every half second

subscriptions _ =
  Sub.batch
    [ notifyGraphRendered (\_ -> GraphReceived)
    , Baba.Baba.subscription BabaMsg
    , Baba.BabaTest.subscription BabaTestMsg
    ]


tdFromString str = td [] [ pre [] [ text str ] ]


view : Model -> Html.Html Msg
view model =
  div []
    [ Baba.Graphics.view (Baba.Baba.GraphicsMsg >> BabaMsg) model.baba.graphics
    , div [ class "editor" ]
      [ textarea
        [ Html.Attributes.rows 20, Html.Attributes.cols 20, onInput BabaInput ]
        []
      ]


    , pre [] [ text model.baba.debugStr ]
    , table [ class "table" ]
      [ tr [] [td [] [ text "a - Zelda"], td [] [ text "f - key"],     td [] [text "K - sink"], td [] [ text "R - weak"],   td [] [ text "W - win"]]
      , tr [] [td [] [ text "b - water"], td [] [ text "g - statue"],  td [] [text "L - pull"], td [] [ text "S - stop"],   td [] [ text "X - you"]]
      , tr [] [td [] [ text "c - rock"],  td [] [ text "h - Link"],    td [] [text "M - move"], td [] [ text "T - defeat"], td [] [ text "Y - text"]]
      , tr [] [td [] [ text "d - shrub"], td [] [],                    td [] [text "O - hot"],  td [] [ text "U - open"],   td [] [ text "Z - melt"]]
      , tr [] [td [] [ text "e - fence"], td [] [],                    td [] [text "P - push"], td [] [ text "V - shut"]]
      ]
    , div [] (List.map text Baba.BabaTest.testResults)
    ]
    
main : Program Flags Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
