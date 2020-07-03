port module Viz exposing ( main )

import Browser
import Html exposing ( div, h3, pre, text, table, td, tr )
import Html.Attributes exposing ( class )

import Json.Encode as Encode
import Json.Decode as Decode

import Graph
import Dict exposing ( Dict )

import Baba


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
  ( { selected = []
    , graph = initialGraph
    , state = InFlight
    }
  , render initialGraph
  )

type Msg
  = GraphReceived


view : Model -> Html.Html Msg
view model =
  div []
    [ h3 [] [ text "Baba 1D test" ]
    , pre [] [ text Baba.testResult ]
    , h3 [] [ text "Rules test" ]
    , table [ class "table" ]
      [ tr []
        [ td []
          [ pre [] [ text Baba.testGridDebugStr ] ]
        , td []
          [ pre [] [ text Baba.rulesTestResult ] ]
        ]
      , tr []
          <| List.map (\s -> td [] [ pre [] [ text s ] ]) Baba.modifiedGridResultStrings
      , tr []
        [ td [] [ pre [] [ text Baba.movesTestGridString ] ]
        , td [] [ pre [] [ text Baba.movesTestGridStep1String ] ]
        , td [] [ pre [] [ text Baba.movesTestGridStep2String ] ]
        ]
      ]
    , text <| "Graph state: " ++ (
      case model.state of
        Idle -> "idle"
        InFlight -> "in flight"
        Pending -> "pending"
      )
    ]

main : Program Flags Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = \model -> notifyGraphRendered (\_ -> GraphReceived)
    }
