port module Viz exposing ( main )

import Browser
import Html exposing ( div, h3, pre, text, table, td, tr )
import Html.Attributes exposing ( class )

import Time

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
  , baba: Baba.Model
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

-- not forwarding commands yet
    BabaMsg babaMsg ->
      ( { model | baba = Baba.update babaMsg model.baba }
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
    ( babaModel, babaCmd ) = Baba.init BabaMsg
  in 
    ( { selected = []
      , graph = initialGraph
      , state = InFlight
      , baba = babaModel
      }
    , Cmd.batch
        [ render initialGraph
        , babaCmd
        ]
    )

type Msg
  = GraphReceived
  | BabaMsg Baba.Msg


-- update Baba every half second

subscriptions _ =
  Sub.batch
    [ notifyGraphRendered (\_ -> GraphReceived)
    , Baba.subscription BabaMsg
    ]


tdFromString str = td [] [ pre [] [ text str ] ]

view : Model -> Html.Html Msg
view model =
  div []
    [ h3 [] [ text "What's up with this?" ]
    , table [ class "table" ]
      [ tr []
        (Baba.problemGraphEvo
          |> List.map Baba.gridToStr
          |> List.map tdFromString
        )
      ]
    , h3 [] [ text "Move tests" ]
    , table [ class "table" ]
      [ tr []
        (model.baba
          |> List.map Baba.gridToStr
          |> List.map tdFromString
        )
      ]
    , h3 [] [ text "Rules test" ]
    , table [ class "table" ]
      [ tr []
        [ tdFromString Baba.testGridDebugStr
        , tdFromString Baba.rulesTestResult
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
    , subscriptions = subscriptions
    }
