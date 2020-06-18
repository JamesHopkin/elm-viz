port module Viz exposing (main)

import Browser
import Html exposing (div, text)

import Json.Encode as Encode
import Json.Decode as Decode

import Graph


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

port renderDot : Encode.Value -> Cmd msg
port receiveGraph : String -> Cmd msg

type alias Msg = Int

type alias Model =
  { woo: Int
  , foo: String
  }

dotGraph = """digraph G {
  node [shape=box];
  From [href="javascript:linkTest('woo!')"];

  From -> Elm [style=dashed, arrowhead=none];
  From -> another;
}
"""

update : Msg -> Model -> (Model, Cmd Msg)
update _ model = (model, Cmd.none)

subscription : Model -> Sub Msg
subscription _ = Sub.none

type alias Flags = Int --Decode.Value


init : Flags -> (Model, Cmd Msg)
init n = ({ woo = n, foo = "foo" }, renderDot <| encodeRenderData { id = 0, data = dotGraph })

view : Model -> Html.Html Msg
view model =
  let s = "hello, elm made me! here's a number: " ++ String.fromInt model.woo
  in div [] [ text s ]

main : Program Flags Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscription
    }
