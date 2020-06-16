port module Viz exposing (main)

import Browser
import Html exposing (div, text)

import Json.Encode as Encode

import Graph

port renderGraph : String -> Cmd msg

type alias Msg = Int

type alias Model =
  { woo: Int
  , foo: String
  }

dotGraph = """digraph G {
  From -> Elm;
}
"""

update : Msg -> Model -> (Model, Cmd Msg)
update _ model = (model, Cmd.none)

subscription : Model -> Sub Msg
subscription _ = Sub.none

type alias Flags = Int --Decode.Value


init : Flags -> (Model, Cmd Msg)
init n = ({ woo = n, foo = "foo" }, renderGraph dotGraph)

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
