port module Viz exposing (main)

import Browser
import Html exposing (div, text)

import Json.Encode as Encode

port testPort : Encode.Value -> Cmd msg

type alias Msg = Int

type alias Model =
  { woo: Int
  , foo: String
  }


update : Msg -> Model -> (Model, Cmd Msg)
update _ model = (model, Cmd.none)

subscription : Model -> Sub Msg
subscription _ = Sub.none

type alias Flags = Int --Decode.Value


init : Flags -> (Model, Cmd Msg)
init n = ({ woo = n, foo = "foo" }, Cmd.none)

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
