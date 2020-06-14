port module Viz exposing (main)

import Browser
import Html exposing (..)

import Json.Decode as Decode
import Json.Encode as Encode

port testPort : Encode.Value -> Cmd msg

type alias Msg = Int

type alias Model =
  { woo: Int
  }




update : Msg -> Model -> (Model, Cmd Msg)
update msg model = (model, Cmd.none)

subscription : Model -> Sub Msg
subscription model = Sub.none

type alias Flags = Int --Decode.Value


init : Flags -> (Model, Cmd Msg)
init n = ({ woo = n }, Cmd.none)


--(\k -> \v -> \b -> text k :: b)
view : Model -> Html.Html Msg
view model =
  let s = "hello, elm made me! here's a number: " ++ (String.fromInt model.woo)
  in div [] [ text s ]

main : Program Flags Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscription
    }

