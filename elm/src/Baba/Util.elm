module Baba.Util exposing (..)

isJust maybe = case maybe of
    Just _ -> True
    _ -> False

isNothing maybe = case maybe of
    Nothing -> True
    _ -> False

ensure default ls maybeResult =
    case maybeResult of
        Just result ->
            result

        _ ->
            let
                dummy = Debug.log "ensure" ls
            in
            default
