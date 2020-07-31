module Baba.Util exposing (..)

isJust maybe = case maybe of
    Just _ -> True
    _ -> False

isNothing maybe = case maybe of
    Nothing -> True
    _ -> False

ensure : a -> List b -> Maybe a -> a
ensure default ls maybeResult =
    case maybeResult of
        Just result ->
            result

        _ ->
            let
                dummy = Debug.log "ensure" ls
            in
            default

curry2 f ( a, b ) = f a b
curry3 f ( a, b, c ) = f a b c
