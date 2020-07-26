module Baba.Util exposing (..)

isJust maybe = case maybe of
    Just _ -> True
    _ -> False

isNothing maybe = case maybe of
    Nothing -> True
    _ -> False
