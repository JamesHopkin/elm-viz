module Baba.Types exposing (..)

type Rule
    = Is Char Verb
    | Becomes Char Char
    | Has Char Char

ruleDebugString rule = case rule of
    Is c v -> String.join " " [String.fromChar c, "is", verbDebugString v]
    Becomes l r -> String.join " " [String.fromChar l, "is", String.fromChar r]
    Has l r -> String.join " " [String.fromChar l, "has", String.fromChar r]

type Conjuction
    = And
    | Not

type Verb
    = Push
    | Move
    | Defeat
    | Win
    | Open
    | Closed
    | Float_
    | You

verbDebugString v = case v of
    Push -> "push"
    Move -> "move"
    Defeat -> "defeat"
    Win -> "win"
    Open -> "open"
    Closed -> "closed"
    Float_ -> "float"
    _ -> "you"

