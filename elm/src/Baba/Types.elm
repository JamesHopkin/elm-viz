module Baba.Types exposing (..)

import Bitwise

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
    | Stop
    | Defeat
    | Win
    | Open
    | Closed
    | Float_
    | You

verbDebugString v = case v of
    Push -> "push"
    Move -> "move"
    Stop -> "stop"
    Defeat -> "defeat"
    Win -> "win"
    Open -> "open"
    Closed -> "closed"
    Float_ -> "float"
    _ -> "you"

--------------
-- verb flags

is : Verb -> Int -> Bool
is v n = Bitwise.and (flagFor v) n /= 0

flagFor : Verb -> Int
flagFor v = case v of
    Push    -> 0x0001
    Move    -> 0x0002
    Stop    -> 0x0004
    Defeat  -> 0x0008
    Win     -> 0x0010
    Open    -> 0x0020
    Closed  -> 0x0040
    Float_  -> 0x0080
    _       -> 0x0100
