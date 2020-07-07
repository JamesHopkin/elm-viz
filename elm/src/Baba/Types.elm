module Baba.Types exposing (..)

import Bitwise

type Noun = Noun Char

type Conjunction
    = And
    | Not

conjunctionDebugString conjunction = case conjunction of
    And -> "and"
    Not -> "not"

type LinkingWord
    = Is
    | Has
    | Makes

-- predicate not really the right word, but these are things that can go
-- on either side of Is
type Predicate
    = All
    | Empty

predicateDebugString predicate = case predicate of
    All -> "all"
    Empty -> "empty"

-- verb also doesn't apply to all, but it's close
type Stative
    = Push
    | Pull
    | Move
    | More
    | Stop
    | Defeat
    | Win
    | Open
    | Closed
    | Float_
    | You

    -- not added to strings or flags yet (generate?)
    | Hot
    | Melt
    | Sink
    | Weak

type Subject
    = Predicate Predicate
    | NounSubject Noun

subjectDebugString subject = case subject of
    Predicate predicate -> predicateDebugString predicate
    NounSubject noun -> nounDebugString noun

type Complement
    = Stative Stative
    | PredicateComplement Predicate
    | NounComplement Noun

complementDebugString complement = case complement of
    Stative stative -> stativeDebugString stative
    PredicateComplement predicate -> predicateDebugString predicate
    NounComplement noun -> nounDebugString noun

type Text
    = Conjunction Conjunction
    | StativeText Stative
    | PredicateText Predicate
    | NounText Noun

-- use all lower case chars internally, but show (and parse) nouns as upper case
-- (instances lower case)
nounDebugString noun = case noun of
    Noun c -> String.fromChar (Char.toUpper c)

stativeDebugString stative = case stative of
    Push -> "push"
    Pull -> "pull"
    Move -> "move"
    More -> "more"
    Stop -> "stop"
    Defeat -> "defeat"
    Win -> "win"
    Open -> "open"
    Closed -> "closed"
    Float_ -> "float"
    _ -> "you"

textDebugString text = case text of
    Conjunction conjunction -> conjunctionDebugString conjunction
    StativeText stative -> stativeDebugString stative
    PredicateText predicate -> predicateDebugString predicate
    NounText noun -> nounDebugString noun

--------------
-- Stative flags

is : Stative -> Int -> Bool
is v n = Bitwise.and (flagFor v) n /= 0

isAny : List Stative -> Int -> Bool
isAny v n = Bitwise.and (flagsFor v) n /= 0

flagFor : Stative -> Int
flagFor v = case v of
    Push    -> 0x0001
    Pull    -> 0x0002
    Move    -> 0x0004
    More    -> 0x0008
    Stop    -> 0x0010
    Defeat  -> 0x0020
    Win     -> 0x0040
    Open    -> 0x0080
    Closed  -> 0x0100
    Float_  -> 0x0200
    _       -> 0x0400

flagsFor : List Stative -> Int
flagsFor verbs = List.foldr (flagFor >> Bitwise.or) 0 verbs