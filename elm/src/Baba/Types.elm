module Baba.Types exposing (..)

import Bitwise

type Noun = Noun Char

nounsEqual lhs rhs = case ( lhs, rhs ) of
    ( Noun lc, Noun rc ) ->
        lc == rc

-- use all lower case chars internally, but show (and parse) nouns as upper case
-- (instances lower case)
nounDebugString noun = case noun of
    Noun c -> String.fromChar (Char.toUpper c)

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

type Restrictive
    = On
    | Near

linkingWordDebugString linkingWord = case linkingWord of
    Is  -> "is"
    Has -> "has"
    Makes -> "makes"

restrictiveDebugString restrictive = case restrictive of
    On -> "on"
    Near -> "near"

-- predicate not really the right word, but these are things that can go
-- on either side of Is
type Predicate
    = All
    | Empty
    | Text

predicateDebugString predicate = case predicate of
    All -> "all"
    Empty -> "empty"
    Text -> "text"

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
    | Hot
    | Melt
    | Sink
    | Weak
    | You

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
    Hot -> "hot"
    Melt -> "melt"
    Sink -> "sink"
    Weak -> "weak"
    _ -> "you"

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

complementAsSubject complement = case complement of
    PredicateComplement predicate -> Just (Predicate predicate)
    NounComplement noun -> Just (NounSubject noun)
    _ -> Nothing

type Text
    = Conjunction Conjunction
    | LinkingWord LinkingWord
    | Restrictive Restrictive
    | PredicateText Predicate
    | StativeText Stative
    | NounText Noun

textDebugString text = case text of
    Conjunction conjunction -> conjunctionDebugString conjunction
    LinkingWord linkingWord -> linkingWordDebugString linkingWord
    Restrictive restrictive -> restrictiveDebugString restrictive
    StativeText stative -> stativeDebugString stative
    PredicateText predicate -> predicateDebugString predicate
    NounText noun -> nounDebugString noun

textAsLinkingWord text = case text of
    LinkingWord word -> Just word
    _ -> Nothing

textAsSubject text = case text of
    PredicateText predicate -> Just (Predicate predicate)
    NounText noun -> Just (NounSubject noun)
    _ -> Nothing

textAsComplement text = case text of
    StativeText stative -> Just (Stative stative)
    PredicateText predicate -> Just (PredicateComplement predicate)
    NounText noun -> Just (NounComplement noun)
    _ -> Nothing

type alias Restriction =
    { word : Restrictive
    , noun : Subject
    , sense : Bool
    }

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
    Hot     -> 0x0400
    Melt    -> 0x0800
    Sink    -> 0x1000
    Weak    -> 0x2000
    _       -> 0x4000


flagsFor : List Stative -> Int
flagsFor verbs = List.foldr (flagFor >> Bitwise.or) 0 verbs