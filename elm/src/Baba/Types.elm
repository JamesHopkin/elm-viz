module Baba.Types exposing (..)

import Bitwise
import Dict as Dict
import List.Extra

type Noun = Noun Char

nounsEqual lhs rhs = case ( lhs, rhs ) of
    ( Noun lc, Noun rc ) ->
        lc == rc

type Conjunction
    = And
    | Not

type LinkingWord
    = Is
    | Has
    | Makes

type Restrictive
    = On
    | Near


-- predicate not really the right word, but these are things that can go
-- on either side of Is
type Predicate
    = All
    | Empty
    | Text

-- verb also doesn"t apply to all, but it"s close
type Stative
    = Push
    | Pull
    | Move
    | More
    | Stop
    | Defeat
    | Win
    | Open
    | Shut  
    | Float_
    | Hot
    | Melt
    | Sink
    | Shift
    | Weak
    | Tele
    | You

type Subject
    = Predicate Predicate
    | NounSubject Noun

subjectDebugString subject = case subject of
    Predicate predicate -> textDebugString (PredicateText predicate)
    NounSubject noun -> textDebugString (NounText noun)

type Complement
    = Stative Stative
    | PredicateComplement Predicate
    | NounComplement Noun

complementDebugString complement = case complement of
    Stative stative -> textDebugString (StativeText stative)
    PredicateComplement predicate -> textDebugString (PredicateText predicate)
    NounComplement noun -> textDebugString (NounText noun)

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

textDebugString text = (getTextInfo text).word

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

--------------
-- Stative flags

is : Stative -> Int -> Bool
is v n = Bitwise.and (flagFor v) n /= 0

isAny : List Stative -> Int -> Bool
isAny v n = Bitwise.and (flagsFor v) n /= 0

flagFor : Stative -> Int
flagFor v = case v of
    Push    -> 0x000001
    Pull    -> 0x000002
    Move    -> 0x000004
    More    -> 0x000008
    Stop    -> 0x000010
    Defeat  -> 0x000020
    Win     -> 0x000040
    Open    -> 0x000080
    Shut    -> 0x000100
    Float_  -> 0x000200
    Hot     -> 0x000400
    Melt    -> 0x000800
    Sink    -> 0x001000
    Shift   -> 0x002000
    Weak    -> 0x004000
    Tele    -> 0x008000
    You     -> 0x010000

flagsFor : List Stative -> Int
flagsFor verbs = List.foldr (flagFor >> Bitwise.or) 0 verbs


getTextInfo text = {char = '<', word = "has", glyph = { x = 0, y = 0, width = 18, height = 16}}

    --case text of
    --LinkingWord Has -> {char = '<', word = "has", glyph = { x = 0, y = 0, width = 18, height = 16}}
    --LinkingWord Is -> {char = '=', word = "is", glyph = { x = 26, y = 0, width = 9, height = 16}}
    --NounText (Noun 'a') -> {char = 'A', word ="Zelda", glyph = { x = 65, y = 96, width = 29, height = 16}}

textByCode = Dict.fromList
    [
    ( '<', LinkingWord Has ),
    ( 'A', NounText (Noun 'a') )
    ]

getTextInfoByCode c =
    case Dict.get c textByCode of

        Just text ->
            Just ( text, getTextInfo text )

        _ ->
            Nothing
