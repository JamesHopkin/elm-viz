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


getTextInfo text = case text of

    LinkingWord Has -> {char = '<',word ="has", glyph = { x = 0, y = 0, width = 18, height = 16}}
    LinkingWord Is -> {char = '=',word ="is", glyph = { x = 26, y = 0, width = 9, height = 16}}
    Conjunction And -> {char = '&',word ="and", glyph = { x = 43, y = 0, width = 18, height = 16}}
    Conjunction Not -> {char = '!',word ="not", glyph = { x = 69, y = 0, width = 18, height = 16}}
    Restrictive On -> {char = '_',word ="on", glyph = { x = 95, y = 0, width = 12, height = 16}}
    Restrictive Near -> {char = '/',word ="near", glyph = { x = 115, y = 0, width = 23, height = 16}}
    PredicateText All -> {char = '^',word ="all", glyph = { x = 146, y = 0, width = 16, height = 16}}
    LinkingWord Makes -> {char = '+',word ="makes", glyph = { x = 170, y = 0, width = 31, height = 16}}
    PredicateText Empty -> {char = '0',word ="empty", glyph = { x = 209, y = 0, width = 32, height = 16}}
    StativeText Tele -> {char = '1',word ="Tele", glyph = { x = 0, y = 32, width = 24, height = 16}}
    StativeText More -> {char = '2',word ="More", glyph = { x = 32, y = 32, width = 24, height = 16}}
    StativeText Sink -> {char = 'K',word ="Sink", glyph = { x = 64, y = 32, width = 21, height = 16}}
    StativeText Pull -> {char = 'L',word ="Pull", glyph = { x = 93, y = 32, width = 22, height = 16}}
    StativeText Move -> {char = 'M',word ="Move", glyph = { x = 123, y = 32, width = 26, height = 16}}
    StativeText Float_ -> {char = 'N',word ="Float", glyph = { x = 157, y = 32, width = 29, height = 16}}
    StativeText Hot -> {char = 'O',word ="Hot", glyph = { x = 194, y = 32, width = 18, height = 16}}
    StativeText Push -> {char = 'P',word ="Push", glyph = { x = 220, y = 32, width = 24, height = 16}}
    StativeText Shift -> {char = 'Q',word ="Shift", glyph = { x = 0, y = 64, width = 27, height = 16}}
    StativeText Weak -> {char = 'R',word ="Weak", glyph = { x = 35, y = 64, width = 25, height = 16}}
    StativeText Stop -> {char = 'S',word ="Stop", glyph = { x = 68, y = 64, width = 24, height = 16}}
    StativeText Defeat -> {char = 'T',word ="Defeat", glyph = { x = 100, y = 64, width = 36, height = 16}}
    StativeText Open -> {char = 'U',word ="Open", glyph = { x = 144, y = 64, width = 24, height = 16}}
    StativeText Shut -> {char = 'V',word ="Shut", glyph = { x = 176, y = 64, width = 24, height = 16}}
    StativeText Win -> {char = 'W',word ="Win", glyph = { x = 208, y = 64, width = 16, height = 16}}
    StativeText You -> {char = 'Y',word ="You", glyph = { x = 232, y = 64, width = 19, height = 16}}
    PredicateText Text -> {char = 'X',word ="text", glyph = { x = 0, y = 96, width = 25, height = 16}}
    StativeText Melt -> {char = 'Z',word ="Melt", glyph = { x = 33, y = 96, width = 24, height = 16}}
    NounText(Noun 'a') -> {char = 'A',word ="Zelda", glyph = { x = 65, y = 96, width = 29, height = 16}}
    NounText(Noun 'b') -> {char = 'B',word ="Water", glyph = { x = 102, y = 96, width = 30, height = 16}}
    NounText(Noun 'c') -> {char = 'C',word ="Rock", glyph = { x = 140, y = 96, width = 24, height = 16}}
    NounText(Noun 'd') -> {char = 'D',word ="Shrub", glyph = { x = 172, y = 96, width = 29, height = 16}}
    NounText(Noun 'e') -> {char = 'E',word ="Fence", glyph = { x = 209, y = 96, width = 30, height = 16}}
    NounText(Noun 'f') -> {char = 'F',word ="Key", glyph = { x = 0, y = 128, width = 19, height = 16}}
    NounText(Noun 'g') -> {char = 'G',word ="Statue", glyph = { x = 27, y = 128, width = 36, height = 16}}
    NounText(Noun 'h') -> {char = 'H',word ="Sign", glyph = { x = 71, y = 128, width = 21, height = 16}}
    NounText(Noun 'i') -> {char = 'I',word ="Link", glyph = { x = 100, y = 128, width = 21, height = 16}}
    NounText(Noun c) -> {char = Char.toUpper c, word ="other", glyph = { x = 0, y = 0, width = 16, height = 16}}

textByCode =
    Dict.fromList
    [
    ('<', LinkingWord Has),
    ('=', LinkingWord Is),
    ('&', Conjunction And),
    ('!', Conjunction Not),
    ('_', Restrictive On),
    ('/', Restrictive Near),
    ('^', PredicateText All),
    ('+', LinkingWord Makes),
    ('0', PredicateText Empty),
    ('1', StativeText Tele),
    ('2', StativeText More),
    ('K', StativeText Sink),
    ('L', StativeText Pull),
    ('M', StativeText Move),
    ('N', StativeText Float_),
    ('O', StativeText Hot),
    ('P', StativeText Push),
    ('Q', StativeText Shift),
    ('R', StativeText Weak),
    ('S', StativeText Stop),
    ('T', StativeText Defeat),
    ('U', StativeText Open),
    ('V', StativeText Shut),
    ('W', StativeText Win),
    ('Y', StativeText You),
    ('X', PredicateText Text),
    ('Z', StativeText Melt),
    ('A', NounText (Noun 'a') ),
    ('B', NounText (Noun 'b') ),
    ('C', NounText (Noun 'c') ),
    ('D', NounText (Noun 'd') ),
    ('E', NounText (Noun 'e') ),
    ('F', NounText (Noun 'f') ),
    ('G', NounText (Noun 'g') ),
    ('H', NounText (Noun 'h') ),
    ('I', NounText (Noun 'i') )
    ]

getTextInfoByCode c =
    case Dict.get c textByCode of

        Just text ->
            Just ( text, getTextInfo text )

        _ ->
            Nothing
