module Baba.Types exposing (..)

import Bitwise
import Dict as Dict

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

stativeDebugString stative = case stative of
    Push -> "push"
    Pull -> "pull"
    Move -> "move"
    More -> "more"
    Stop -> "stop"
    Defeat -> "defeat"
    Win -> "win"
    Open -> "open"
    Shut -> "shut"
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


typeInfos = Dict.fromList
    [
    ( '<', {text = LinkingWord Has, word = "has", glyph = { x = 0, y = 0, width = 18, height = 16}} ),
    ( '=', {text = LinkingWord Is, word = "is", glyph = { x = 26, y = 0, width = 9, height = 16}} ),
    ( '&', {text = Conjunction And, word = "and", glyph = { x = 43, y = 0, width = 18, height = 16}} ),
    ( '!', {text = Conjunction Not, word = "not", glyph = { x = 69, y = 0, width = 18, height = 16}} ),
    ( '_', {text = Restrictive On, word = "on", glyph = { x = 95, y = 0, width = 12, height = 16}} ),
    ( '/', {text = Restrictive Near, word = "near", glyph = { x = 115, y = 0, width = 23, height = 16}} ),
    ( '^', {text = PredicateText All, word = "all", glyph = { x = 146, y = 0, width = 16, height = 16}} ),
    ( '+', {text = LinkingWord Makes, word = "makes", glyph = { x = 170, y = 0, width = 31, height = 16}} ),
    ( '0', {text = PredicateText Empty, word = "empty", glyph = { x = 209, y = 0, width = 32, height = 16}} ),
    ( '1', {text = StativeText Tele, word = "Tele", glyph = { x = 0, y = 32, width = 24, height = 16}} ),
    ( '2', {text = StativeText More, word = "More", glyph = { x = 32, y = 32, width = 24, height = 16}} ),
    ( 'K', {text = StativeText Sink, word = "Sink", glyph = { x = 64, y = 32, width = 21, height = 16}} ),
    ( 'L', {text = StativeText Pull, word = "Pull", glyph = { x = 93, y = 32, width = 22, height = 16}} ),
    ( 'M', {text = StativeText Move, word = "Move", glyph = { x = 123, y = 32, width = 26, height = 16}} ),
    ( 'N', {text = StativeText Float_, word = "Float", glyph = { x = 157, y = 32, width = 29, height = 16}} ),
    ( 'O', {text = StativeText Hot, word = "Hot", glyph = { x = 194, y = 32, width = 18, height = 16}} ),
    ( 'P', {text = StativeText Push, word = "Push", glyph = { x = 220, y = 32, width = 24, height = 16}} ),
    ( 'Q', {text = StativeText Shift, word = "Shift", glyph = { x = 0, y = 64, width = 27, height = 16}} ),
    ( 'R', {text = StativeText Weak, word = "Weak", glyph = { x = 35, y = 64, width = 25, height = 16}} ),
    ( 'S', {text = StativeText Stop, word = "Stop", glyph = { x = 68, y = 64, width = 24, height = 16}} ),
    ( 'T', {text = StativeText Defeat, word = "Defeat", glyph = { x = 100, y = 64, width = 36, height = 16}} ),
    ( 'U', {text = StativeText Open, word = "Open", glyph = { x = 144, y = 64, width = 24, height = 16}} ),
    ( 'V', {text = StativeText Shut, word = "Shut", glyph = { x = 176, y = 64, width = 24, height = 16}} ),
    ( 'W', {text = StativeText Win, word = "Win", glyph = { x = 208, y = 64, width = 16, height = 16}} ),
    ( 'Y', {text = StativeText You, word = "You", glyph = { x = 232, y = 64, width = 19, height = 16}} ),
    ( 'X', {text = PredicateText Text, word = "text", glyph = { x = 0, y = 96, width = 25, height = 16}} ),
    ( 'Z', {text = StativeText Melt, word = "Melt", glyph = { x = 33, y = 96, width = 24, height = 16}} ),
    ( 'A', {text = NounText (Noun 'a'), word ="Zelda", glyph = { x = 65, y = 96, width = 29, height = 16}} ),
    ( 'B', {text = NounText (Noun 'b'), word ="water", glyph = { x = 102, y = 96, width = 30, height = 16}} ),
    ( 'C', {text = NounText (Noun 'c'), word ="rock", glyph = { x = 140, y = 96, width = 23, height = 16}} ),
    ( 'D', {text = NounText (Noun 'd'), word ="shrub", glyph = { x = 171, y = 96, width = 29, height = 16}} ),
    ( 'E', {text = NounText (Noun 'e'), word ="fence", glyph = { x = 208, y = 96, width = 30, height = 16}} ),
    ( 'F', {text = NounText (Noun 'f'), word ="key", glyph = { x = 0, y = 128, width = 19, height = 16}} ),
    ( 'G', {text = NounText (Noun 'g'), word ="statue", glyph = { x = 27, y = 128, width = 36, height = 16}} ),
    ( 'H', {text = NounText (Noun 'h'), word ="sign", glyph = { x = 71, y = 128, width = 21, height = 16}} ),
    ( 'I', {text = NounText (Noun 'i'), word ="Link", glyph = { x = 100, y = 128, width = 21, height = 16}} )    ]

getTypeInfoByCode code = Dict.get code typeInfos