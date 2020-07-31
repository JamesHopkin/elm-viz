module Baba.Grammar exposing (..)

import Baba.Types as Types exposing
    ( textAsLinkingWord, textAsSubject, textAsComplement
    )

type alias Complement =
    { word : Types.Complement
    , sense : Bool
    }

type Rhs
    = Is (List Complement)
    | Link (List Types.Subject)

type alias Sentence =
    { subject : List Types.Subject
    , restriction : List Types.Restriction
    , rhs : List Rhs
    }

-- need a clause function to drop last complement
--  - must be true sense in Is case
--  - must leave at least one complement


{-
    Multiple clause stealing last item
        0  1 2  3  4  5 6
        a is A and b is B
    first clause comes out as 'a is A and b'
    
    steal b to use as 
-}


notes = """


    + means e.g. a+ -->  a [and a...]
    * means e.g. a* --> [a [and a...]]

Complement = not? tcompl
Rhs = (is Complement+) | (link Argument+)
Restriction = not? on|near noun 
Sentence = Subject+ Restriction* Rhs+

encode +s as item and list? (head without maybe!)

Hierarchy of calls?
    e.g. top level tries to parse subj then restr then rhs
or FSA?

    A -> B;
    B -> { B; C; rhs };
    C -> { C; rhs };
    rhs -> { is; link };
    is -> { D; rhs; done };
    link -> { E; rhs; done };

look for:
A: subject
B: subject or restriction or rhs 
C: restriction or rhs
rhs: is+complement or other linking word+argument
is: complement or rhs or done
link: argument or rhs or done

how to distinguish fail and done?
    those with done do not return maybes?


simplified version for first attempt:
    A, B, rhs and is

get it working before passing back remaining words or num consumed
"""


-- putting this in a function as an example to base the more complicated rhs on
parseSubject : List Types.Text -> Maybe ( Types.Subject, List Types.Text )
parseSubject words = case words of 
    head :: tail ->
        case textAsSubject head of
            Just subject ->
                Just ( subject, tail )

            _ ->
                Nothing

    _ ->
        Nothing

parseRhs : List Types.Text -> Maybe ( Rhs, List Types.Text )
parseRhs words = case words of
    Types.LinkingWord Types.Is :: isTail ->

        case parseComplement isTail of 
            Just ( complement, afterComplement ) ->
                let
                    accumComplements : List Complement -> List Types.Text -> ( List Complement, List Types.Text )
                    accumComplements acc complWords = case complWords of
                        Types.Conjunction Types.And :: afterAnd ->

                            case parseComplement afterAnd of
                                Just ( anotherComplement, remaining ) ->
                                    accumComplements
                                        (anotherComplement :: acc)
                                        remaining

                                _ ->
                                    ( acc, complWords )

                        _ ->
                            ( acc, complWords )
                    ( complements, afterAllComplements ) = accumComplements [complement] afterComplement
                in
                Just ( Is complements, afterAllComplements )


            _ ->
                -- nothing after "is"
                Nothing

-- todo other linking words
--    Types.LinkingWord link :: linkTail ->


    _ ->
        -- no linking word
        Nothing


parseComplement : List Types.Text -> Maybe ( Complement, List Types.Text )
parseComplement words = case words of
    Types.Conjunction Types.Not :: notTerm :: afterNotComplement ->
            case textAsComplement notTerm of
                Just complement ->
                    Just
                        ( { word = complement, sense = False }
                        , afterNotComplement
                        )

                _ ->
                    -- "not" must be followed by complement
                    Nothing

    head :: tail ->
        case textAsComplement head of
            Just complement ->
                Just
                    ( { word = complement, sense = True }
                    , tail
                    )

            _ ->
                -- word after "is" not valid
                Nothing

    _ ->
        -- no word
        Nothing

parseRestriction : List Types.Text -> Maybe ( Types.Restriction, List Types.Text )
parseRestriction words = case words of
    Types.Conjunction Types.Not :: Types.Restrictive restrictive :: tail ->
        case parseSubject tail of 
            Just ( noun, rest ) ->
                Just
                    ( { word = restrictive, noun = noun, sense = False }
                    , rest
                    )

            _ ->
                Nothing

    Types.Restrictive restrictive :: tail ->
        case parseSubject tail of 
            Just ( noun, rest ) ->
                Just
                    ( { word = restrictive, noun = noun, sense = True }
                    , rest
                    )
            _ ->
                Nothing

    _ ->
        -- no restrictive
        Nothing

-- A
parse : List Types.Text -> Maybe Sentence
parse words = 
    case parseSubject words of 
        Just ( subject, tail ) ->
            subjectState [subject] tail

        _ ->
            Nothing

-- B
subjectState : (List Types.Subject) -> List Types.Text -> Maybe Sentence
subjectState subject words = case words of 
    Types.Conjunction Types.And :: tail ->
        case parseSubject tail of
            Just ( anotherSubject, afterSubject ) ->
                subjectState
                    (anotherSubject :: subject)
                    afterSubject

            _ ->
                -- 'and' not followed by subject: syntax error
                Nothing


    _ ->
        case parseRestriction words of
            Just ( restriction, afterRestriction ) ->
                restrictionState
                    { subject = subject
                    , restriction = [restriction]
                    }
                    afterRestriction

            _ ->
                case parseRhs words of
                    Just ( rhs, rest ) ->
                        Just <| rhsState
                            { subject = subject
                            , restriction = []
                            , rhs = [rhs]
                            }
                            rest

                    _ ->
                        -- no linking word
                        Nothing

type alias RestrictionStateArg =
    { subject : List Types.Subject
    , restriction : List Types.Restriction
    }

restrictionState : RestrictionStateArg -> List Types.Text -> Maybe Sentence
restrictionState soFar words = case words of
    Types.Conjunction Types.And :: tail ->
        case parseRestriction tail of
            Just ( restriction, afterRestriction ) ->
                restrictionState
                    { subject = soFar.subject
                    , restriction = restriction :: soFar.restriction
                    }
                    afterRestriction

            _ ->
                -- syntax error: "add" with no restriction
                Nothing

    _ ->
                --let
                --   dummy = Debug.log "restrictionState" soFar.restriction
                --in
                case parseRhs words of

                    Just ( rhs, rest ) ->
                        Just <| rhsState
                            { subject = soFar.subject
                            , restriction = soFar.restriction
                            , rhs = [rhs]
                            }
                            rest

                    _ ->
                        -- no linking word
                        Nothing


-- rhs
rhsState : Sentence -> List Types.Text -> Sentence
rhsState sentenceSoFar words = case words of
    Types.Conjunction Types.And :: tail ->
        case parseRhs tail of
            Just ( anotherRhs, afterRhs ) ->
                rhsState
                    { sentenceSoFar
                    | rhs = anotherRhs :: sentenceSoFar.rhs
                    }
                    afterRhs

            _ ->
                sentenceSoFar
    _ ->
        sentenceSoFar
