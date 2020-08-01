module Baba.Rules exposing ( lookForRules, ruleDebugString, getTransform,
                            rulesFromSentence, ruleDebugString_New,
                            Rule(..) )

import Baba.Cell exposing (..)
import Baba.Grammar as Grammar
import Baba.LinkedGrid as LinkedGrid exposing ( Direction (..) )
import Baba.Types as Types

import Baba.Util exposing (..)

type Rule
    = Is Types.Subject Types.Complement
    | Has Types.Subject Types.Subject


ruleDebugString rule = case rule of
    Is c v -> String.join " " [Types.subjectDebugString c, "is", Types.complementDebugString v]
    Has l r -> String.join " " [Types.subjectDebugString l, "has", Types.subjectDebugString r]

surrounding : 
    (el -> el -> el -> acc -> acc)
    -> acc
    -> LinkedGrid.Axis el
    -> acc
surrounding fn a axis =
        case ( LinkedGrid.axisForward -1 axis, LinkedGrid.axisForward 1 axis ) of
            ( Just prevAxis, Just nextAxis )
                -> fn
                    (LinkedGrid.axisGet prevAxis)
                    (LinkedGrid.axisGet axis)
                    (LinkedGrid.axisGet nextAxis)
                    (surrounding fn a nextAxis)

            ( Nothing, Just nextAxis )
                -> surrounding fn a nextAxis

            _ -> a

lookForRulesOnAxis : Axis -> List Rule
lookForRulesOnAxis axis = 
    let
        impl : Cell -> Cell -> Cell -> List Rule -> List Rule
        impl x y z a =
            case ( firstSubject x, firstLinkingWord y ) of
                ( Just lhs, Just link ) ->
                    let
                        complementCases complement =
                            case link of
                                Types.Is ->
                                    Just (Is lhs complement)

                                Types.Has ->
                                    Maybe.map (\rhs -> Has lhs rhs) (Types.complementAsSubject complement)

                                _ -> Nothing
                    in
                    case Maybe.andThen complementCases (firstComplement z) of
                        Just rule -> rule :: a
                        _ -> a

                _ -> a

    in
        surrounding impl [] axis

lookForRulesOnAxis_New : Axis -> List Rule_New
lookForRulesOnAxis_New startingAxis = 
    let
        impl : Maybe Axis -> List Types.Text -> List Rule_New -> List Rule_New
        impl axis current complete =
            let
                getText a = firstText (LinkedGrid.axisGet a)
                next = Maybe.andThen (LinkedGrid.axisForward 1) axis
            in
            case Maybe.andThen getText axis of
                    Just text ->
                        impl next (text :: current) complete

                    _ ->
                        let
                            sentences =
                                if List.length current > 0 then
                                    Grammar.findSentences (List.reverse current)
                                        |> List.concatMap rulesFromSentence
                                        |> (++) complete
                                else
                                    complete
                        in
                        if isJust axis then
                            impl next [] sentences
                        else
                            sentences
    in
    impl (Just startingAxis) [] []


fold :
    (loc -> Maybe loc)
    -> (loc -> acc -> acc)
    -> loc
    -> acc -> acc
fold nextFn f loc acc =
    let
        soFar = f loc acc
    in
        case nextFn loc of
            Just next -> fold nextFn f next soFar
            _ -> soFar

lookForRules : Grid -> List Rule
lookForRules grid =
    case LinkedGrid.at 0 0 grid of
        Just origin ->
            let
                rowRules : List Rule
                rowRules = 
                    let
                        rowFunc : Location -> List Rule
                        rowFunc loc = lookForRulesOnAxis <| LinkedGrid.makeAxis loc Right

                        prependRow : Location -> List Rule -> List Rule
                        prependRow = rowFunc >> (++)
                    in
                        fold LinkedGrid.below prependRow origin []


                columnRules : List Rule
                columnRules = 
                    let
                        columnFunc loc = lookForRulesOnAxis <| LinkedGrid.makeAxis loc Down
                    in
                        fold LinkedGrid.right (columnFunc >> (++)) origin []

            in
                (Is (Types.Predicate Types.Text) (Types.Stative Types.Push)) ::
                    rowRules ++ columnRules
        _ -> []

getTransform rule = case rule of
    Is subj (Types.NounComplement (Types.Noun obj)) ->
        Just ( subj, obj )

    _ ->
        Nothing


type Rule_New
    = Is_New Types.Subject (List Grammar.Restriction) Grammar.Complement
    | Link Types.LinkingWord Types.Subject (List Grammar.Restriction) Types.Subject

ruleDebugString_New rule = 
    let

        subjectAndRestrictions s r = 
            [Types.subjectDebugString s] ++ (if List.length r == 0 then [] else [
                String.join " and " (List.map Grammar.restrictionDebugString r)
            ])
    in
    case rule of
        Is_New s r c -> String.join " " <| (subjectAndRestrictions s r) ++ ["is", Grammar.complementDebugString c]
        Link l s r o -> String.join " " <| (subjectAndRestrictions s r) ++ [Types.linkingWordDebugString l, Types.subjectDebugString o]

getRestrictions : Rule_New -> List Grammar.Restriction
getRestrictions rule =
    []

rulesFromSentence : Grammar.Sentence -> List Rule_New
rulesFromSentence sentence =
    let

        subjectFold : Types.Subject -> List Rule_New -> List Rule_New
        subjectFold subject acc =
            let
                rhsFold : Grammar.Rhs -> List Rule_New -> List Rule_New
                rhsFold rhs rhsAcc = 
                    case rhs of
                        Grammar.Is complements ->
                            let
                                complementFold : Grammar.Complement -> List Rule_New -> List Rule_New
                                complementFold compl complAcc =
                                    (Is_New subject sentence.restriction compl) :: complAcc
                            in
                                List.foldr complementFold rhsAcc complements
                        _ ->
                            rhsAcc
            in
            List.foldr rhsFold acc sentence.rhs
    in
    List.foldr subjectFold [] sentence.subject
