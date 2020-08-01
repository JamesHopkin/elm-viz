module Baba.Rules exposing ( lookForRules, ruleDebugString, getTransform, getApplicableStative,
                            rulesFromSentence, ruleDebugString_New, PositiveAndNegativeRules,
                            lookForRules_New, getApplicableStative_New, getApplicableTransform,
                            Rule(..), Rule_New )

import Baba.Cell as Cell exposing (..)
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

add ( lp, ln ) ( rp, rn ) = ( lp ++ ln, rp ++ rn )

concatMap : (a -> ( List b, List c )) -> List a -> ( List b, List c )
concatMap f list =
    let
        foldFunc ( l, r ) ( lacc, racc ) = ( l ++ lacc, r ++ racc )

    in
    List.foldr foldFunc ( [], [] ) (List.map f list)

lookForRulesOnAxis_New : Axis -> PositiveAndNegativeRules
lookForRulesOnAxis_New startingAxis = 
    let
        impl : Maybe Axis -> List Types.Text -> PositiveAndNegativeRules -> PositiveAndNegativeRules
        impl axis current (( pos, neg ) as complete) =
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
                                        |> concatMap rulesFromSentence
                                        |> add complete
                                else
                                    complete
                        in
                        if isJust axis then
                            impl next [] sentences
                        else
                            sentences
    in
    impl (Just startingAxis) [] ( [], [] )


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

lookForRules_New : Grid -> PositiveAndNegativeRules
lookForRules_New grid =
    case LinkedGrid.at 0 0 grid of
        Just origin ->
            let
                rowRules : PositiveAndNegativeRules
                rowRules = 
                    let
                        rowFunc : Location -> PositiveAndNegativeRules
                        rowFunc loc = lookForRulesOnAxis_New <| LinkedGrid.makeAxis loc Right

                        prependRow : Location -> PositiveAndNegativeRules -> PositiveAndNegativeRules
                        prependRow = rowFunc >> add

                        ( pos, neg ) = fold LinkedGrid.below prependRow origin ( [], [] )

                        textIsPush = Is_New (Types.Predicate Types.Text) [] (Types.Stative Types.Push)
                    in
                        ( textIsPush :: pos, neg )


                columnRules : PositiveAndNegativeRules
                columnRules = 
                    let
                        columnFunc loc = lookForRulesOnAxis_New <| LinkedGrid.makeAxis loc Down
                    in
                        fold LinkedGrid.right (columnFunc >> add) origin ( [], [] )

            in
                add rowRules columnRules

        _ -> ( [], [] )

getTransform rule = case rule of
    Is_New subj restrictions (Types.NounComplement (Types.Noun obj)) ->
        Just ( subj, restrictions, obj )

    _ ->
        Nothing


type Rule_New
    = Is_New Types.Subject (List Grammar.Restriction) Types.Complement
    | Link Types.LinkingWord Types.Subject (List Grammar.Restriction) Types.Subject

ruleDebugString_New rule sense = 
    let

        subjectAndRestrictions s r = 
            [Types.subjectDebugString s] ++ (if List.length r == 0 then [] else [
                String.join " and " (List.map Grammar.restrictionDebugString r)
            ])
    in
    case rule of
        Is_New s r c -> String.join " " <| (subjectAndRestrictions s r) ++ [if sense then "is" else "is not", Types.complementDebugString c]
        Link l s r o -> String.join " " <| (subjectAndRestrictions s r) ++ [Types.linkingWordDebugString l, Types.subjectDebugString o]

getRestrictions : Rule_New -> List Grammar.Restriction
getRestrictions rule =
    []

type alias PositiveAndNegativeRules = ( List Rule_New, List Rule_New )

rulesFromSentence : Grammar.Sentence -> PositiveAndNegativeRules
rulesFromSentence sentence =
    let

        subjectFold : Types.Subject -> PositiveAndNegativeRules -> PositiveAndNegativeRules
        subjectFold subject acc =
            let
                rhsFold : Grammar.Rhs -> PositiveAndNegativeRules -> PositiveAndNegativeRules
                rhsFold rhs rhsAcc = 
                    case rhs of
                        Grammar.Is complements ->
                            let
                                complementFold : Grammar.Complement -> PositiveAndNegativeRules -> PositiveAndNegativeRules
                                complementFold compl (( pos, neg ) as complAcc) =
                                    let
                                        rule = Is_New subject sentence.restriction compl.word
                                    in
                                    if compl.sense then
                                        ( rule :: pos, neg )
                                    else
                                        ( pos, rule :: neg )
                            in
                                List.foldr complementFold rhsAcc complements
                        _ ->
                            rhsAcc
            in
            List.foldr rhsFold acc sentence.rhs
    in
    List.foldr subjectFold ( [], [] ) sentence.subject

-- will need neighbouring cells in the end
getApplicableStative : Rule -> Cell -> Object -> Maybe Types.Stative
getApplicableStative rule cell object =
    case ( Cell.getObjectWord object, rule ) of 
            ( Cell.Instance objectNoun, Is (Types.NounSubject noun) (Types.Stative stative) ) ->
                if Types.nounsEqual noun objectNoun then
                    Just stative

                else
                    Nothing

            ( Cell.Text _, Is (Types.Predicate Types.Text) (Types.Stative stative) ) ->
                Just stative

            _ ->
                Nothing

getApplicableStative_New : Rule_New -> Cell -> Object -> Maybe Types.Stative
getApplicableStative_New rule cell object =
    case ( Cell.getObjectWord object, rule ) of 
            ( Cell.Instance objectNoun, Is_New (Types.NounSubject noun) restr (Types.Stative stative) ) ->
                if Types.nounsEqual noun objectNoun then
                    Just stative

                else
                    Nothing

            ( Cell.Text _, Is_New (Types.Predicate Types.Text) [] (Types.Stative stative) ) ->
                Just stative

            _ ->
                Nothing

getApplicableTransform rule cell object =
    case ( Cell.getObjectWord object, rule ) of 
        ( Cell.Instance objectNoun, Is_New (Types.NounSubject noun) restr (Types.NounComplement obj) ) ->
            if Types.nounsEqual objectNoun noun then
                Just obj

            else
                Nothing

        ( Cell.Text _, Is_New (Types.Predicate Types.Text) restr (Types.NounComplement obj) ) ->
            Just obj

        _ ->
            Nothing