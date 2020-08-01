module Baba.Rules exposing ( getTransform,
                            rulesFromSentence, ruleDebugString, PositiveAndNegativeRules,
                            lookForRules, getApplicableStative, getApplicableTransform,
                            Rule )

import Baba.Cell as Cell exposing (..)
import Baba.Grammar as Grammar
import Baba.LinkedGrid as LinkedGrid exposing ( Direction (..) )
import Baba.Types as Types

import Baba.Util exposing (..)


add ( lp, ln ) ( rp, rn ) = ( lp ++ rp, ln ++ rn )

concatMap : (a -> ( List b, List c )) -> List a -> ( List b, List c )
concatMap f list =
    let
        foldFunc ( l, r ) ( lacc, racc ) = ( l ++ lacc, r ++ racc )

    in
    List.foldr foldFunc ( [], [] ) (List.map f list)

lookForRulesOnAxis : Axis -> PositiveAndNegativeRules
lookForRulesOnAxis startingAxis = 
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
                                    --let
                                    --    ( p, n ) = complete
                                    --    dummy = Debug.log "on axis" [List.length p, List.length n]
                                    --in
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

lookForRules : Grid -> PositiveAndNegativeRules
lookForRules grid =
    case LinkedGrid.at 0 0 grid of
        Just origin ->
            let
                rowRules : PositiveAndNegativeRules
                rowRules = 
                    let
                        rowFunc : Location -> PositiveAndNegativeRules
                        rowFunc loc = lookForRulesOnAxis <| LinkedGrid.makeAxis loc Right

                        ( pos, neg ) = fold LinkedGrid.below (rowFunc >> add) origin ( [], [] )

                        textIsPush = Is (Types.Predicate Types.Text) [] (Types.Stative Types.Push)
                    in
                        ( textIsPush :: pos, neg )


                columnRules : PositiveAndNegativeRules
                columnRules = 
                    let
                        columnFunc loc = lookForRulesOnAxis <| LinkedGrid.makeAxis loc Down
                        result = fold LinkedGrid.right (columnFunc >> add) origin ( [], [] )
                    in
                        result

            in
                add rowRules columnRules

        _ -> ( [], [] )

getTransform rule = case rule of
    Is subj restrictions (Types.NounComplement (Types.Noun obj)) ->
        Just ( subj, restrictions, obj )

    _ ->
        Nothing


type Rule
    = Is Types.Subject (List Grammar.Restriction) Types.Complement
    | Link Types.LinkingWord Types.Subject (List Grammar.Restriction) Types.Subject

ruleDebugString sense rule = 
    let

        subjectAndRestrictions s r = 
            [Types.subjectDebugString s] ++ (if List.length r == 0 then [] else [
                String.join " and " (List.map Grammar.restrictionDebugString r)
            ])
    in
    case rule of
        Is s r c -> String.join " " <| (subjectAndRestrictions s r) ++ [if sense then "is" else "is not", Types.complementDebugString c]
        Link l s r o -> String.join " " <| (subjectAndRestrictions s r) ++ [Types.linkingWordDebugString l, Types.subjectDebugString o]

getRestrictions : Rule -> List Grammar.Restriction
getRestrictions rule =
    []

type alias PositiveAndNegativeRules = ( List Rule, List Rule )

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
                                        rule = Is subject sentence.restriction compl.word
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
            ( Cell.Instance objectNoun, Is (Types.NounSubject noun) restr (Types.Stative stative) ) ->
                if Types.nounsEqual noun objectNoun then
                    Just stative

                else
                    Nothing

            ( Cell.Text _, Is (Types.Predicate Types.Text) [] (Types.Stative stative) ) ->
                Just stative

            _ ->
                Nothing

getApplicableTransform rule cell object =
    case ( Cell.getObjectWord object, rule ) of 
        ( Cell.Instance objectNoun, Is (Types.NounSubject noun) restr (Types.NounComplement obj) ) ->
            if Types.nounsEqual objectNoun noun then
                Just obj

            else
                Nothing

        ( Cell.Text _, Is (Types.Predicate Types.Text) restr (Types.NounComplement obj) ) ->
            Just obj

        _ ->
            Nothing