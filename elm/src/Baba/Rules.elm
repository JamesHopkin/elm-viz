module Baba.Rules exposing ( isTransform,
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

type Rule
    = Is Types.Subject (List Grammar.Restriction) Types.Complement
    | Link Types.LinkingWord Types.Subject (List Grammar.Restriction) Types.Subject

ruleDebugString sense rule = 
    let

        subjectAndRestrictions s r = 
            Types.subjectDebugString s :: (if List.length r == 0 then [] else [
                String.join " and " (List.map Grammar.restrictionDebugString r)
            ])
    in
    case rule of
        Is s r c -> String.join " " <| subjectAndRestrictions s r ++ [if sense then "is" else "is not", Types.complementDebugString c]
        Link l s r o -> String.join " " <| subjectAndRestrictions s r ++ [Types.linkingWordDebugString l, Types.subjectDebugString o]

getRestrictions : Rule -> List Grammar.Restriction
getRestrictions rule = case rule of
    Is _ restrictions _
        -> restrictions

    Link _ _ restrictions _
        -> restrictions

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
                                complementFold compl ( pos, neg ) =
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

passesRestriction : Grammar.Restriction -> Cell -> Bool
passesRestriction restriction cell =
    case restriction.word of
        Types.On ->
            case restriction.noun of
                Types.Predicate Types.Empty ->
                    List.length cell == 1

                _ ->
                    List.any (\obj -> Cell.objectMatchesSubject obj restriction.noun) cell
        _ ->
            -- only handling On so far
            True

checkRestrictions restrictions cell result =
    let
        foldFunc r acc =
            if isJust acc && passesRestriction r cell then
                acc

            else
                Nothing
    in
    List.foldr foldFunc (Just result) restrictions

-- will need neighbouring cells in the end
getApplicableStative : Rule -> Cell -> Object -> Maybe Types.Stative
getApplicableStative rule cell object =
    case rule of
        Is subject restrictions (Types.Stative stative) ->
            if wordMatchesSubject (Cell.getObjectWord object) subject then
                checkRestrictions restrictions cell stative
            else
                Nothing

        _ ->
            Nothing

isTransform rule = case rule of
    Is _ _ (Types.Stative _) ->
        False

    Is _ _ _ ->
        True

    _ ->
        False

getApplicableTransform : Rule -> Cell -> Object -> Maybe Cell.ObjectKind
getApplicableTransform rule cell object =
    case rule of
        Is subject restrictions complement ->
            if wordMatchesSubject (Cell.getObjectWord object) subject then
                case ( subject, complement ) of
                    ( _, Types.NounComplement noun ) ->
                        checkRestrictions restrictions cell (Cell.Instance noun)

                    ( Types.NounSubject noun, Types.PredicateComplement Types.Text ) ->
                        checkRestrictions restrictions cell <| Cell.Text <| Types.NounText noun

                    _ ->
                        Nothing

            else
                Nothing

        _ ->
            Nothing
