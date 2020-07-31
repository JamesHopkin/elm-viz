module Baba.Rules exposing ( lookForRules, ruleDebugString, getTransform,
                            Rule(..) )

import Baba.Cell exposing (..)
import Baba.LinkedGrid as LinkedGrid exposing ( Direction (..) )
import Baba.Types as Types



type Rule
    = Is Types.Subject Types.Complement
    | Has Types.Subject Types.Subject

type alias RestrictedRule =
    { rule : Rule
    , restrictions : List Types.Restriction
    }

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