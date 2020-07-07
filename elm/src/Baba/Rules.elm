module Baba.Rules exposing ( lookForRules, ruleDebugString )

import Baba.Cell exposing (..)
import Baba.LinkedGrid as LinkedGrid exposing ( Direction (..) )
import Baba.Types as Types

type Rule
    = Is Types.Subject Types.Complement
    | Has Types.Subject Types.Subject

ruleDebugString rule = case rule of
    Is c v -> String.join " " [Types.subjectDebugString c, "is", Types.complementDebugString v]
    Has l r -> String.join " " [Types.subjectDebugString l, "has", Types.subjectDebugString r]

tempGetFirst cell = case cell of
    first :: rest -> first
    _ -> makeObject -1 '$' -- need to fix if these start coming out

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

            case ( asText x, asStative x, asText z ) of
                ( Nothing, _, _ ) -> a
                ( _, Just _, _ )  -> a
                ( _, _, Nothing ) -> a
                _ ->
                    let
                        firstX = getObjectWord (tempGetFirst x)
                        firstY = getObjectWord (tempGetFirst y)
                        firstZ = getObjectWord (tempGetFirst z)
                    in
                        case ( firstY, asStative z ) of
                            ( '=', Just zStative ) ->
                                Is
                                    (Types.NounSubject (Types.Noun firstX))
                                    (Types.Stative (stativeFromOccupant zStative))
                                :: a

                            ( '=', Nothing ) ->
                                Is
                                    (Types.NounSubject (Types.Noun firstX))
                                    (Types.NounComplement (Types.Noun firstZ))
                                :: a

                            ( '<', Nothing ) ->
                                Has
                                    (Types.NounSubject (Types.Noun firstX))
                                    (Types.NounSubject (Types.Noun firstZ))
                                :: a
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
                rowRules ++ columnRules
        _ -> []