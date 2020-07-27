module Baba.Make exposing ( doTransformations )

import Baba.Cell as Cell exposing ( Grid )
import Baba.Rules exposing ( Rule(..) )
import Baba.Types as Types

import Baba.LinkedGrid as LinkedGrid

-------------------------------
-- "makes" and transformations

emptyGrid : Grid
emptyGrid = LinkedGrid.make [] 0 0


doTransformations : List Rule -> Grid -> Grid
doTransformations rules grid =
    let
        objFold : ( Int, Int, Cell.Object ) -> Cell.Grid -> Cell.Grid
        objFold ( x, y, object ) objFoldGrid =
            let
                updateCell : Types.Noun -> Cell.Location -> Cell.Location
                updateCell newNoun location =
                    let
                        id = Cell.getObjectId object
                        withoutObject = List.filter (Cell.getObjectId >> (/=) id)
                            <| LinkedGrid.getContents location

                        updatedObject = Cell.setObjectWordAndFlags (Cell.Instance newNoun) 0 object
                    in
                        LinkedGrid.setContents (updatedObject :: withoutObject) location


                applicable rule =
                    case ( Cell.getObjectWord object, rule ) of 
                        ( Cell.Instance objectNoun, Is (Types.NounSubject noun) (Types.NounComplement obj) ) ->
                            if Types.nounsEqual objectNoun noun then
                                Just obj

                            else
                                Nothing

                        ( Cell.Text _, Is (Types.Predicate Types.Text) (Types.NounComplement obj) ) ->
                            Just obj

                        _ ->
                            Nothing

                ruleFold : Rule -> Cell.Grid -> Cell.Grid
                ruleFold rule ruleApplyGrid =
                    case applicable rule of
                        Just newNoun ->
                            case LinkedGrid.at x y ruleApplyGrid of
                                    Just location ->
                                        LinkedGrid.gridFromLocation (updateCell newNoun location)

                                    _ ->
                                        emptyGrid -- should never happen!

                        _ ->
                            ruleApplyGrid

            in
            List.foldl ruleFold objFoldGrid rules

    in
    Cell.foldObjects objFold grid grid