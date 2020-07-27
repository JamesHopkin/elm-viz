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

            case Cell.getObjectWord object of 
                    Cell.Instance noun ->
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


                            ruleFold : Rule -> Cell.Grid -> Cell.Grid
                            ruleFold rule ruleApplyGrid =
                                case rule of
                                        Is (Types.NounSubject subject) (Types.NounComplement target) ->
                                            if Types.nounsEqual noun subject then
                                                case LinkedGrid.at x y ruleApplyGrid of
                                                        Just location ->
                                                            LinkedGrid.gridFromLocation (updateCell target location)

                                                        _ ->
                                                            emptyGrid -- should never happen!

                                            else
                                                ruleApplyGrid


                                        _ ->
                                            ruleApplyGrid

                        in
                        List.foldl ruleFold objFoldGrid rules

                    _ ->
                        objFoldGrid

    in
    Cell.foldObjects objFold grid grid