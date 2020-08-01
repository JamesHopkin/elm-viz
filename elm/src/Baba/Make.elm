module Baba.Make exposing ( doTransformations )

import Baba.Cell as Cell exposing ( Grid )
import Baba.Rules exposing ( Rule(..), PositiveAndNegativeRules, Rule_New(..), getApplicableTransform )
import Baba.Types as Types

import Baba.LinkedGrid as LinkedGrid

-------------------------------
-- "makes" and transformations

emptyGrid : Grid
emptyGrid = LinkedGrid.make [] 0 0


doTransformations : PositiveAndNegativeRules -> Grid -> Grid
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

                ruleFold : Rule_New -> Cell.Grid -> Cell.Grid
                ruleFold rule ruleApplyGrid =
                    case LinkedGrid.at x y ruleApplyGrid of
                        Just location ->
                            let
                                cell = LinkedGrid.getContents location
                            in

                            case getApplicableTransform rule cell object of
                                Just newNoun ->
                                    LinkedGrid.gridFromLocation (updateCell newNoun location)

                                _ ->
                                    ruleApplyGrid

                        _ ->
                            emptyGrid -- should never happen!


            in
            List.foldl ruleFold objFoldGrid (Tuple.first rules) -- work out how to handle negatives next

    in
    Cell.foldObjects objFold grid grid