module Baba.Make exposing ( doTransformations )

import Baba.Cell as Cell exposing ( Grid )
import Baba.Rules exposing ( Rule )

-------------------------------
-- "makes" and transformations


-- probably want to go through items by id, rather than looking in each location

doTransformations : List Rule -> Grid -> Grid
doTransformations rules grid =
    let
        ruleFold : Int -> Int -> Rule -> Cell.Grid -> Cell.Grid
        ruleFold x y rule ruleApplyGrid = ruleApplyGrid

        objFold : ( Int, Int, Cell.Object ) -> Cell.Grid -> Cell.Grid
        objFold ( x, y, object ) objFoldGrid =
                    case Cell.getObjectWord object of 
                        Cell.Instance noun -> 
                            objFoldGrid

                        _ ->
                            objFoldGrid

    in
    Cell.foldObjects objFold grid grid