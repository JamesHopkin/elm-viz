module Baba.Rules exposing ( lookForRules )

import Baba.Cell exposing (..)
import Baba.LinkedGrid as LinkedGrid exposing ( Direction (..) )
import Baba.Types exposing (..)

tempGetFirst cell = case cell of
  first :: rest -> first
  _ -> ( -1, '$' ) -- need to fix if these start coming out

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

      case ( asText x, asVerb x, asText z ) of
        ( Nothing, _, _ ) -> a
        ( _, Just _, _ )  -> a
        ( _, _, Nothing ) -> a
        _ ->
          let
            ( _, firstX ) = tempGetFirst x
            ( _, firstZ ) = tempGetFirst z
          in
            case ( tempGetFirst y, asVerb z ) of
              ( ( _, '=' ), Just zVerb ) -> Is      firstX (verbFromOccupant zVerb) :: a
              ( ( _, '='), Nothing )     -> Becomes firstX firstZ                   :: a
              ( ( _, '<'), Nothing )     -> Has     firstX firstZ                   :: a
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