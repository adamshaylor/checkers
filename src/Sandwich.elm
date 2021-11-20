module Sandwich exposing (..)

type Sandwich breadType fillingType
  = JustBread breadType
  | SandwichWithFilling breadType fillingType (Sandwich breadType fillingType)

justBread : breadType -> Sandwich breadType fillingType
justBread bread = JustBread bread

addLayer : fillingType -> breadType -> Sandwich breadType fillingType -> Sandwich breadType fillingType
addLayer newLayerFilling newLayerBread sandwich =
  SandwichWithFilling newLayerBread newLayerFilling sandwich

getLastBread : Sandwich breadType fillingType -> breadType
getLastBread sandwich =
  case sandwich of
    JustBread bread -> bread
    SandwichWithFilling bread _ _ -> bread

getLastPlusNBread : Int -> Sandwich breadType fillingType -> Maybe breadType
getLastPlusNBread n sandwich =
  if n < 0 then
    Nothing
  else
    case (n, sandwich) of
      (0, _) -> Just (getLastBread sandwich)
      (_, JustBread _) -> Nothing
      (_, SandwichWithFilling _ _ restOfSandwich) -> getLastPlusNBread (n - 1) restOfSandwich

getLastFilling : Sandwich breadType fillingType -> Maybe fillingType
getLastFilling sandwich =
  case sandwich of
    JustBread _ -> Nothing
    SandwichWithFilling _ filling _ -> Just filling

getLastPlusNFilling : Int -> Sandwich breadType fillingType -> Maybe fillingType
getLastPlusNFilling n sandwich =
  if n < 0 then
    Nothing
  else
    case (n, sandwich) of
      (0, _) -> getLastFilling sandwich
      (_, JustBread _) -> Nothing
      (_, SandwichWithFilling _ _ restOfSandwich) -> getLastPlusNFilling (n - 1) restOfSandwich
