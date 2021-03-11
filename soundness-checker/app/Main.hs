module Main where

import Apply (apply)
import Convert
import ExampleEvolutionPlan
import Text.Pretty.Simple (pPrint)
import Types
import Validate (validate)

main :: IO ()
main =
  pPrint $ convert exampleEvolutionPlan

validateAndApply :: TimeOperation -> TemporalFeatureModel -> Either [ValidationError] TemporalFeatureModel
validateAndApply o tfm = choose (validate o tfm) (apply o tfm)
  where
    choose [] x = Right x
    choose es _ = Left es
