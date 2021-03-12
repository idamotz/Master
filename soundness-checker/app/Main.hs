module Main where

import ExampleTemporalFeaturemodel
import Text.Pretty.Simple (pPrint)
import TreeSequence

main :: IO ()
main =
  pPrint $ toTreeSequence exampleTemporalFeatureModel
