module Main where

import ExampleIntervalBasedFeatureModel
import Text.Pretty.Simple (pPrint)
import TreeSequence

main :: IO ()
main =
  pPrint $ toTreeSequence exampleIntervalBasedFeatureModel
