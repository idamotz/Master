module Main where

import ExampleTemporalFeaturemodel
import Text.Pretty.Simple (pPrint)
import qualified TreeSequence as TS

main :: IO ()
main =
  pPrint $ TS.toTreeSequence exampleTemporalFeatureModel
