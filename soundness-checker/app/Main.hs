module Main where

import Convert
import ExampleEvolutionPlan
import Text.Pretty.Simple (pPrint)

main :: IO ()
main =
  pPrint $ convert exampleEvolutionPlan
