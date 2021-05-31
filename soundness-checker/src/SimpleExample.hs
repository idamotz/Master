{-# LANGUAGE OverloadedLists #-}

module SimpleExample where

import qualified Data.IntervalMap.Generic.Strict as IM
import Types

im :: Validity -> a -> ValidityMap a
im = IM.singleton

simplePlan :: IntervalBasedFeatureModel
simplePlan =
  IntervalBasedFeatureModel
    (FeatureID "feature:root")
    [
      ( "Root"
      , im (Validity (TP 1) Forever) (FeatureID "feature:root")
      )
    ]
    [
      ( FeatureID "feature:root"
      , FeatureValidity
          (im (Validity (TP 1) Forever) ())
          (im (Validity (TP 1) Forever) "Root")
          (im (Validity (TP 1) Forever) Mandatory)
          mempty
          (im (Validity (TP 1) (TP 5)) [GroupID "group:A"])
      )
    ]
    [
      ( GroupID "group:A"
      , GroupValidity
          (im (Validity (TP 1) (TP 5)) ())
          (im (Validity (TP 1) (TP 5)) And)
          (im (Validity (TP 1) (TP 5)) (FeatureID "feature:root"))
          mempty
      )
    ]

paradoxOp :: UpdateOperation
paradoxOp =
  AddOperation (Validity (TP 3) Forever) $
    AddFeature (FeatureID "feature:B") "B" Mandatory (GroupID "group:A")

soundOp :: UpdateOperation
soundOp =
  AddOperation (Validity (TP 3) (TP 5)) $
    AddFeature (FeatureID "feature:B") "B" Mandatory (GroupID "group:A")
