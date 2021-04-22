module Program where

import Apply (apply)
import Types
import Validate (validate)

validateAndApply :: TimeOperation -> IntervalBasedFeatureModel -> Either [ValidationError] IntervalBasedFeatureModel
validateAndApply o ibfm = choose (validate o ibfm) (apply o ibfm)
  where
    choose [] x = Right x
    choose es _ = Left es
