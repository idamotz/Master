module Program where

import Apply (apply)
import Types
import Validate (validate)

validateAndApply :: TimeOperation -> TemporalFeatureModel -> Either [ValidationError] TemporalFeatureModel
validateAndApply o tfm = choose (validate o tfm) (apply o tfm)
  where
    choose [] x = Right x
    choose es _ = Left es
