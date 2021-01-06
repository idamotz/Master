{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Helpers where

import           Control.Lens
import qualified Data.Map     as M
import           Types
import Text.Pretty.Simple (pPrint)
import ExampleValidities hiding ((=:))
import qualified Data.IntervalMap.Generic.Strict as IM

type instance IxValue (IM.IntervalMap k v) = v
type instance Index (IM.IntervalMap Validity v) = TimePoint

lookupTP :: TimePoint -> ValidityMap a -> Maybe (Validity, a)
lookupTP tp im = IM.lookupMin $ IM.containing im tp

instance Ixed (IM.IntervalMap Validity v) where
  ix tp handler im = case lookupTP tp im of
                             Just (k, v) -> handler v <&> \v' -> IM.insert k v' im
                             Nothing     -> pure im

test :: IO ()
test = do
  let (=:) = (,)
  let exmap = IM.fromList 
                [  Validity (TP 2) (TP 3) =: 1
                ,  Validity (TP 4) (TP 7) =: 2
                ,  Validity (TP 8) (TP 10) =: 3
                ]
  print $ lookupTP (TP 2) exmap
  print $ lookupTP (TP 6) exmap
  print $ lookupTP (TP 9) exmap
  print $ 
    exmap &
      ix (TP 10)
      *~ 100


