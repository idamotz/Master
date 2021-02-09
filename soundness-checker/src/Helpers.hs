{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Helpers where

import Control.Lens
import qualified Data.IntervalMap.Generic.Strict as IM
import Data.List ((\\))
import qualified Data.Map as M
import ExampleValidities hiding ((=:))
import Text.Pretty.Simple (pPrint)
import Types

type instance IxValue (IM.IntervalMap k v) = v
type instance Index (IM.IntervalMap Validity v) = TimePoint

lookupTP :: TimePoint -> ValidityMap a -> Maybe (Validity, a)
lookupTP tp im = IM.lookupMin $ IM.containing im tp

instance Ixed (IM.IntervalMap Validity v) where
  ix tp handler im = case lookupTP tp im of
    Just (k, v) -> handler v <&> \v' -> IM.insert k v' im
    Nothing -> pure im

parentGroup :: FeatureID -> TimePoint -> Fold Validities GroupID
parentGroup fid tp =
  featureValidities . ix fid . parentValidities
    . to (lookupTP tp)
    . _Just
    . _2

parentFeature :: GroupID -> TimePoint -> Fold Validities FeatureID
parentFeature gid tp =
  groupValidities . ix gid . parentValidities
    . to (lookupTP tp)
    . _Just
    . _2

ancestors :: Either FeatureID GroupID -> TimePoint -> Validities -> [Either FeatureID GroupID]
ancestors (Left fid) _ vs | fid == _rootID vs = []
ancestors (Left fid) tp vs =
  let pgid = vs ^?! parentGroup fid tp
   in Right pgid : ancestors (Right pgid) tp vs
ancestors (Right gid) tp vs =
  let pfid = vs ^?! parentFeature gid tp
   in Left pfid : ancestors (Left pfid) tp vs

-- This is incorrect: in the recursive call, an must have the correct ancestors
hasCycles :: Either FeatureID GroupID -> Either FeatureID GroupID -> Validity -> Validities -> Bool
hasCycles n c (Validity tstart tend) vs =
  let an = ancestors n tstart vs
      ac = ancestors c tstart vs
      critical = c : (ac \\ an)
   in undefined

lookupFid :: FeatureID -> Validities -> Maybe FeatureValidity
lookupFid fid = M.lookup fid . _featureValidities

lookupGid :: GroupID -> Validities -> Maybe GroupValidity
lookupGid gid = M.lookup gid . _groupValidities

lookupNode :: Either FeatureID GroupID -> Validities -> Maybe (Either FeatureValidity GroupValidity)
lookupNode nid vs = either (fmap Left . (`lookupFid` vs)) (fmap Right . (`lookupGid` vs)) nid

nextMove :: HasParentValidities s (ValidityMap a) => TimePoint -> Fold s (Validity, a)
nextMove tp = parentValidities . to (lookupTP tp) . _Just

firstMove :: [Either FeatureID GroupID] -> TimePoint -> Validities -> Maybe (Either FeatureID GroupID, TimePoint, Either FeatureID GroupID)
firstMove xs tp validities = undefined
  where
    firstTP :: Maybe TimePoint
    firstTP =
      minimumOf
        ( folded
            . beside
              (to (`lookupFid` validities) . _Just . nextMove tp . _1 . end)
              (to (`lookupGid` validities) . _Just . nextMove tp . _1 . end)
        )
        xs

-- p 250
