{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Helpers where

import Control.Lens
import qualified Data.IntervalMap.Generic.Strict as IM
import Data.List ((\\))
import qualified Data.Map as M
import ExampleValidities hiding ((=:))
import Text.Pretty.Simple (pPrint)
import Types

-- type instance IxValue (IM.IntervalMap k v) = v
-- type instance Index (IM.IntervalMap Validity v) = TimePoint

lookupTP :: TimePoint -> ValidityMap a -> Maybe (Validity, a)
lookupTP tp im = IM.lookupMin $ IM.containing im tp

-- instance Ixed (IM.IntervalMap Validity v) where
--   ix tp handler im = case lookupTP tp im of
--     Just (k, v) -> handler v <&> \v' -> IM.insert k v' im
--     Nothing -> pure im

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
   in if n `elem` ac
        then True
        else case nextMove critical tstart vs of
          Nothing -> False
          Just (node, time, target) ->
            if time >= tend
              then False
              else
                if n `elem` ancestors target time vs
                  then True
                  else hasCycles n target (Validity time tend) vs

nextMove :: [Either FeatureID GroupID] -> TimePoint -> Validities -> Maybe (Either FeatureID GroupID, TimePoint, Either FeatureID GroupID)
nextMove = undefined
