{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Helpers where

import Control.Lens
import qualified Data.IntervalMap.Generic.Strict as IM
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import Safe.Foldable (minimumByMay)
import Types

infix 5 \\
(\\) :: Eq a => [a] -> [a] -> [a]
xs \\ ys = helper (reverse xs) (reverse ys)
  where
    helper (x : xs) (y : ys) | x == y = helper xs ys
    helper xs _ = reverse xs

type instance IxValue (IM.IntervalMap k v) = v
type instance Index (IM.IntervalMap Validity v) = TimePoint

type Node = Either FeatureValidity GroupValidity
type NodeID = Either FeatureID GroupID

lookupTP :: TimePoint -> ValidityMap a -> Maybe (Validity, a)
lookupTP tp im = IM.lookupMin $ IM.containing im tp

instance Ixed (IM.IntervalMap Validity v) where
  ix tp handler im = case lookupTP tp im of
    Just (k, v) -> handler v <&> \v' -> IM.insert k v' im
    Nothing -> pure im

parentGroup :: FeatureID -> TimePoint -> Fold TemporalFeatureModel GroupID
parentGroup fid tp =
  featureValidities . ix fid . parentValidities
    . to (lookupTP tp)
    . _Just
    . _2

parentFeature :: GroupID -> TimePoint -> Fold TemporalFeatureModel FeatureID
parentFeature gid tp =
  groupValidities . ix gid . parentValidities
    . to (lookupTP tp)
    . _Just
    . _2

ancestors :: NodeID -> TimePoint -> TemporalFeatureModel -> [NodeID]
ancestors (Left fid) _ vs | fid == _rootID vs = []
ancestors (Left fid) tp vs =
  let pgid = vs ^?! parentGroup fid tp
   in Right pgid : ancestors (Right pgid) tp vs
ancestors (Right gid) tp vs =
  let pfid = vs ^?! parentFeature gid tp
   in Left pfid : ancestors (Left pfid) tp vs

hasCycles :: NodeID -> NodeID -> Validity -> TemporalFeatureModel -> Bool
hasCycles n c interval@(Validity tstart _) vs =
  let an = ancestors n tstart vs
      ac = ancestors c tstart vs
      critical = c : (ac \\ an)
   in n `elem` ac || hasCriticalMoves critical interval
  where
    hasCriticalMoves :: [NodeID] -> Validity -> Bool
    hasCriticalMoves critical interval@(Validity _ tend) =
      case firstMove critical interval vs of
        Nothing -> False
        Just (moveTime, moved, target) ->
          let targetAncestors = target : ancestors target moveTime vs
              an = ancestors n moveTime vs
           in n `elem` targetAncestors
                || hasCriticalMoves
                  (takeWhile (/= target) critical ++ (targetAncestors \\ an))
                  (Validity moveTime tend)

lookupFid :: FeatureID -> TemporalFeatureModel -> Maybe FeatureValidity
lookupFid fid = M.lookup fid . _featureValidities

lookupGid :: GroupID -> TemporalFeatureModel -> Maybe GroupValidity
lookupGid gid = M.lookup gid . _groupValidities

lookupNode :: Either FeatureID GroupID -> TemporalFeatureModel -> Maybe (Either FeatureValidity GroupValidity)
lookupNode nid vs = either (fmap Left . (`lookupFid` vs)) (fmap Right . (`lookupGid` vs)) nid

nextMove :: HasParentValidities s (ValidityMap a) => TimePoint -> Fold s (Validity, a)
nextMove tp = parentValidities . to (lookupTP tp) . _Just

firstMove :: [NodeID] -> Validity -> TemporalFeatureModel -> Maybe (TimePoint, NodeID, NodeID)
firstMove xs (Validity tstart tend) featureModel =
  minimumByMay (comparing $ view _1) $ mapMaybe getMove xs
  where
    getMove :: NodeID -> Maybe (TimePoint, NodeID, NodeID)
    getMove (Left fid) = do
      feature <- lookupFid fid featureModel
      (Validity _ e, _) <- lookupTP tstart $ feature ^. parentValidities
      (Validity _ _, target) <- lookupTP e $ feature ^. parentValidities
      if e >= tend
        then Nothing
        else Just (e, Left fid, Right target)
    getMove (Right gid) = do
      group <- lookupGid gid featureModel
      (Validity _ e, _) <- lookupTP tstart $ group ^. parentValidities
      (Validity _ _, target) <- lookupTP e $ group ^. parentValidities
      if e >= tend
        then Nothing
        else Just (e, Right gid, Left target)

-- p 250
