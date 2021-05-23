{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Helpers where

import Control.Lens
import Data.IntervalMap.Generic.Strict ((!))
import qualified Data.IntervalMap.Generic.Strict as IM
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import qualified Data.Set as S
import Safe.Foldable (minimumByMay)
import Types

infix 6 \\
(\\) :: Eq a => [a] -> [a] -> [a]
xs \\ ys = helper (reverse xs) (reverse ys)
  where
    helper (x : xs) (y : ys) | x == y = helper xs ys
    helper xs _ = reverse xs

type instance IxValue (IM.IntervalMap k v) = v
type instance Index (IM.IntervalMap Validity v) = TimePoint

instance Ixed (IM.IntervalMap Validity v) where
  ix tp handler im = case lookupTP tp im of
    Just (k, v) -> handler v <&> \v' -> insert k v' im
    Nothing -> pure im

type Node = Either FeatureValidity GroupValidity
type NodeID = Either FeatureID GroupID

-----------------
-- ValidityMap --
-----------------

lookupTP :: TimePoint -> ValidityMap a -> Maybe (Validity, a)
lookupTP tp im = IM.lookupMin $ IM.containing im tp

containingIntervalTP :: TimePoint -> ValidityMap a -> Maybe Validity
containingIntervalTP tp = fmap fst . lookupTP tp

containingInterval :: Validity -> ValidityMap a -> ValidityMap a
containingInterval validity =
  IM.filterWithKey (\k _ -> k `IM.subsumes` validity)
    . (`IM.intersecting` validity)

containingTPVal :: Ord a => TimePoint -> a -> ValidityMap (S.Set a) -> Maybe (Validity, S.Set a)
containingTPVal tp v = IM.lookupMin . IM.filter (S.member v) . (`IM.containing` tp)

lookupOverlappingValues :: Validity -> ValidityMap a -> [a]
lookupOverlappingValues validity =
  IM.elems
    . flip IM.intersecting validity

lookupOverlapping :: Validity -> ValidityMap a -> [(Validity, a)]
lookupOverlapping validity =
  IM.assocs . flip IM.intersecting validity

-- Do not insert if empty interval
insert :: Validity -> a -> ValidityMap a -> ValidityMap a
insert (Validity s e) | s >= e = const id
insert iv = IM.insert iv

insertWith :: (a -> a -> a) -> Validity -> a -> ValidityMap a -> ValidityMap a
insertWith _ (Validity s e) | s >= e = const id
insertWith f iv = IM.insertWith f iv

---------------
-- Intervals --
---------------
validityOverlap :: Validity -> Validity -> Validity
validityOverlap (Validity s1 e1) (Validity s2 e2) = Validity (max s1 s2) (min e1 e2)

---------------------------------
-- IntervalBasedFeatureModel lookup --
---------------------------------

lookupFeature :: FeatureID -> IntervalBasedFeatureModel -> Maybe FeatureValidity
lookupFeature fid = M.lookup fid . _featureValidities

lookupGroup :: GroupID -> IntervalBasedFeatureModel -> Maybe GroupValidity
lookupGroup gid = M.lookup gid . _groupValidities

lookupFeatureDefault :: FeatureID -> IntervalBasedFeatureModel -> FeatureValidity
lookupFeatureDefault fid = M.findWithDefault (FeatureValidity mempty mempty mempty mempty mempty) fid . view featureValidities

lookupGroupDefault :: GroupID -> IntervalBasedFeatureModel -> GroupValidity
lookupGroupDefault gid = M.findWithDefault (GroupValidity mempty mempty mempty mempty) gid . view groupValidities

lookupNameDefault :: Name -> IntervalBasedFeatureModel -> ValidityMap FeatureID
lookupNameDefault name = M.findWithDefault mempty name . view nameValidities

lookupNode :: Either FeatureID GroupID -> IntervalBasedFeatureModel -> Maybe (Either FeatureValidity GroupValidity)
lookupNode nid vs = either (fmap Left . (`lookupFeature` vs)) (fmap Right . (`lookupGroup` vs)) nid

parentGroup :: FeatureID -> TimePoint -> Fold IntervalBasedFeatureModel GroupID
parentGroup fid tp =
  featureValidities . ix fid . parentValidities
    . to (lookupTP tp)
    . _Just
    . _2

parentFeature :: GroupID -> TimePoint -> Fold IntervalBasedFeatureModel FeatureID
parentFeature gid tp =
  groupValidities . ix gid . parentValidities
    . to (lookupTP tp)
    . _Just
    . _2

compatibleTypes :: GroupType -> FeatureType -> Bool
compatibleTypes And _ = True
compatibleTypes _ Optional = True
compatibleTypes _ _ = False

lookupNameInterval :: Name -> Validity -> IntervalBasedFeatureModel -> ValidityMap FeatureID
lookupNameInterval name validity =
  maybe mempty (`IM.intersecting` validity)
    . M.lookup name
    . view nameValidities

---------------------------------------
-- IntervalBasedFeatureModel modification --
---------------------------------------

insertSingleton :: Ord a => Validity -> a -> ValidityMap (S.Set a) -> ValidityMap (S.Set a)
insertSingleton validity x = insertWith (<>) validity (S.singleton x)

insertName :: Name -> Validity -> FeatureID -> IntervalBasedFeatureModel -> IntervalBasedFeatureModel
insertName name validity fid =
  over nameValidities $
    M.insertWith (const $ insert validity fid) name $ IM.singleton validity fid

insertEmptyFeature :: FeatureID -> IntervalBasedFeatureModel -> IntervalBasedFeatureModel
insertEmptyFeature fid =
  over featureValidities $ M.insertWith (const id) fid (FeatureValidity mempty mempty mempty mempty mempty)

insertEmptyGroup :: GroupID -> IntervalBasedFeatureModel -> IntervalBasedFeatureModel
insertEmptyGroup gid =
  over groupValidities $ M.insertWith (const id) gid (GroupValidity mempty mempty mempty mempty)

clampIntervalEnd :: TimePoint -> ValidityMap a -> ValidityMap a
clampIntervalEnd tp vm =
  let Just (containingKey@(Validity s _), val) = lookupTP tp vm
   in insert (Validity s tp) val
        . IM.delete containingKey
        $ vm

clampIntervalEndValue :: Ord a => TimePoint -> a -> ValidityMap (S.Set a) -> ValidityMap (S.Set a)
clampIntervalEndValue tp v vmap =
  let Just (Validity s e, containingSet) = containingTPVal tp v vmap
   in insertSingleton (Validity s tp) v
        . deleteIfEmpty (Validity s e)
        . insert (Validity s e) (S.delete v containingSet)
        $ vmap
  where
    deleteIfEmpty :: Validity -> ValidityMap (S.Set a) -> ValidityMap (S.Set a)
    deleteIfEmpty k vm = if null (vm ! k) then IM.delete k vm else vm

deleteNameIfNull :: Name -> IntervalBasedFeatureModel -> IntervalBasedFeatureModel
deleteNameIfNull name ibfm =
  ibfm & nameValidities . at name
    %~ (\(Just im) -> if null im then Nothing else Just im)

deleteFeatureIfNull :: FeatureID -> IntervalBasedFeatureModel -> IntervalBasedFeatureModel
deleteFeatureIfNull fid ibfm =
  ibfm & featureValidities . at fid
    %~ \f@(Just (FeatureValidity fe _ _ _ _)) -> if null fe then Nothing else f

deleteGroupIfNull :: GroupID -> IntervalBasedFeatureModel -> IntervalBasedFeatureModel
deleteGroupIfNull gid ibfm =
  ibfm & groupValidities . at gid
    %~ \g@(Just (GroupValidity ge _ _ _)) -> if null ge then Nothing else g

--------------------
-- Move algorithm --
--------------------

ancestors :: NodeID -> TimePoint -> IntervalBasedFeatureModel -> [NodeID]
ancestors (Left fid) _ vs | fid == _rootID vs = []
ancestors (Left fid) tp vs =
  let pgid = vs ^?! parentGroup fid tp
   in Right pgid : ancestors (Right pgid) tp vs
ancestors (Right gid) tp vs =
  let pfid = vs ^?! parentFeature gid tp
   in Left pfid : ancestors (Left pfid) tp vs

hasCycles :: NodeID -> NodeID -> Validity -> IntervalBasedFeatureModel -> Bool
hasCycles n c interval@(Validity tstart _) vs =
  let an = ancestors n tstart vs
      ac = ancestors c tstart vs
      critical = c : ac \\ an
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

nextMove :: HasParentValidities s (ValidityMap a) => TimePoint -> Fold s (Validity, a)
nextMove tp = parentValidities . to (lookupTP tp) . _Just

firstMove :: [NodeID] -> Validity -> IntervalBasedFeatureModel -> Maybe (TimePoint, NodeID, NodeID)
firstMove xs (Validity tstart tend) featureModel =
  minimumByMay (comparing $ view _1) $ mapMaybe getMove xs
  where
    getMove :: NodeID -> Maybe (TimePoint, NodeID, NodeID)
    getMove (Left fid) = do
      feature <- lookupFeature fid featureModel
      (Validity _ e, _) <- lookupTP tstart $ feature ^. parentValidities
      (Validity _ _, target) <- lookupTP e $ feature ^. parentValidities
      if e >= tend
        then Nothing
        else Just (e, Left fid, Right target)
    getMove (Right gid) = do
      group <- lookupGroup gid featureModel
      (Validity _ e, _) <- lookupTP tstart $ group ^. parentValidities
      (Validity _ _, target) <- lookupTP e $ group ^. parentValidities
      if e >= tend
        then Nothing
        else Just (e, Right gid, Left target)
