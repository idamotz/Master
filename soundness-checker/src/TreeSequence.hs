module TreeSequence where

import Control.Lens
import Control.Monad.State.Lazy
import qualified Data.IntervalMap.Generic.Strict as IM
import Data.Maybe (catMaybes, fromJust, isNothing)
import Data.Monoid (Endo)
import qualified Data.Set as S
import Helpers
import Types

toSetOf :: Ord a => Getting (Endo [a]) s a -> s -> S.Set a
toSetOf l = S.fromList . toListOf l

toTreeSequence :: TemporalFeatureModel -> TreeSequence
toTreeSequence tfm =
  let tps = filter (/= Forever) $ S.toAscList $ getTimePoints tfm
   in zip tps $ treeAt tfm <$> tps

treeAt :: TemporalFeatureModel -> TimePoint -> FeatureModel
treeAt tfm tp = FeatureModel . fromJust $ evalState (convertFeature tp (tfm ^. rootID)) tfm

convertFeature :: TimePoint -> FeatureID -> State TemporalFeatureModel (Maybe Feature)
convertFeature tp fid = do
  FeatureValidity e n t p c <- fmap fromJust . gets . preview $ featureValidities . ix fid
  if isNothing $ containingInterval tp e
    then return Nothing
    else do
      let Just (_, name) = lookupTP tp n
      let Just (_, typ) = lookupTP tp t
      let childIDs = foldMap S.toList . IM.elems $ IM.containing c tp
      children <- traverse (convertGroup tp) childIDs
      return . Just $ Feature fid name typ (catMaybes children)

convertGroup :: TimePoint -> GroupID -> State TemporalFeatureModel (Maybe Group)
convertGroup tp gid = do
  GroupValidity e t p c <- fmap fromJust . gets . preview $ groupValidities . ix gid
  if isNothing $ containingInterval tp e
    then return Nothing
    else do
      let Just (_, typ) = lookupTP tp t
      let childIDs = foldMap S.toList . IM.elems $ IM.containing c tp
      children <- traverse (convertFeature tp) childIDs
      return . Just $ Group gid typ (catMaybes children)

validityToList :: Validity -> [TimePoint]
validityToList (Validity s e) = [s, e]

getTimePoints :: TemporalFeatureModel -> S.Set TimePoint
getTimePoints (TemporalFeatureModel _ _ features groups) =
  toSetOf (folded . existenceValidities . timePoints) features
    <> toSetOf (folded . nameValidities . timePoints) features
    <> toSetOf (folded . typeValidities . timePoints) features
    <> toSetOf (folded . parentValidities . timePoints) features
    <> toSetOf (folded . existenceValidities . timePoints) groups
    <> toSetOf (folded . typeValidities . timePoints) groups
    <> toSetOf (folded . parentValidities . timePoints) groups
  where
    timePoints = to IM.keys . folded . to validityToList . folded
