module TreeSequence (toTreeSequence, treeAt, getTimePoints) where

import Control.Lens
import qualified Data.IntervalMap.Generic.Strict as IM
import Data.Maybe (fromJust, mapMaybe)
import Data.Monoid (Endo)
import qualified Data.Set as S
import Helpers
import Types

toSetOf :: Ord a => Getting (Endo [a]) s a -> s -> S.Set a
toSetOf l = S.fromList . toListOf l

toTreeSequence :: IntervalBasedFeatureModel -> TreeSequence
toTreeSequence ibfm =
  let tps = filter (/= Forever) . S.toAscList $ getTimePoints ibfm
   in zip tps $ treeAt ibfm <$> tps

treeAt :: IntervalBasedFeatureModel -> TimePoint -> FeatureModel
treeAt ibfm tp = FeatureModel . fromJust . convertFeature ibfm tp $ ibfm ^. rootID

convertFeature :: IntervalBasedFeatureModel -> TimePoint -> FeatureID -> Maybe Feature
convertFeature ibfm tp fid = do
  FeatureValidity _ names types _ childGroups <- ibfm ^? featureValidities . ix fid
  (_, name) <- lookupTP tp names
  (_, typ) <- lookupTP tp types
  let childIDs = foldMap S.toList . IM.elems $ IM.containing childGroups tp
  return . Feature fid name typ $ mapMaybe (convertGroup ibfm tp) childIDs

convertGroup :: IntervalBasedFeatureModel -> TimePoint -> GroupID -> Maybe Group
convertGroup ibfm tp gid = do
  GroupValidity _ types _ childFeatures <- ibfm ^? groupValidities . ix gid
  (_, typ) <- lookupTP tp types
  let childIDs = foldMap S.toList . IM.elems $ IM.containing childFeatures tp
  return . Group gid typ $ mapMaybe (convertFeature ibfm tp) childIDs

validityToList :: Validity -> [TimePoint]
validityToList (Validity s e) = [s, e]

getTimePoints :: IntervalBasedFeatureModel -> S.Set TimePoint
getTimePoints (IntervalBasedFeatureModel _ _ features groups) =
  toSetOf (folded . existenceValidities . timePoints) features
    <> toSetOf (folded . nameValidities . timePoints) features
    <> toSetOf (folded . typeValidities . timePoints) features
    <> toSetOf (folded . parentValidities . timePoints) features
    <> toSetOf (folded . existenceValidities . timePoints) groups
    <> toSetOf (folded . typeValidities . timePoints) groups
    <> toSetOf (folded . parentValidities . timePoints) groups
  where
    timePoints = to IM.keys . folded . to validityToList . folded
