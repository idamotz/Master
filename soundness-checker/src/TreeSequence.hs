module TreeSequence where

import Control.Lens
import qualified Data.IntervalMap.Generic.Strict as IM
import Data.Maybe (fromJust, mapMaybe)
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
treeAt tfm tp = FeatureModel . fromJust $ convertFeature tfm tp (tfm ^. rootID)

convertFeature :: TemporalFeatureModel -> TimePoint -> FeatureID -> Maybe Feature
convertFeature tfm tp fid = do
  FeatureValidity e n t p c <- tfm ^? featureValidities . ix fid
  (_, name) <- lookupTP tp n
  (_, typ) <- lookupTP tp t
  let childIDs = foldMap S.toList . IM.elems $ IM.containing c tp
  let children = mapMaybe (convertGroup tfm tp) childIDs
  return $ Feature fid name typ children

convertGroup :: TemporalFeatureModel -> TimePoint -> GroupID -> Maybe Group
convertGroup tfm tp gid = do
  GroupValidity e t p c <- tfm ^? groupValidities . ix gid
  (_, typ) <- lookupTP tp t
  let childIDs = foldMap S.toList . IM.elems $ IM.containing c tp
  let children = mapMaybe (convertFeature tfm tp) childIDs
  return $ Group gid typ children

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
