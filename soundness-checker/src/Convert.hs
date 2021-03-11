module Convert (convert) where

import Apply
import Control.Lens
import Control.Monad.State
import Data.Foldable (traverse_)
import qualified Data.IntervalMap.Generic.Strict as IM
import qualified Data.Map.Strict as M
import Helpers
import Types

convert :: EvolutionPlan -> TemporalFeatureModel
convert (EvolutionPlan initModel startTime operations) =
  foldl
    (flip apply)
    (convertInitModel initModel startTime)
    operations

convertInitModel :: FeatureModel -> TimePoint -> TemporalFeatureModel
convertInitModel (FeatureModel r) tp = execState (convertFeature tp Nothing r) (TemporalFeatureModel (_featureID r) M.empty M.empty M.empty)

convertFeature :: TimePoint -> Maybe GroupID -> Feature -> State TemporalFeatureModel ()
convertFeature tp mParentID (Feature fid name ftype children) = do
  modify $ insertEmptyFeature fid
  let validity = Validity tp Forever
  modify $ insertName name validity fid
  featureValidities . ix fid %= \feature ->
    feature
      & existenceValidities %~ IM.insert validity ()
      & nameValidities %~ IM.insert validity name
      & typeValidities %~ IM.insert validity ftype
  case mParentID of
    Just parentID -> do
      featureValidities . ix fid . parentValidities %= IM.insert validity parentID
      modify $ insertEmptyGroup parentID
      groupValidities . ix parentID . childValidities
        %= insertSingleton validity fid
    _ -> return ()
  traverse_ (convertGroup tp fid) children

convertGroup :: TimePoint -> FeatureID -> Group -> State TemporalFeatureModel ()
convertGroup tp parentID (Group gid gtype children) = do
  modify $ insertEmptyGroup gid
  let validity = Validity tp Forever
  groupValidities . ix gid %= \group ->
    group
      & existenceValidities %~ IM.insert validity ()
      & typeValidities %~ IM.insert validity gtype
      & parentValidities %~ IM.insert validity parentID
  modify $ insertEmptyFeature parentID
  featureValidities . ix parentID . childValidities %= insertSingleton validity gid
  traverse_ (convertFeature tp (Just gid)) children
