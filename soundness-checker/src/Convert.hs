module Convert where

import Control.Lens
import Control.Monad.State
import Data.Foldable (traverse_)
import qualified Data.IntervalMap.Generic.Strict as IM
import qualified Data.Map as M
import qualified Data.Set as S
import Helpers
import Types

u = undefined

type ValidityState = State TemporalFeatureModel

convert :: EvolutionPlan -> TemporalFeatureModel
convert (EvolutionPlan initModel startTime operations) =
  foldl (flip applyOperation) (convertInitModel initModel startTime) (operations)

insertSingleton :: Ord a => Validity -> a -> ValidityMap (S.Set a) -> ValidityMap (S.Set a)
insertSingleton validity x = IM.insertWith (<>) validity (S.singleton x)

convertInitModel :: FeatureModel -> TimePoint -> TemporalFeatureModel
convertInitModel (FeatureModel r) tp = execState (convertFeature tp Nothing r) (TemporalFeatureModel (_featureID r) M.empty M.empty M.empty)

insertName :: Name -> Validity -> FeatureID -> TemporalFeatureModel -> TemporalFeatureModel
insertName name validity fid =
  over nameValidities $
    M.insertWith (const $ IM.insert validity fid) name $ IM.singleton validity fid

applyOperation :: TimeOperation -> TemporalFeatureModel -> TemporalFeatureModel
applyOperation (AddOperation validity (AddFeature fid name fType parentID)) =
  insertName name validity fid
    . over
      (groupValidities . ix parentID . childValidities)
      (insertSingleton validity fid)
    . over
      (featureValidities . ix fid)
      ( \feature ->
          feature
            & existenceValidities %~ IM.insert validity ()
            & nameValidities %~ IM.insert validity name
            & typeValidities %~ IM.insert validity fType
            & parentValidities %~ IM.insert validity parentID
      )
    . insertEmptyFeature fid
applyOperation (AddOperation validity (AddGroup gid gType parentID)) =
  over
    (featureValidities . ix parentID . childValidities)
    (insertSingleton validity gid)
    . over
      (groupValidities . ix gid)
      ( \group ->
          group
            & existenceValidities %~ IM.insert validity ()
            & typeValidities %~ IM.insert validity gType
            & parentValidities %~ IM.insert validity parentID
      )
    . insertEmptyGroup gid
applyOperation (TimeOperation tp (ChangeGroupType gid newType)) = \tfm ->
  let types = tfm ^?! groupValidities . ix gid . typeValidities
      Just (containingKey@(Validity s e), oldType) = lookupTP tp types
   in tfm
        & groupValidities . ix gid . typeValidities
        %~ IM.insert (Validity tp e) newType
          . IM.insert (Validity s tp) oldType
          . IM.delete containingKey
applyOperation _ = error "Implement!"
applyOperation (AddOperation _ op) = error $ "Add operation can not be used with " ++ show op
applyOperation (TimeOperation _ op) = error $ "Time operation can not be used with " ++ show op

-- data TimeOperation = AddOperation Validity Operation | TimeOperation TimePoint Operation deriving (Show, Eq)

-- data Operation
--   = AddFeature FeatureID Name FeatureType GroupID
--   | AddGroup GroupID GroupType FeatureID
--   | RemoveFeature FeatureID
--   | RemoveGroup GroupID
--   | MoveFeature FeatureID GroupID
--   | MoveGroup GroupID FeatureID
--   | ChangeFeatureType FeatureID FeatureType
--   | ChangeGroupType GroupID GroupType
--   | ChangeFeatureName FeatureID Name
--   deriving (Show, Eq)

insertEmptyFeature :: FeatureID -> TemporalFeatureModel -> TemporalFeatureModel
insertEmptyFeature fid =
  over featureValidities $ M.insertWith (const id) fid (FeatureValidity mempty mempty mempty mempty mempty)

insertEmptyGroup :: GroupID -> TemporalFeatureModel -> TemporalFeatureModel
insertEmptyGroup gid =
  over groupValidities $ M.insertWith (const id) gid (GroupValidity mempty mempty mempty mempty)

-- data FeatureValidity = FeatureValidity
--   { _existenceValidities :: ValidityMap ()
--   , _nameValidities :: ValidityMap Name
--   , _typeValidities :: ValidityMap FeatureType
--   , _parentValidities :: ValidityMap GroupID
--   , _childValidities :: ValidityMap (S.Set GroupID)
--   }
--   deriving (Show, Eq)

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

-- data Feature = Feature
--   { _featureID :: FeatureID
--   , _name :: Name
--   , _varType :: FeatureType
--   , _parent :: Maybe GroupID
--   , _childGroups :: [Group]
--   }
--   deriving (Show, Eq)
