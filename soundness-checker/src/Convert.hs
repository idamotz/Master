module Convert where

import Control.Lens
import Control.Monad.State
import Data.Foldable (traverse_)
import Data.IntervalMap.Generic.Strict ((!))
import qualified Data.IntervalMap.Generic.Strict as IM
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Helpers
import Types

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
applyOperation (TimeOperation tp (RemoveFeature fid)) = \tfm ->
  let (FeatureValidity fe fn ft fp fc) = tfm ^?! featureValidities . ix fid
      Just (_, name) = lookupTP tp fn
      Just (_, parentID) = lookupTP tp fp
   in tfm
        & nameValidities . ix name %~ clampIntervalEnd tp
        & featureValidities . ix fid
        .~ ( FeatureValidity
              (clampIntervalEnd tp fe)
              (clampIntervalEnd tp fn)
              (clampIntervalEnd tp ft)
              (clampIntervalEnd tp fp)
              fc
           )
          & groupValidities . ix parentID . childValidities %~ clampIntervalEndValue tp fid
applyOperation (TimeOperation tp (RemoveGroup gid)) = \tfm ->
  let (GroupValidity ge gt gp gc) = tfm ^?! groupValidities . ix gid
      Just (_, parentID) = lookupTP tp gp
   in tfm
        & groupValidities . ix gid
        .~ ( GroupValidity
              (clampIntervalEnd tp ge)
              (clampIntervalEnd tp gt)
              (clampIntervalEnd tp gp)
              gc
           )
          & featureValidities . ix parentID . childValidities %~ clampIntervalEndValue tp gid
applyOperation (TimeOperation tp (MoveFeature fid newParent)) = \tfm ->
  let Just (Validity _ e, oldParent) = tfm ^?! featureValidities . ix fid . parentValidities . to (lookupTP tp)
   in tfm
        & groupValidities . ix oldParent . childValidities
        %~ clampIntervalEndValue tp fid
        & groupValidities . ix newParent . childValidities
        %~ insertSingleton (Validity tp e) fid
        & featureValidities . ix fid . parentValidities
        %~ clampIntervalEnd tp
          . IM.insert (Validity tp e) newParent
applyOperation (TimeOperation tp (ChangeFeatureType fid newType)) = \tfm ->
  let types = tfm ^?! featureValidities . ix fid . typeValidities
      Just (containingKey@(Validity s e), oldType) = lookupTP tp types
   in tfm
        & featureValidities . ix fid . typeValidities
        %~ IM.insert (Validity tp e) newType
          . IM.insert (Validity s tp) oldType
          . IM.delete containingKey
applyOperation (TimeOperation tp (ChangeGroupType gid newType)) = \tfm ->
  let types = tfm ^?! groupValidities . ix gid . typeValidities
      Just (containingKey@(Validity s e), oldType) = lookupTP tp types
   in tfm
        & groupValidities . ix gid . typeValidities
        %~ IM.insert (Validity tp e) newType
          . IM.insert (Validity s tp) oldType
          . IM.delete containingKey
applyOperation (TimeOperation tp (ChangeFeatureName fid newName)) = \tfm ->
  let Just ((Validity _ e), oldName) = tfm ^?! featureValidities . ix fid . nameValidities . to (lookupTP tp)
   in tfm
        & nameValidities . ix oldName
          %~ clampIntervalEnd tp
        & id
          %~ insertName newName (Validity tp e) fid
        & featureValidities . ix fid . nameValidities
          %~ clampIntervalEnd tp . IM.insert (Validity tp e) newName
applyOperation (AddOperation _ op) = error $ "Add operation can not be used with " ++ show op
applyOperation (TimeOperation _ op) = error $ "Time operation can not be used with " ++ show op

clampIntervalEnd :: TimePoint -> ValidityMap a -> ValidityMap a
clampIntervalEnd tp vm =
  let Just (containingKey@(Validity s _), val) = lookupTP tp vm
   in IM.insert (Validity s tp) val
        . IM.delete containingKey
        $ vm

clampIntervalEndValue :: Ord a => TimePoint -> a -> ValidityMap (S.Set a) -> ValidityMap (S.Set a)
clampIntervalEndValue tp v vmap =
  let Just ((Validity s e), containingSet) = containingTPVal tp v vmap
   in insertSingleton (Validity s tp) v
        . deleteIfEmpty (Validity s e)
        . IM.insert (Validity s e) (S.delete v containingSet)
        $ vmap
  where
    deleteIfEmpty :: Validity -> ValidityMap (S.Set a) -> ValidityMap (S.Set a)
    deleteIfEmpty k vm = if null (vm ! k) then IM.delete k vm else vm

insertEmptyFeature :: FeatureID -> TemporalFeatureModel -> TemporalFeatureModel
insertEmptyFeature fid =
  over featureValidities $ M.insertWith (const id) fid (FeatureValidity mempty mempty mempty mempty mempty)

insertEmptyGroup :: GroupID -> TemporalFeatureModel -> TemporalFeatureModel
insertEmptyGroup gid =
  over groupValidities $ M.insertWith (const id) gid (GroupValidity mempty mempty mempty mempty)

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
