module Validate where

import Control.Lens
import Data.Foldable (toList)
import qualified Data.IntervalMap.Generic.Strict as IM
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Set as S
import Helpers
import Types

checkCompatibleTypesFeature :: Validity -> FeatureType -> ValidityMap GroupType -> Maybe ValidationError
checkCompatibleTypesFeature validity ftype gtypes =
  (\(interval, gtype) -> IncompatibleTypes ftype gtype interval)
    <$> ( IM.lookupMin
            . IM.filter (`compatibleTypes` ftype)
            . (`IM.intersecting` validity)
            $ gtypes
        )

checkCompatibleTypesGroup :: Validity -> GroupType -> ValidityMap FeatureType -> Maybe ValidationError
checkCompatibleTypesGroup validity gType fTypes =
  (\(interval, fType) -> IncompatibleTypes fType gType interval)
    <$> ( IM.lookupMin
            . IM.filter (compatibleTypes gType)
            . (`IM.intersecting` validity)
            $ fTypes
        )

checkNodeNotExists :: Validity -> ValidityMap () -> Maybe ValidationError
checkNodeNotExists validity vm = NodeAlreadyExists . fst <$> IM.lookupMin (IM.intersecting vm validity)

checkParentExists :: Validity -> ValidityMap () -> Maybe ValidationError
checkParentExists validity validityMap
  | not . null $ IM.within validityMap validity = Nothing
  | otherwise = Just ParentNotExists

checkNameInUse :: Validity -> Name -> TemporalFeatureModel -> Maybe ValidationError
checkNameInUse validity name = fmap (uncurry NameInUse) . IM.lookupMin . lookupNameInterval name validity

checkNotChanged :: Validity -> ValidityMap a -> (a -> ChangeOperation) -> Maybe ValidationError
checkNotChanged (Validity s e) vm op =
  case containingInterval s vm of
    Just (Validity s' e') ->
      if e' /= e
        then ChangePlanned e' . op . snd <$> lookupTP e' vm
        else Nothing
    Nothing -> Just NodeNotExists -- but this should already have been caught

checkNoChildren :: Validity -> ValidityMap a -> Maybe ValidationError
checkNoChildren v vm =
  let keys = IM.keys . (`IM.intersecting` v) $ vm
   in if null keys
        then Nothing
        else Just . HasChildren $ foldl1 (<>) keys

checkFeatureCycles :: Validity -> FeatureID -> GroupID -> TemporalFeatureModel -> Maybe ValidationError
checkFeatureCycles v fid gid tfm
  | hasCycles (Left fid) (Right gid) v tfm = Just CreatesCycle
  | otherwise = Nothing

checkGroupCycles :: Validity -> GroupID -> FeatureID -> TemporalFeatureModel -> Maybe ValidationError
checkGroupCycles v gid fid tfm
  | hasCycles (Right gid) (Left fid) v tfm = Just CreatesCycle
  | otherwise = Nothing

validate :: TimeOperation -> TemporalFeatureModel -> [ValidationError]
validate (AddOperation v (AddFeature fid name ftype gid)) tfm =
  let (FeatureValidity existence _ _ _ _) = lookupFeatureDefault fid tfm
      (GroupValidity groupExistence groupTypes _ _) = lookupGroupDefault gid tfm
   in catMaybes
        [ checkNodeNotExists v existence
        , checkParentExists v groupExistence
        , checkNameInUse v name tfm
        , checkCompatibleTypesFeature v ftype groupTypes
        ]
validate (AddOperation v (AddGroup gid gtype fid)) tfm =
  catMaybes
    [ checkNodeNotExists v . view existenceValidities $ lookupGroupDefault gid tfm
    , checkParentExists v . view existenceValidities $ lookupFeatureDefault fid tfm
    ]
validate (ChangeOperation tp (RemoveFeature fid)) tfm =
  let (FeatureValidity existence names types parents children) = lookupFeatureDefault fid tfm
   in case containingInterval tp existence of
        Nothing -> [NodeNotExists]
        Just (Validity _ endTime) ->
          let scope = Validity tp endTime
           in catMaybes
                [ checkNotChanged scope names $ ChangeFeatureName fid
                , checkNotChanged scope types $ ChangeFeatureType fid
                , checkNotChanged scope parents $ MoveFeature fid
                , checkNoChildren scope children
                ]
validate (ChangeOperation tp (RemoveGroup gid)) tfm =
  let (GroupValidity existence types parents children) = lookupGroupDefault gid tfm
   in case containingInterval tp existence of
        Nothing -> [NodeNotExists]
        Just (Validity _ endTime) ->
          let scope = Validity tp endTime
           in catMaybes
                [ checkNotChanged scope types $ ChangeGroupType gid
                , checkNotChanged scope parents $ MoveGroup gid
                , checkNoChildren scope children
                ]
validate (ChangeOperation tp (MoveFeature fid gid)) tfm =
  let (FeatureValidity existence _ types _ _) = lookupFeatureDefault fid tfm
      (GroupValidity groupExistence groupTypes _ _) = lookupGroupDefault gid tfm
   in case (containingInterval tp existence, containingInterval tp groupExistence) of
        (Nothing, Nothing) -> [NodeNotExists, ParentNotExists]
        (_, Nothing) -> [ParentNotExists]
        (Nothing, _) -> [NodeNotExists]
        (Just (Validity _ endTime), Just (Validity pStart pEnd)) ->
          if pEnd < endTime || pStart > tp
            then [ParentNotExists]
            else
              let scope = Validity tp endTime
               in catMaybes $
                    ( flip (checkCompatibleTypesFeature scope) groupTypes
                        <$> lookupOverlappingValues scope types
                    )
                      ++ [checkFeatureCycles scope fid gid tfm]
validate (ChangeOperation tp (MoveGroup gid fid)) tfm =
  let (GroupValidity existence _ _ _) = lookupGroupDefault gid tfm
      (FeatureValidity featureExistence _ _ _ _) = lookupFeatureDefault fid tfm
   in case (containingInterval tp existence, containingInterval tp featureExistence) of
        (Nothing, Nothing) -> [NodeNotExists, ParentNotExists]
        (_, Nothing) -> [ParentNotExists]
        (Nothing, _) -> [NodeNotExists]
        (Just (Validity _ endTime), Just (Validity pStart pEnd)) ->
          if pEnd < endTime || pStart > tp
            then [ParentNotExists]
            else
              let scope = Validity tp endTime
               in toList $ checkGroupCycles scope gid fid tfm
validate (ChangeOperation tp (ChangeFeatureType fid fType)) tfm =
  let (FeatureValidity _ _ types parents _) = lookupFeatureDefault fid tfm
   in case containingInterval tp types of
        Nothing -> [NodeNotExists]
        Just (Validity _ endTime) ->
          let scope = Validity tp endTime
           in mapMaybe
                ( \(v, gid) ->
                    tfm ^? groupValidities . ix gid . typeValidities
                      >>= checkCompatibleTypesFeature (validityOverlap v scope) fType
                )
                . IM.assocs
                . (`IM.intersecting` scope)
                $ parents
validate (ChangeOperation tp (ChangeGroupType gid gType)) tfm =
  let (GroupValidity _ types _ children) = lookupGroupDefault gid tfm
   in case containingInterval tp types of
        Nothing -> [NodeNotExists]
        Just (Validity _ endTime) ->
          let scope = Validity tp endTime
           in concatMap
                ( \(v, childIDs) ->
                    mapMaybe
                      ( \fid ->
                          tfm ^? featureValidities . ix fid . typeValidities
                            >>= checkCompatibleTypesGroup (validityOverlap v scope) gType
                      )
                      (S.toList childIDs)
                )
                . IM.assocs
                . (`IM.intersecting` scope)
                $ children
validate (ChangeOperation tp (ChangeFeatureName fid name)) tfm =
  let (FeatureValidity _ names _ _ _) = lookupFeatureDefault fid tfm
   in case containingInterval tp names of
        Nothing -> [NodeNotExists]
        Just (Validity _ endTime) ->
          let scope = Validity tp endTime
              conflictingUses = IM.assocs . flip IM.intersecting scope $ lookupNameDefault name tfm
           in if null conflictingUses
                then []
                else uncurry NameInUse <$> conflictingUses
