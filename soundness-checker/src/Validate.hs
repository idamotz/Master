module Validate (validate) where

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

checkNameInUse :: Validity -> Name -> IntervalBasedFeatureModel -> Maybe ValidationError
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

checkFeatureCycles :: Validity -> FeatureID -> GroupID -> IntervalBasedFeatureModel -> Maybe ValidationError
checkFeatureCycles v fid gid ibfm
  | hasCycles (Left fid) (Right gid) v ibfm = Just CreatesCycle
  | otherwise = Nothing

checkGroupCycles :: Validity -> GroupID -> FeatureID -> IntervalBasedFeatureModel -> Maybe ValidationError
checkGroupCycles v gid fid ibfm
  | hasCycles (Right gid) (Left fid) v ibfm = Just CreatesCycle
  | otherwise = Nothing

validate :: TimeOperation -> IntervalBasedFeatureModel -> [ValidationError]
validate (AddOperation v (AddFeature fid name ftype gid)) ibfm =
  let (FeatureValidity existence _ _ _ _) = lookupFeatureDefault fid ibfm
      (GroupValidity groupExistence groupTypes _ _) = lookupGroupDefault gid ibfm
   in catMaybes
        [ checkNodeNotExists v existence
        , checkParentExists v groupExistence
        , checkNameInUse v name ibfm
        , checkCompatibleTypesFeature v ftype groupTypes
        ]
validate (AddOperation v (AddGroup gid gtype fid)) ibfm =
  catMaybes
    [ checkNodeNotExists v . view existenceValidities $ lookupGroupDefault gid ibfm
    , checkParentExists v . view existenceValidities $ lookupFeatureDefault fid ibfm
    ]
validate (ChangeOperation tp (RemoveFeature fid)) ibfm =
  let (FeatureValidity existence names types parents children) = lookupFeatureDefault fid ibfm
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
validate (ChangeOperation tp (RemoveGroup gid)) ibfm =
  let (GroupValidity existence types parents children) = lookupGroupDefault gid ibfm
   in case containingInterval tp existence of
        Nothing -> [NodeNotExists]
        Just (Validity _ endTime) ->
          let scope = Validity tp endTime
           in catMaybes
                [ checkNotChanged scope types $ ChangeGroupType gid
                , checkNotChanged scope parents $ MoveGroup gid
                , checkNoChildren scope children
                ]
validate (ChangeOperation tp (MoveFeature fid gid)) ibfm =
  let (FeatureValidity existence _ types _ _) = lookupFeatureDefault fid ibfm
      (GroupValidity groupExistence groupTypes _ _) = lookupGroupDefault gid ibfm
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
                      ++ [checkFeatureCycles scope fid gid ibfm]
validate (ChangeOperation tp (MoveGroup gid fid)) ibfm =
  let (GroupValidity existence _ _ _) = lookupGroupDefault gid ibfm
      (FeatureValidity featureExistence _ _ _ _) = lookupFeatureDefault fid ibfm
   in case (containingInterval tp existence, containingInterval tp featureExistence) of
        (Nothing, Nothing) -> [NodeNotExists, ParentNotExists]
        (_, Nothing) -> [ParentNotExists]
        (Nothing, _) -> [NodeNotExists]
        (Just (Validity _ endTime), Just (Validity pStart pEnd)) ->
          if pEnd < endTime || pStart > tp
            then [ParentNotExists]
            else
              let scope = Validity tp endTime
               in toList $ checkGroupCycles scope gid fid ibfm
validate (ChangeOperation tp (ChangeFeatureType fid fType)) ibfm =
  let (FeatureValidity _ _ types parents _) = lookupFeatureDefault fid ibfm
   in case containingInterval tp types of
        Nothing -> [NodeNotExists]
        Just (Validity _ endTime) ->
          let scope = Validity tp endTime
           in mapMaybe
                ( \(v, gid) ->
                    ibfm ^? groupValidities . ix gid . typeValidities
                      >>= checkCompatibleTypesFeature (validityOverlap v scope) fType
                )
                . IM.assocs
                . (`IM.intersecting` scope)
                $ parents
validate (ChangeOperation tp (ChangeGroupType gid gType)) ibfm =
  let (GroupValidity _ types _ children) = lookupGroupDefault gid ibfm
   in case containingInterval tp types of
        Nothing -> [NodeNotExists]
        Just (Validity _ endTime) ->
          let scope = Validity tp endTime
           in concatMap
                ( \(v, childIDs) ->
                    mapMaybe
                      ( \fid ->
                          ibfm ^? featureValidities . ix fid . typeValidities
                            >>= checkCompatibleTypesGroup (validityOverlap v scope) gType
                      )
                      (S.toList childIDs)
                )
                . IM.assocs
                . (`IM.intersecting` scope)
                $ children
validate (ChangeOperation tp (ChangeFeatureName fid name)) ibfm =
  let (FeatureValidity _ names _ _ _) = lookupFeatureDefault fid ibfm
   in case containingInterval tp names of
        Nothing -> [NodeNotExists]
        Just (Validity _ endTime) ->
          let scope = Validity tp endTime
              conflictingUses = IM.assocs . flip IM.intersecting scope $ lookupNameDefault name ibfm
           in if null conflictingUses
                then []
                else uncurry NameInUse <$> conflictingUses
