{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Types where

import Control.Lens
import qualified Data.IntervalMap.Generic.Strict as IM
import qualified Data.Map as M
import qualified Data.Set as S
import qualified GHC.Exts as E

newtype FeatureID = FeatureID String deriving (Show, Eq, Ord)
type RootID = FeatureID
newtype GroupID = GroupID String deriving (Show, Eq, Ord)
type Name = String

data GroupType
  = Alternative
  | Or
  | And
  deriving (Show, Eq, Ord)

data FeatureType
  = Optional
  | Mandatory
  deriving (Show, Eq, Ord)

--------------------
--   VALIDITIES   --
--------------------

type ValidityMap a = IM.IntervalMap Validity a

data TimePoint
  = TP Int
  | Forever
  deriving (Show, Eq, Ord)

data Validity = Validity
  { _start :: TimePoint
  , _end :: TimePoint
  }
  deriving (Show, Eq, Ord)

-- Intervals are half-closed [x, y) -- left-inclusive, right-exclusive
instance IM.Interval Validity TimePoint where
  lowerBound = _start
  upperBound = _end
  rightClosed _ = False

instance Semigroup Validity where
  Validity s1 e1 <> Validity s2 e2 = Validity (min s1 s2) (max e1 e2)

-- instance (IM.Interval iv e, Ord e) => E.IsList (IM.IntervalMap iv a) where
--   type Item (IM.IntervalMap iv a) = (iv, a)
--   fromList = IM.fromList
--   toList = IM.toList

data IntervalBasedFeatureModel = IntervalBasedFeatureModel
  { _rootID :: RootID
  , _nameValidities :: NameValidities
  , _featureValidities :: FeatureValidities
  , _groupValidities :: GroupValidities
  }
  deriving (Show, Eq)

type NameValidities = M.Map Name (ValidityMap FeatureID)
type FeatureValidities = M.Map FeatureID FeatureValidity
type GroupValidities = M.Map GroupID GroupValidity

data FeatureValidity = FeatureValidity
  { _existenceValidities :: ValidityMap ()
  , _nameValidities :: ValidityMap Name
  , _typeValidities :: ValidityMap FeatureType
  , _parentValidities :: ValidityMap GroupID
  , _childValidities :: ValidityMap (S.Set GroupID)
  }
  deriving (Show, Eq)

data GroupValidity = GroupValidity
  { _existenceValidities :: ValidityMap ()
  , _typeValidities :: ValidityMap GroupType
  , _parentValidities :: ValidityMap FeatureID
  , _childValidities :: ValidityMap (S.Set FeatureID)
  }
  deriving (Show, Eq)

------------------------
--   FEATURE MODELS   --
------------------------

newtype FeatureModel = FeatureModel {_rootFeature :: Feature}
  deriving (Show, Eq)

type TreeSequence = [(TimePoint, FeatureModel)]

data EvolutionPlan = EvolutionPlan
  { _initialModel :: FeatureModel
  , _initialTime :: TimePoint
  , _operations :: [TimeOperation]
  }
  deriving (Show)

data Feature = Feature
  { _featureID :: FeatureID
  , _name :: Name
  , _varType :: FeatureType
  , _childGroups :: [Group]
  }
  deriving (Show, Eq)

data Group = Group
  { _groupID :: GroupID
  , _varType :: GroupType
  , _childFeatures :: [Feature]
  }
  deriving (Show, Eq)

data TimeOperation
  = AddOperation Validity AddOperation
  | ChangeOperation TimePoint ChangeOperation
  deriving (Show, Eq)

data AddOperation
  = AddFeature FeatureID Name FeatureType GroupID
  | AddGroup GroupID GroupType FeatureID
  deriving (Show, Eq)

data ChangeOperation
  = RemoveFeature FeatureID
  | RemoveGroup GroupID
  | MoveFeature FeatureID GroupID
  | MoveGroup GroupID FeatureID
  | ChangeFeatureType FeatureID FeatureType
  | ChangeGroupType GroupID GroupType
  | ChangeFeatureName FeatureID Name
  deriving (Show, Eq)

--------------------
-- Error Messages --
--------------------

data ValidationError
  = NodeAlreadyExists Validity
  | ParentNotExists
  | NameInUse Validity FeatureID
  | IncompatibleTypes FeatureType GroupType Validity
  | ChangePlanned TimePoint ChangeOperation
  | NodeNotExists
  | HasChildren Validity
  | CreatesCycle
  deriving (Show, Eq)

makePrisms ''GroupType
makePrisms ''TimePoint
makeFieldsNoPrefix ''Validity
makeFieldsNoPrefix ''IntervalBasedFeatureModel
makeFieldsNoPrefix ''FeatureValidity
makeFieldsNoPrefix ''GroupValidity
makePrisms ''FeatureType
makeFieldsNoPrefix ''Feature
makeFieldsNoPrefix ''Group
makeFieldsNoPrefix ''FeatureModel
makeFieldsNoPrefix ''EvolutionPlan
makePrisms ''TimeOperation
