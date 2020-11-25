{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}

module Types where

import           Control.Lens
import qualified Data.IntervalMap.Generic.Strict as IM
import qualified Data.Map                        as M

-- kanskje newtypes TODO
type FeatureID = Int
type RootID    = FeatureID
type GroupID   = Int
type Name      = String

data GroupType = Alternative
               | Or
               | And
               deriving (Show, Eq, Ord)

data FeatureType = Optional
                 | Mandatory
                 deriving (Show, Eq, Ord)


--------------------
--   VALIDITIES   --
--------------------

type ValidityMap a = IM.IntervalMap Validity a

data TimePoint = TP Int
               | Forever
               deriving (Show, Eq, Ord)

data Validity =
  Validity
    { _start :: TimePoint
    , _end   :: TimePoint
    } deriving (Show, Eq, Ord)

-- Intervals are half-closed [x, y) -- left-inclusive, right-exclusive
instance IM.Interval Validity TimePoint where
  lowerBound = _start
  upperBound = _end
  rightClosed _ = False

data Validities =
  Validities
    { _rootID            :: RootID
    , _nameValidities    :: NameValidities
    , _featureValidities :: FeatureValidities
    , _groupValidities   :: GroupValidities
    } deriving (Show, Eq)

type NameValidities    = M.Map Name (ValidityMap FeatureID)
type FeatureValidities = M.Map FeatureID FeatureValidity
type GroupValidities   = M.Map GroupID GroupValidity

data FeatureValidity =
  FeatureValidity
    { _existenceValidities :: ValidityMap ()
    , _nameValidities      :: ValidityMap Name
    , _typeValidities      :: ValidityMap FeatureType
    , _parentValidities    :: ValidityMap GroupID
    } deriving (Show, Eq)

data GroupValidity =
  GroupValidity
    { _existenceValidities :: ValidityMap ()
    , _typeValidities      :: ValidityMap GroupType
    , _parentValidities    :: ValidityMap FeatureID
    } deriving (Show, Eq)

------------------------
--   FEATURE MODELS   --
------------------------

newtype FeatureModel =
  FeatureModel { _rootFeature :: Feature } deriving (Show, Eq)

data Feature =
  Feature
    { _featureID   :: FeatureID
    , _name        :: Name
    , _varType     :: FeatureType
    , _parent      :: Maybe GroupID
    , _childGroups :: [Group]
    } deriving (Show, Eq)

data Group =
  Group
    { _groupID       :: GroupID
    , _varType       :: GroupType
    , _parent        :: FeatureID
    , _childFeatures :: [Feature]
    } deriving (Show, Eq)


makePrisms ''GroupType
makePrisms ''TimePoint
makeFieldsNoPrefix ''Validity
makeFieldsNoPrefix ''Validities
makeFieldsNoPrefix ''FeatureValidity
makeFieldsNoPrefix ''GroupValidity
makePrisms ''FeatureType
makeFieldsNoPrefix ''Feature
makeFieldsNoPrefix ''Group
makeFieldsNoPrefix ''FeatureModel
