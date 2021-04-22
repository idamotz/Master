module ExampleIntervalBasedFeatureModel (exampleIntervalBasedFeatureModel) where

import qualified Data.IntervalMap.Generic.Strict as IM
import qualified Data.Map as M
import qualified Data.Set as S
import Types

f :: String -> FeatureID
f = FeatureID

g :: String -> GroupID
g = GroupID

v :: TimePoint -> TimePoint -> Validity
v = Validity

(∞) :: TimePoint
(∞) = Forever

im :: [(Validity, a)] -> ValidityMap a
im = IM.fromList

set :: Ord a => [a] -> S.Set a
set = S.fromList

-- This is the output of `convert exampleEvolutionPlan`
exampleIntervalBasedFeatureModel :: IntervalBasedFeatureModel
exampleIntervalBasedFeatureModel =
  IntervalBasedFeatureModel
    (f "feature:car")
    ( M.fromList
        [ ("Android Auto", im [(v (TP 1) (∞), f "feature:android-auto")])
        , ("Apple Car Play", im [(v (TP 1) (∞), f "feature:car-play")])
        , ("Assistance Systems", im [(v (TP 6) (∞), f "feature:comfort-systems")])
        , ("Bluetooth", im [(v (TP 0) (TP 7), f "feature:bluetooth")])
        , ("Car", im [(v (TP 0) (∞), f "feature:car")])
        , ("Comfort Systems", im [(v (TP 2) (TP 6), f "feature:comfort-systems")])
        , ("Distance Sensors", im [(v (TP 5) (TP 7), f "feature:distance-sensors")])
        , ("Emergency Brake", im [(v (TP 6) (∞), f "feature:emergency-brake")])
        , ("FrontSensors", im [(v (TP 7) (∞), f "feature:front-sensors")])
        , ("Infotainment System", im [(v (TP 0) (∞), f "feature:infotainment")])
        , ("Parking Pilot", im [(v (TP 2) (∞), f "feature:parking-pilot")])
        , ("Rear Sensors", im [(v (TP 7) (∞), f "feature:rear-sensors")])
        , ("Sensors", im [(v (TP 6) (∞), f "feature:sensors")])
        ]
    )
    ( M.fromList
        [
          ( f "feature:android-auto"
          , FeatureValidity
              (im [(v (TP 1) (∞), ())])
              (im [(v (TP 1) (∞), "Android Auto")])
              (im [(v (TP 1) (∞), Optional)])
              (im [(v (TP 1) (∞), g "group:info2")])
              mempty
          )
        ,
          ( f "feature:bluetooth"
          , FeatureValidity
              (im [(v (TP 0) (TP 7), ())])
              (im [(v (TP 0) (TP 7), "Bluetooth")])
              (im [(v (TP 0) (TP 7), Optional)])
              (im [(v (TP 0) (TP 7), g "group:info1")])
              mempty
          )
        ,
          ( f "feature:car"
          , FeatureValidity
              (im [(v (TP 0) (∞), ())])
              (im [(v (TP 0) (∞), "Car")])
              (im [(v (TP 0) (∞), Mandatory)])
              mempty
              (im [(v (TP 0) (∞), set [g "group:car1"])])
          )
        ,
          ( f "feature:car-play"
          , FeatureValidity
              (im [(v (TP 1) (∞), ())])
              (im [(v (TP 1) (∞), "Apple Car Play")])
              (im [(v (TP 1) (∞), Optional)])
              (im [(v (TP 1) (∞), g "group:info2")])
              mempty
          )
        ,
          ( f "feature:comfort-systems"
          , FeatureValidity
              (im [(v (TP 2) (∞), ())])
              ( im
                  [ (v (TP 2) (TP 6), "Comfort Systems")
                  , (v (TP 6) (∞), "Assistance Systems")
                  ]
              )
              (im [(v (TP 2) (∞), Optional)])
              (im [(v (TP 2) (∞), g "group:car1")])
              (im [(v (TP 2) (∞), set [g "group:comfort1"])])
          )
        ,
          ( f "feature:distance-sensors"
          , FeatureValidity
              (im [(v (TP 5) (TP 7), ())])
              (im [(v (TP 5) (TP 7), "Distance Sensors")])
              (im [(v (TP 5) (TP 7), Mandatory)])
              ( im
                  [ (v (TP 5) (TP 6), g "group:pilot1")
                  , (v (TP 6) (TP 7), g "group:sensors1")
                  ]
              )
              mempty
          )
        ,
          ( f "feature:emergency-brake"
          , FeatureValidity
              (im [(v (TP 6) (∞), ())])
              (im [(v (TP 6) (∞), "Emergency Brake")])
              (im [(v (TP 6) (∞), Optional)])
              (im [(v (TP 6) (∞), g "group:comfort1")])
              mempty
          )
        ,
          ( f "feature:front-sensors"
          , FeatureValidity
              (im [(v (TP 7) (∞), ())])
              (im [(v (TP 7) (∞), "FrontSensors")])
              (im [(v (TP 7) (∞), Optional)])
              (im [(v (TP 7) (∞), g "group:sensors1")])
              mempty
          )
        ,
          ( f "feature:infotainment"
          , FeatureValidity
              (im [(v (TP 0) (∞), ())])
              (im [(v (TP 0) (∞), "Infotainment System")])
              (im [(v (TP 0) (∞), Mandatory)])
              (im [(v (TP 0) (∞), g "group:car1")])
              ( im
                  [ (v (TP 0) (∞), set [g "group:info1"])
                  , (v (TP 1) (∞), set [g "group:info2"])
                  ]
              )
          )
        ,
          ( f "feature:parking-pilot"
          , FeatureValidity
              (im [(v (TP 2) (∞), ())])
              (im [(v (TP 2) (∞), "Parking Pilot")])
              (im [(v (TP 2) (∞), Optional)])
              (im [(v (TP 2) (∞), g "group:comfort1")])
              (im [(v (TP 5) (∞), set [g "group:pilot1"])])
          )
        ,
          ( f "feature:rear-sensors"
          , FeatureValidity
              (im [(v (TP 7) (∞), ())])
              (im [(v (TP 7) (∞), "Rear Sensors")])
              (im [(v (TP 7) (∞), Optional)])
              (im [(v (TP 7) (∞), g "group:sensors1")])
              mempty
          )
        ,
          ( f "feature:sensors"
          , FeatureValidity
              (im [(v (TP 6) (∞), ())])
              (im [(v (TP 6) (∞), "Sensors")])
              (im [(v (TP 6) (∞), Optional)])
              (im [(v (TP 6) (∞), g "group:car1")])
              (im [(v (TP 6) (∞), set [g "group:sensors1"])])
          )
        ]
    )
    ( M.fromList
        [
          ( g "group:car1"
          , GroupValidity
              (im [(v (TP 0) (∞), ())])
              (im [(v (TP 0) (∞), And)])
              (im [(v (TP 0) (∞), f "feature:car")])
              ( im
                  [ (v (TP 0) (∞), set [f "feature:infotainment"])
                  , (v (TP 2) (∞), set [f "feature:comfort-systems"])
                  , (v (TP 6) (∞), set [f "feature:sensors"])
                  ]
              )
          )
        ,
          ( g "group:comfort1"
          , GroupValidity
              (im [(v (TP 2) (∞), ())])
              (im [(v (TP 2) (∞), And)])
              (im [(v (TP 2) (∞), f "feature:comfort-systems")])
              ( im
                  [ (v (TP 2) (∞), set [f "feature:parking-pilot"])
                  , (v (TP 6) (∞), set [f "feature:emergency-brake"])
                  ]
              )
          )
        ,
          ( g "group:info1"
          , GroupValidity
              (im [(v (TP 0) (∞), ())])
              (im [(v (TP 0) (∞), And)])
              (im [(v (TP 0) (∞), f "feature:infotainment")])
              (im [(v (TP 0) (TP 7), set [f "feature:bluetooth"])])
          )
        ,
          ( g "group:info2"
          , GroupValidity
              (im [(v (TP 1) (∞), ())])
              (im [(v (TP 1) (TP 5), Alternative), (v (TP 5) (∞), Or)])
              (im [(v (TP 1) (∞), f "feature:infotainment")])
              ( im [(v (TP 1) (∞), set [f "feature:android-auto", f "feature:car-play"])]
              )
          )
        ,
          ( g "group:pilot1"
          , GroupValidity
              (im [(v (TP 5) (∞), ())])
              (im [(v (TP 5) (∞), And)])
              (im [(v (TP 5) (∞), f "feature:parking-pilot")])
              (im [(v (TP 5) (TP 6), set [f "feature:distance-sensors"])])
          )
        ,
          ( g "group:sensors1"
          , GroupValidity
              (im [(v (TP 6) (∞), ())])
              (im [(v (TP 6) (∞), And)])
              (im [(v (TP 6) (∞), f "feature:sensors")])
              ( im
                  [ (v (TP 6) (TP 7), set [f "feature:distance-sensors"])
                  , (v (TP 7) (∞), set [f "feature:front-sensors", f "feature:rear-sensors"])
                  ]
              )
          )
        ]
    )
