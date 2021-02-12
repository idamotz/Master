module ExampleValidities (exampleValidities) where

import qualified Data.IntervalMap.Generic.Strict as IM
import qualified Data.Map as M
import qualified Data.Set as S
import Types

(=:) = (,)

f = FeatureID
g = GroupID

exampleValidities :: TemporalFeatureModel
exampleValidities =
  TemporalFeatureModel
    (f "feature:car")
    -- Name validities --
    ( M.fromList
        [
          ( "Car"
          , IM.fromList [Validity (TP 0) Forever =: (f "feature:car")]
          )
        ,
          ( "Infotainment System"
          , IM.fromList [Validity (TP 0) Forever =: (f "feature:infotainment")]
          )
        ,
          ( "Bluetooth"
          , IM.fromList [Validity (TP 0) Forever =: (f "feature:bluetooth")]
          )
        ,
          ( "Android Auto"
          , IM.fromList [Validity (TP 1) Forever =: (f "feature:android-auto")]
          )
        ,
          ( "Apple Car Play"
          , IM.fromList [Validity (TP 1) Forever =: (f "feature:apple-car-play")]
          )
        ,
          ( "Comfort Systems"
          , IM.fromList [Validity (TP 2) Forever =: (f "feature:comfort-systems")]
          )
        ,
          ( "Parking Pilot"
          , IM.fromList [Validity (TP 2) Forever =: (f "feature:parking-pilot")]
          )
        ]
    )
    -- Feature validities --
    ( M.fromList
        [
          ( (f "feature:car")
          , FeatureValidity
              (IM.fromList [Validity (TP 0) Forever =: ()])
              (IM.fromList [Validity (TP 0) Forever =: "Car"])
              (IM.fromList [Validity (TP 0) Forever =: Mandatory])
              (IM.fromList [])
              (IM.fromList [Validity (TP 0) Forever =: S.singleton (g "group:car1")])
          )
        ,
          ( (f "feature:infotainment")
          , FeatureValidity
              (IM.fromList [Validity (TP 0) Forever =: ()])
              (IM.fromList [Validity (TP 0) Forever =: "Infotainment System"])
              (IM.fromList [Validity (TP 0) Forever =: Mandatory])
              (IM.fromList [Validity (TP 0) Forever =: (g "group:car1")])
              ( IM.fromList
                  [ Validity (TP 0) Forever =: S.singleton (g "group:info1")
                  , Validity (TP 1) Forever =: S.singleton (g "group:info2")
                  ]
              )
          )
        ,
          ( (f "feature:bluetooth")
          , FeatureValidity
              (IM.fromList [Validity (TP 0) Forever =: ()])
              (IM.fromList [Validity (TP 0) Forever =: "Bluetooth"])
              (IM.fromList [Validity (TP 0) Forever =: Optional])
              (IM.fromList [Validity (TP 0) Forever =: (g "group:info1")])
              IM.empty
          )
        ,
          ( (f "feature:android-auto")
          , FeatureValidity
              (IM.fromList [Validity (TP 1) Forever =: ()])
              (IM.fromList [Validity (TP 1) Forever =: "Android Auto"])
              (IM.fromList [Validity (TP 1) Forever =: Optional])
              (IM.fromList [Validity (TP 1) Forever =: (g "group:info2")])
              IM.empty
          )
        ,
          ( (f "feature:apple-car-play")
          , FeatureValidity
              (IM.fromList [Validity (TP 1) Forever =: ()])
              (IM.fromList [Validity (TP 1) Forever =: "Apple Car Play"])
              (IM.fromList [Validity (TP 1) Forever =: Optional])
              (IM.fromList [Validity (TP 1) Forever =: (g "group:info2")])
              IM.empty
          )
        ,
          ( (f "feature:comfort-systems")
          , FeatureValidity
              (IM.fromList [Validity (TP 2) Forever =: ()])
              (IM.fromList [Validity (TP 2) Forever =: "Comfort Systems"])
              (IM.fromList [Validity (TP 2) Forever =: Optional])
              (IM.fromList [Validity (TP 2) Forever =: (g "group:car1")])
              (IM.fromList [Validity (TP 2) Forever =: S.singleton (g "group:comfort1")])
          )
        ,
          ( (f "feature:parking-pilot")
          , FeatureValidity
              (IM.fromList [Validity (TP 2) Forever =: ()])
              (IM.fromList [Validity (TP 2) Forever =: "Parking Pilot"])
              (IM.fromList [Validity (TP 2) Forever =: Optional])
              (IM.fromList [Validity (TP 2) Forever =: (g "group:comfort1")])
              IM.empty
          )
        ]
    )
    -- Group validities --
    ( M.fromList
        [ -- Group 1 of Car (id (f "feature:car"))

          ( (g "group:car1")
          , GroupValidity
              (IM.fromList [Validity (TP 0) Forever =: ()])
              (IM.fromList [Validity (TP 0) Forever =: And])
              (IM.fromList [Validity (TP 0) Forever =: (f "feature:car")])
              ( IM.fromList
                  [ Validity (TP 0) Forever =: S.singleton (f "feature:infotainment")
                  , Validity (TP 2) Forever =: S.singleton (f "feature:comfort-systems")
                  ]
              )
          )
        , -- Group 1 of Infotainment System (id (f "feature:infotainment"))

          ( (g "group:info1")
          , GroupValidity
              (IM.fromList [Validity (TP 0) Forever =: ()])
              (IM.fromList [Validity (TP 0) Forever =: And])
              (IM.fromList [Validity (TP 0) Forever =: (f "feature:infotainment")])
              ( IM.fromList
                  [Validity (TP 0) Forever =: S.singleton (f "feature:bluetooth")]
              )
          )
        , -- Group 2 of Infotainment System (id (f "feature:infotainment"))

          ( (g "group:info2")
          , GroupValidity
              (IM.fromList [Validity (TP 1) Forever =: ()])
              (IM.fromList [Validity (TP 1) Forever =: Alternative])
              (IM.fromList [Validity (TP 1) Forever =: (f "feature:infotainment")])
              ( IM.fromList
                  [ Validity (TP 2) Forever
                      =: S.fromList
                        [(f "feature:apple-car-play"), (f "feature:android-auto")]
                  ]
              )
          )
        , -- Group 1 of Comfort Systems (id (f "feature:comfort-systems"))

          ( (g "group:comfort1")
          , GroupValidity
              (IM.fromList [Validity (TP 2) Forever =: ()])
              (IM.fromList [Validity (TP 2) Forever =: Alternative])
              (IM.fromList [Validity (TP 2) Forever =: (f "feature:comfort-systems")])
              ( IM.fromList
                  [ Validity (TP 2) Forever
                      =: S.singleton (f "feature:parking-pilot")
                  ]
              )
          )
        ]
    )
