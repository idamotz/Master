module ExampleTemporalFeaturemodel where

import qualified Data.IntervalMap.Generic.Strict as IM
import qualified Data.Map as M
import qualified Data.Set as S
import Types

-- This is the output of `convert exampleEvolutionPlan`
exampleTemporalFeatureModel :: TemporalFeatureModel
exampleTemporalFeatureModel =
  TemporalFeatureModel
    (FeatureID "feature:car")
    ( M.fromList
        [
          ( "Android Auto"
          , IM.fromList
              [
                ( Validity (TP 1) Forever
                , FeatureID "feature:android-auto"
                )
              ]
          )
        ,
          ( "Apple Car Play"
          , IM.fromList
              [
                ( Validity (TP 1) Forever
                , FeatureID "feature:car-play"
                )
              ]
          )
        ,
          ( "Assistance Systems"
          , IM.fromList
              [
                ( Validity (TP 6) Forever
                , FeatureID "feature:comfort-systems"
                )
              ]
          )
        ,
          ( "Bluetooth"
          , IM.fromList
              [
                ( Validity (TP 0) (TP 7)
                , FeatureID "feature:bluetooth"
                )
              ]
          )
        ,
          ( "Car"
          , IM.fromList
              [
                ( Validity (TP 0) Forever
                , FeatureID "feature:car"
                )
              ]
          )
        ,
          ( "Comfort Systems"
          , IM.fromList
              [
                ( Validity (TP 2) (TP 6)
                , FeatureID "feature:comfort-systems"
                )
              ]
          )
        ,
          ( "Distance Sensors"
          , IM.fromList
              [
                ( Validity (TP 5) (TP 7)
                , FeatureID "feature:distance-sensors"
                )
              ]
          )
        ,
          ( "Emergency Brake"
          , IM.fromList
              [
                ( Validity (TP 6) Forever
                , FeatureID "feature:emergency-brake"
                )
              ]
          )
        ,
          ( "FrontSensors"
          , IM.fromList
              [
                ( Validity (TP 7) Forever
                , FeatureID "feature:front-sensors"
                )
              ]
          )
        ,
          ( "Infotainment System"
          , IM.fromList
              [
                ( Validity (TP 0) Forever
                , FeatureID "feature:infotainment"
                )
              ]
          )
        ,
          ( "Parking Pilot"
          , IM.fromList
              [
                ( Validity (TP 2) Forever
                , FeatureID "feature:parking-pilot"
                )
              ]
          )
        ,
          ( "Rear Sensors"
          , IM.fromList
              [
                ( Validity (TP 7) Forever
                , FeatureID "feature:rear-sensors"
                )
              ]
          )
        ,
          ( "Sensors"
          , IM.fromList
              [
                ( Validity (TP 6) Forever
                , FeatureID "feature:sensors"
                )
              ]
          )
        ]
    )
    ( M.fromList
        [
          ( FeatureID "feature:android-auto"
          , FeatureValidity
              ( IM.fromList
                  [
                    ( Validity (TP 1) Forever
                    , ()
                    )
                  ]
              )
              ( IM.fromList
                  [
                    ( Validity (TP 1) Forever
                    , "Android Auto"
                    )
                  ]
              )
              ( IM.fromList
                  [
                    ( Validity (TP 1) Forever
                    , Optional
                    )
                  ]
              )
              ( IM.fromList
                  [
                    ( Validity (TP 1) Forever
                    , GroupID "group:info2"
                    )
                  ]
              )
              ( IM.fromList []
              )
          )
        ,
          ( FeatureID "feature:bluetooth"
          , FeatureValidity
              ( IM.fromList
                  [
                    ( Validity (TP 0) (TP 7)
                    , ()
                    )
                  ]
              )
              ( IM.fromList
                  [
                    ( Validity (TP 0) (TP 7)
                    , "Bluetooth"
                    )
                  ]
              )
              ( IM.fromList
                  [
                    ( Validity (TP 0) (TP 7)
                    , Optional
                    )
                  ]
              )
              ( IM.fromList
                  [
                    ( Validity (TP 0) (TP 7)
                    , GroupID "group:info1"
                    )
                  ]
              )
              ( IM.fromList []
              )
          )
        ,
          ( FeatureID "feature:car"
          , FeatureValidity
              ( IM.fromList
                  [
                    ( Validity (TP 0) Forever
                    , ()
                    )
                  ]
              )
              ( IM.fromList
                  [
                    ( Validity (TP 0) Forever
                    , "Car"
                    )
                  ]
              )
              ( IM.fromList
                  [
                    ( Validity (TP 0) Forever
                    , Mandatory
                    )
                  ]
              )
              (IM.fromList [])
              ( IM.fromList
                  [
                    ( Validity (TP 0) Forever
                    , S.fromList [GroupID "group:car1"]
                    )
                  ]
              )
          )
        ,
          ( FeatureID "feature:car-play"
          , FeatureValidity
              ( IM.fromList
                  [
                    ( Validity (TP 1) Forever
                    , ()
                    )
                  ]
              )
              ( IM.fromList
                  [
                    ( Validity (TP 1) Forever
                    , "Apple Car Play"
                    )
                  ]
              )
              ( IM.fromList
                  [
                    ( Validity (TP 1) Forever
                    , Optional
                    )
                  ]
              )
              ( IM.fromList
                  [
                    ( Validity (TP 1) Forever
                    , GroupID "group:info2"
                    )
                  ]
              )
              ( IM.fromList []
              )
          )
        ,
          ( FeatureID "feature:comfort-systems"
          , FeatureValidity
              ( IM.fromList
                  [
                    ( Validity (TP 2) Forever
                    , ()
                    )
                  ]
              )
              ( IM.fromList
                  [
                    ( Validity (TP 2) (TP 6)
                    , "Comfort Systems"
                    )
                  ,
                    ( Validity (TP 6) Forever
                    , "Assistance Systems"
                    )
                  ]
              )
              ( IM.fromList
                  [
                    ( Validity (TP 2) Forever
                    , Optional
                    )
                  ]
              )
              ( IM.fromList
                  [
                    ( Validity (TP 2) Forever
                    , GroupID "group:car1"
                    )
                  ]
              )
              ( IM.fromList
                  [
                    ( Validity (TP 2) Forever
                    , S.fromList [GroupID "group:comfort1"]
                    )
                  ]
              )
          )
        ,
          ( FeatureID "feature:distance-sensors"
          , FeatureValidity
              ( IM.fromList
                  [
                    ( Validity (TP 5) (TP 7)
                    , ()
                    )
                  ]
              )
              ( IM.fromList
                  [
                    ( Validity (TP 5) (TP 7)
                    , "Distance Sensors"
                    )
                  ]
              )
              ( IM.fromList
                  [
                    ( Validity (TP 5) (TP 7)
                    , Mandatory
                    )
                  ]
              )
              ( IM.fromList
                  [
                    ( Validity (TP 5) (TP 6)
                    , GroupID "group:pilot1"
                    )
                  ,
                    ( Validity (TP 6) (TP 7)
                    , GroupID "group:sensors1"
                    )
                  ]
              )
              ( IM.fromList []
              )
          )
        ,
          ( FeatureID "feature:emergency-brake"
          , FeatureValidity
              ( IM.fromList
                  [
                    ( Validity (TP 6) Forever
                    , ()
                    )
                  ]
              )
              ( IM.fromList
                  [
                    ( Validity (TP 6) Forever
                    , "Emergency Brake"
                    )
                  ]
              )
              ( IM.fromList
                  [
                    ( Validity (TP 6) Forever
                    , Optional
                    )
                  ]
              )
              ( IM.fromList
                  [
                    ( Validity (TP 6) Forever
                    , GroupID "group:comfort1"
                    )
                  ]
              )
              ( IM.fromList []
              )
          )
        ,
          ( FeatureID "feature:front-sensors"
          , FeatureValidity
              ( IM.fromList
                  [
                    ( Validity (TP 7) Forever
                    , ()
                    )
                  ]
              )
              ( IM.fromList
                  [
                    ( Validity (TP 7) Forever
                    , "FrontSensors"
                    )
                  ]
              )
              ( IM.fromList
                  [
                    ( Validity (TP 7) Forever
                    , Optional
                    )
                  ]
              )
              ( IM.fromList
                  [
                    ( Validity (TP 7) Forever
                    , GroupID "group:sensors1"
                    )
                  ]
              )
              ( IM.fromList []
              )
          )
        ,
          ( FeatureID "feature:infotainment"
          , FeatureValidity
              ( IM.fromList
                  [
                    ( Validity (TP 0) Forever
                    , ()
                    )
                  ]
              )
              ( IM.fromList
                  [
                    ( Validity (TP 0) Forever
                    , "Infotainment System"
                    )
                  ]
              )
              ( IM.fromList
                  [
                    ( Validity (TP 0) Forever
                    , Mandatory
                    )
                  ]
              )
              ( IM.fromList
                  [
                    ( Validity (TP 0) Forever
                    , GroupID "group:car1"
                    )
                  ]
              )
              ( IM.fromList
                  [
                    ( Validity (TP 0) Forever
                    , S.fromList [GroupID "group:info1"]
                    )
                  ,
                    ( Validity (TP 1) Forever
                    , S.fromList [GroupID "group:info2"]
                    )
                  ]
              )
          )
        ,
          ( FeatureID "feature:parking-pilot"
          , FeatureValidity
              ( IM.fromList
                  [
                    ( Validity (TP 2) Forever
                    , ()
                    )
                  ]
              )
              ( IM.fromList
                  [
                    ( Validity (TP 2) Forever
                    , "Parking Pilot"
                    )
                  ]
              )
              ( IM.fromList
                  [
                    ( Validity (TP 2) Forever
                    , Optional
                    )
                  ]
              )
              ( IM.fromList
                  [
                    ( Validity (TP 2) Forever
                    , GroupID "group:comfort1"
                    )
                  ]
              )
              ( IM.fromList
                  [
                    ( Validity (TP 5) Forever
                    , S.fromList [GroupID "group:pilot1"]
                    )
                  ]
              )
          )
        ,
          ( FeatureID "feature:rear-sensors"
          , FeatureValidity
              ( IM.fromList
                  [
                    ( Validity (TP 7) Forever
                    , ()
                    )
                  ]
              )
              ( IM.fromList
                  [
                    ( Validity (TP 7) Forever
                    , "Rear Sensors"
                    )
                  ]
              )
              ( IM.fromList
                  [
                    ( Validity (TP 7) Forever
                    , Optional
                    )
                  ]
              )
              ( IM.fromList
                  [
                    ( Validity (TP 7) Forever
                    , GroupID "group:sensors1"
                    )
                  ]
              )
              ( IM.fromList []
              )
          )
        ,
          ( FeatureID "feature:sensors"
          , FeatureValidity
              ( IM.fromList
                  [
                    ( Validity (TP 6) Forever
                    , ()
                    )
                  ]
              )
              ( IM.fromList
                  [
                    ( Validity (TP 6) Forever
                    , "Sensors"
                    )
                  ]
              )
              ( IM.fromList
                  [
                    ( Validity (TP 6) Forever
                    , Optional
                    )
                  ]
              )
              ( IM.fromList
                  [
                    ( Validity (TP 6) Forever
                    , GroupID "group:car1"
                    )
                  ]
              )
              ( IM.fromList
                  [
                    ( Validity (TP 6) Forever
                    , S.fromList [GroupID "group:sensors1"]
                    )
                  ]
              )
          )
        ]
    )
    ( M.fromList
        [
          ( GroupID "group:car1"
          , GroupValidity
              ( IM.fromList
                  [
                    ( Validity (TP 0) Forever
                    , ()
                    )
                  ]
              )
              ( IM.fromList
                  [
                    ( Validity (TP 0) Forever
                    , And
                    )
                  ]
              )
              ( IM.fromList
                  [
                    ( Validity (TP 0) Forever
                    , FeatureID "feature:car"
                    )
                  ]
              )
              ( IM.fromList
                  [
                    ( Validity (TP 0) Forever
                    , S.fromList [FeatureID "feature:infotainment"]
                    )
                  ,
                    ( Validity (TP 2) Forever
                    , S.fromList [FeatureID "feature:comfort-systems"]
                    )
                  ,
                    ( Validity (TP 6) Forever
                    , S.fromList [FeatureID "feature:sensors"]
                    )
                  ]
              )
          )
        ,
          ( GroupID "group:comfort1"
          , GroupValidity
              ( IM.fromList
                  [
                    ( Validity (TP 2) Forever
                    , ()
                    )
                  ]
              )
              ( IM.fromList
                  [
                    ( Validity (TP 2) Forever
                    , And
                    )
                  ]
              )
              ( IM.fromList
                  [
                    ( Validity (TP 2) Forever
                    , FeatureID "feature:comfort-systems"
                    )
                  ]
              )
              ( IM.fromList
                  [
                    ( Validity (TP 2) Forever
                    , S.fromList [FeatureID "feature:parking-pilot"]
                    )
                  ,
                    ( Validity (TP 6) Forever
                    , S.fromList [FeatureID "feature:emergency-brake"]
                    )
                  ]
              )
          )
        ,
          ( GroupID "group:info1"
          , GroupValidity
              ( IM.fromList
                  [
                    ( Validity (TP 0) Forever
                    , ()
                    )
                  ]
              )
              ( IM.fromList
                  [
                    ( Validity (TP 0) Forever
                    , And
                    )
                  ]
              )
              ( IM.fromList
                  [
                    ( Validity (TP 0) Forever
                    , FeatureID "feature:infotainment"
                    )
                  ]
              )
              ( IM.fromList
                  [
                    ( Validity (TP 0) (TP 7)
                    , S.fromList [FeatureID "feature:bluetooth"]
                    )
                  ]
              )
          )
        ,
          ( GroupID "group:info2"
          , GroupValidity
              ( IM.fromList
                  [
                    ( Validity (TP 1) Forever
                    , ()
                    )
                  ]
              )
              ( IM.fromList
                  [
                    ( Validity (TP 1) (TP 5)
                    , Alternative
                    )
                  ,
                    ( Validity (TP 5) Forever
                    , Or
                    )
                  ]
              )
              ( IM.fromList
                  [
                    ( Validity (TP 1) Forever
                    , FeatureID "feature:infotainment"
                    )
                  ]
              )
              ( IM.fromList
                  [
                    ( Validity (TP 1) Forever
                    , S.fromList
                        [ FeatureID "feature:android-auto"
                        , FeatureID "feature:car-play"
                        ]
                    )
                  ]
              )
          )
        ,
          ( GroupID "group:pilot1"
          , GroupValidity
              ( IM.fromList
                  [
                    ( Validity (TP 5) Forever
                    , ()
                    )
                  ]
              )
              ( IM.fromList
                  [
                    ( Validity (TP 5) Forever
                    , And
                    )
                  ]
              )
              ( IM.fromList
                  [
                    ( Validity (TP 5) Forever
                    , FeatureID "feature:parking-pilot"
                    )
                  ]
              )
              ( IM.fromList
                  [
                    ( Validity (TP 5) (TP 6)
                    , S.fromList [FeatureID "feature:distance-sensors"]
                    )
                  ]
              )
          )
        ,
          ( GroupID "group:sensors1"
          , GroupValidity
              ( IM.fromList
                  [
                    ( Validity (TP 6) Forever
                    , ()
                    )
                  ]
              )
              ( IM.fromList
                  [
                    ( Validity (TP 6) Forever
                    , And
                    )
                  ]
              )
              ( IM.fromList
                  [
                    ( Validity (TP 6) Forever
                    , FeatureID "feature:sensors"
                    )
                  ]
              )
              ( IM.fromList
                  [
                    ( Validity (TP 6) (TP 7)
                    , S.fromList [FeatureID "feature:distance-sensors"]
                    )
                  ,
                    ( Validity (TP 7) Forever
                    , S.fromList
                        [ FeatureID "feature:front-sensors"
                        , FeatureID "feature:rear-sensors"
                        ]
                    )
                  ]
              )
          )
        ]
    )
