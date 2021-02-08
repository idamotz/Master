module ExampleValidities where

import Control.Lens hiding (element)
import qualified Data.IntervalMap.Generic.Strict as IM
import qualified Data.Map as M
import qualified Data.Set as S
import Types

(=:) = (,)

exampleValidities :: Validities
exampleValidities =
  Validities
    "feature:car"
    -- Name validities --
    ( M.fromList
        [
          ( "Car"
          , IM.fromList [Validity (TP 0) Forever =: "feature:car"]
          )
        ,
          ( "Infotainment System"
          , IM.fromList [Validity (TP 0) Forever =: "feature:infotainment"]
          )
        ,
          ( "Bluetooth"
          , IM.fromList [Validity (TP 0) Forever =: "feature:bluetooth"]
          )
        ,
          ( "Android Auto"
          , IM.fromList [Validity (TP 1) Forever =: "feature:android-auto"]
          )
        ,
          ( "Apple Car Play"
          , IM.fromList [Validity (TP 1) Forever =: "feature:apple-car-play"]
          )
        ,
          ( "Comfort Systems"
          , IM.fromList [Validity (TP 2) Forever =: "feature:comfort-systems"]
          )
        ,
          ( "Parking Pilot"
          , IM.fromList [Validity (TP 2) Forever =: "feature:parking-pilot"]
          )
        ]
    )
    -- Feature validities --
    ( M.fromList
        [
          ( "feature:car"
          , FeatureValidity
              (IM.fromList [Validity (TP 0) Forever =: ()])
              (IM.fromList [Validity (TP 0) Forever =: "Car"])
              (IM.fromList [Validity (TP 0) Forever =: Mandatory])
              (IM.fromList [])
              (IM.fromList [Validity (TP 0) Forever =: S.singleton "group:car1"])
          )
        ,
          ( "feature:infotainment"
          , FeatureValidity
              (IM.fromList [Validity (TP 0) Forever =: ()])
              (IM.fromList [Validity (TP 0) Forever =: "Infotainment System"])
              (IM.fromList [Validity (TP 0) Forever =: Mandatory])
              (IM.fromList [Validity (TP 0) Forever =: "group:car1"])
              ( IM.fromList
                  [ Validity (TP 0) Forever =: S.singleton "group:info1"
                  , Validity (TP 1) Forever =: S.singleton "group:info2"
                  ]
              )
          )
        ,
          ( "feature:bluetooth"
          , FeatureValidity
              (IM.fromList [Validity (TP 0) Forever =: ()])
              (IM.fromList [Validity (TP 0) Forever =: "Bluetooth"])
              (IM.fromList [Validity (TP 0) Forever =: Optional])
              (IM.fromList [Validity (TP 0) Forever =: "group:info1"])
              IM.empty
          )
        ,
          ( "feature:android-auto"
          , FeatureValidity
              (IM.fromList [Validity (TP 1) Forever =: ()])
              (IM.fromList [Validity (TP 1) Forever =: "Android Auto"])
              (IM.fromList [Validity (TP 1) Forever =: Optional])
              (IM.fromList [Validity (TP 1) Forever =: "group:info2"])
              IM.empty
          )
        ,
          ( "feature:apple-car-play"
          , FeatureValidity
              (IM.fromList [Validity (TP 1) Forever =: ()])
              (IM.fromList [Validity (TP 1) Forever =: "Apple Car Play"])
              (IM.fromList [Validity (TP 1) Forever =: Optional])
              (IM.fromList [Validity (TP 1) Forever =: "group:info2"])
              IM.empty
          )
        ,
          ( "feature:comfort-systems"
          , FeatureValidity
              (IM.fromList [Validity (TP 2) Forever =: ()])
              (IM.fromList [Validity (TP 2) Forever =: "Comfort Systems"])
              (IM.fromList [Validity (TP 2) Forever =: Optional])
              (IM.fromList [Validity (TP 2) Forever =: "group:car1"])
              (IM.fromList [Validity (TP 2) Forever =: S.singleton "group:comfort1"])
          )
        ,
          ( "feature:parking-pilot"
          , FeatureValidity
              (IM.fromList [Validity (TP 2) Forever =: ()])
              (IM.fromList [Validity (TP 2) Forever =: "Parking Pilot"])
              (IM.fromList [Validity (TP 2) Forever =: Optional])
              (IM.fromList [Validity (TP 2) Forever =: "group:comfort1"])
              IM.empty
          )
        ]
    )
    -- Group validities --
    ( M.fromList
        [ -- Group 1 of Car (id "feature:car")

          ( "group:car1"
          , GroupValidity
              (IM.fromList [Validity (TP 0) Forever =: ()])
              (IM.fromList [Validity (TP 0) Forever =: And])
              (IM.fromList [Validity (TP 0) Forever =: "feature:car"])
              ( IM.fromList
                  [ Validity (TP 0) Forever =: S.singleton "feature:infotainment"
                  , Validity (TP 2) Forever =: S.singleton "feature:comfort-systems"
                  ]
              )
          )
        , -- Group 1 of Infotainment System (id "feature:infotainment")

          ( "group:info1"
          , GroupValidity
              (IM.fromList [Validity (TP 0) Forever =: ()])
              (IM.fromList [Validity (TP 0) Forever =: And])
              (IM.fromList [Validity (TP 0) Forever =: "feature:infotainment"])
              ( IM.fromList
                  [Validity (TP 0) Forever =: S.singleton "feature:bluetooth"]
              )
          )
        , -- Group 2 of Infotainment System (id "feature:infotainment")

          ( "group:info2"
          , GroupValidity
              (IM.fromList [Validity (TP 1) Forever =: ()])
              (IM.fromList [Validity (TP 1) Forever =: Alternative])
              (IM.fromList [Validity (TP 1) Forever =: "feature:infotainment"])
              ( IM.fromList
                  [ Validity (TP 2) Forever
                      =: S.fromList
                        ["feature:apple-car-play", "feature:android-auto"]
                  ]
              )
          )
        , -- Group 1 of Comfort Systems (id "feature:comfort-systems")

          ( "group:comfort1"
          , GroupValidity
              (IM.fromList [Validity (TP 2) Forever =: ()])
              (IM.fromList [Validity (TP 2) Forever =: Alternative])
              (IM.fromList [Validity (TP 2) Forever =: "feature:comfort-systems"])
              ( IM.fromList
                  [ Validity (TP 2) Forever
                      =: S.singleton "feature:parking-pilot"
                  ]
              )
          )
        ]
    )
