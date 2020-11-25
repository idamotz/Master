module ExampleValidities where

import           Control.Lens                    hiding (element)
import qualified Data.IntervalMap.Generic.Strict as IM
import qualified Data.Map                        as M
import           Types

(=:) = (,)

exampleValidities :: Validities
exampleValidities =
  Validities 0
    -- Name validities --
    ( M.fromList
        [ ( "Car"
          , IM.fromList [ Validity (TP 0) Forever =: 0 ]
          )
        , ( "Infotainment System"
          , IM.fromList [ Validity (TP 0) Forever =: 1 ]
          )
        , ( "Bluetooth"
          , IM.fromList [ Validity (TP 0) Forever =: 2 ]
          )
        , ( "Android Auto"
          , IM.fromList [ Validity (TP 1) Forever =: 3 ]
          )
        , ( "Apple Car Play"
          , IM.fromList [ Validity (TP 1) Forever =: 4 ]
          )
        , ( "Comfort Systems"
          , IM.fromList [ Validity (TP 2) Forever =: 5 ]
          )
        , ( "Parking Pilot"
          , IM.fromList [ Validity (TP 2) Forever =: 6 ]
          )
        ]
    )
    -- Feature validities --
    ( M.fromList
        [ ( 0
          , FeatureValidity
              (IM.fromList [ Validity (TP 0) Forever =: () ])
              (IM.fromList [ Validity (TP 0) Forever =: "Car" ])
              (IM.fromList [ Validity (TP 0) Forever =: Mandatory ])
              (IM.fromList [])
          )
        , ( 1
          , FeatureValidity
              (IM.fromList [ Validity (TP 0) Forever =: () ])
              (IM.fromList [ Validity (TP 0) Forever =: "Infotainment System" ])
              (IM.fromList [ Validity (TP 0) Forever =: Mandatory ])
              (IM.fromList [ Validity (TP 0) Forever =: 10 ])
          )
        , ( 2
          , FeatureValidity
              (IM.fromList [ Validity (TP 0) Forever =: () ])
              (IM.fromList [ Validity (TP 0) Forever =: "Bluetooth" ])
              (IM.fromList [ Validity (TP 0) Forever =: Optional ])
              (IM.fromList [ Validity (TP 0) Forever =: 11 ])
          )
        , ( 3
          , FeatureValidity
              (IM.fromList [ Validity (TP 1) Forever =: () ])
              (IM.fromList [ Validity (TP 1) Forever =: "Android Auto" ])
              (IM.fromList [ Validity (TP 1) Forever =: Optional ])
              (IM.fromList [ Validity (TP 1) Forever =: 21 ])
          )
        , ( 4
          , FeatureValidity
              (IM.fromList [ Validity (TP 1) Forever =: () ])
              (IM.fromList [ Validity (TP 1) Forever =: "Apple Car Play" ])
              (IM.fromList [ Validity (TP 1) Forever =: Optional ])
              (IM.fromList [ Validity (TP 1) Forever =: 21 ])
          )
        , ( 5
          , FeatureValidity
              (IM.fromList [ Validity (TP 2) Forever =: () ])
              (IM.fromList [ Validity (TP 2) Forever =: "Comfort Systems" ])
              (IM.fromList [ Validity (TP 2) Forever =: Optional ])
              (IM.fromList [ Validity (TP 2) Forever =: 10 ])
          )
        , ( 6
          , FeatureValidity
              (IM.fromList [ Validity (TP 2) Forever =: () ])
              (IM.fromList [ Validity (TP 2) Forever =: "Parking Pilot" ])
              (IM.fromList [ Validity (TP 2) Forever =: Optional ])
              (IM.fromList [ Validity (TP 2) Forever =: 15 ])
          )
        ]
    )
    -- Group validities --
    ( M.fromList
        [ -- Group 1 of Car (id 0)
          ( 10
          , GroupValidity
              (IM.fromList [ Validity (TP 0) Forever =: () ])
              (IM.fromList [ Validity (TP 0) Forever =: And ])
              (IM.fromList [ Validity (TP 0) Forever =: 0 ])
          )
          -- Group 1 of Infotainment System (id 1)
        , ( 11
          , GroupValidity
              (IM.fromList [ Validity (TP 0) Forever =: () ])
              (IM.fromList [ Validity (TP 0) Forever =: And ])
              (IM.fromList [ Validity (TP 0) Forever =: 1 ])
          )
          -- Group 2 of Infotainment System (id 1)
        , ( 21
          , GroupValidity
              (IM.fromList [ Validity (TP 1) Forever =: () ])
              (IM.fromList [ Validity (TP 1) Forever =: Alternative ])
              (IM.fromList [ Validity (TP 1) Forever =: 1 ])
          )
          -- Group 1 of Comfort Systems (id 5)
        , ( 15
          , GroupValidity
              (IM.fromList [ Validity (TP 1) Forever =: () ])
              (IM.fromList [ Validity (TP 1) Forever =: Alternative ])
              (IM.fromList [ Validity (TP 1) Forever =: 1 ])
          )
        ]
    )

