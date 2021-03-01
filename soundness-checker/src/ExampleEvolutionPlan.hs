module ExampleEvolutionPlan where

import Types

f = FeatureID
g = GroupID

exampleEvolutionPlan = EvolutionPlan model (TP 0) operations

model :: FeatureModel
model =
  FeatureModel
    ( Feature
        (f "feature:car")
        "Car"
        Mandatory
        [ Group
            (g "group:car1")
            And
            [ Feature
                (f "feature:infotainment")
                "Infotainment System"
                Mandatory
                [ Group
                    (g "group:info1")
                    And
                    [ Feature
                        (f "feature:bluetooth")
                        "Bluetooth"
                        Optional
                        []
                    ]
                ]
            ]
        ]
    )

to = ChangeOperation
ao = AddOperation
v = Validity

operations :: [TimeOperation]
operations =
  (ao (v (TP 1) Forever) <$> tp1) ++ (ao (v (TP 2) Forever) <$> tp2) ++ tp5 ++ tp6 ++ tp7
  where
    tp1 =
      [ AddGroup (g "group:info2") Alternative (f "feature:infotainment")
      , AddFeature (f "feature:android-auto") "Android Auto" Optional (g "group:info2")
      , AddFeature (f "feature:car-play") "Apple Car Play" Optional (g "group:info2")
      ]
    tp2 =
      [ AddFeature (f "feature:comfort-systems") "Comfort Systems" Optional (g "group:car1")
      , AddGroup (g "group:comfort1") And (f "feature:comfort-systems")
      , AddFeature (f "feature:parking-pilot") "Parking Pilot" Optional (g "group:comfort1")
      ]
    tp5 =
      [ to (TP 5) $ ChangeGroupType (g "group:info2") Or
      , ao (v (TP 5) Forever) $ AddGroup (g "group:pilot1") And (f "feature:parking-pilot")
      , ao (v (TP 5) Forever) $ AddFeature (f "feature:distance-sensors") "Distance Sensors" Mandatory (g "group:pilot1")
      ]
    tp6 =
      [ ao (v (TP 6) Forever) $ AddFeature (f "feature:sensors") "Sensors" Optional (g "group:car1")
      , ao (v (TP 6) Forever) $ AddGroup (g "group:sensors1") And (f "feature:sensors")
      , to (TP 6) $ MoveFeature (f "feature:distance-sensors") (g "group:sensors1")
      , ao (v (TP 6) Forever) $ AddFeature (f "feature:emergency-brake") "Emergency Brake" Optional (g "group:comfort1")
      , to (TP 6) $ ChangeFeatureName (f "feature:comfort-systems") "Assistance Systems"
      ]
    tp7 =
      [ to (TP 7) $ RemoveFeature (f "feature:distance-sensors")
      , ao (v (TP 7) Forever) $ AddFeature (f "feature:front-sensors") "FrontSensors" Optional (g "group:sensors1")
      , ao (v (TP 7) Forever) $ AddFeature (f "feature:rear-sensors") "Rear Sensors" Optional (g "group:sensors1")
      , to (TP 7) $ RemoveFeature (f "feature:bluetooth")
      ]
