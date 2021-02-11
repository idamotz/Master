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
        ( [ Group
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
                          (f "bluetooth")
                          "Bluetooth"
                          Optional
                          []
                      ]
                  ]
              ]
          ]
        )
    )

-- data TimeOperation = AddOperation Validity Operation | TimeOperation TimePoint Operation deriving (Show, Eq)

-- data Operation
--   = AddFeature FeatureID Name FeatureType GroupID
--   | AddGroup GroupID GroupType FeatureID
--   | RemoveFeature FeatureID
--   | RemoveGroup GroupID
--   | MoveFeature FeatureID GroupID
--   | MoveGroup GroupID FeatureID
--   | ChangeFeatureType FeatureID FeatureType
--   | ChangeGroupType GroupID GroupType
--   | ChangeFeatureName FeatureID Name
--   deriving (Show, Eq)

to = TimeOperation
ao = AddOperation
v = Validity

operations :: [TimeOperation]
operations =
  (ao (v (TP 1) Forever) <$> tp1)
    ++ (ao (v (TP 2) Forever) <$> tp2)
    ++ tp5
    ++ (to (TP 6) <$> tp6)
    ++ (to (TP 7) <$> tp7)
  where
    tp1 =
      [ AddGroup (g "group:info2") Alternative (f "feature:infotainment")
      , AddFeature (f "feature:android-auto") "Android Auto" Optional (g "group:info2")
      , AddFeature (f "feature:car-play") "Apple Car Play" Optional (g "group:info2")
      ]
    tp2 =
      [ AddFeature (f "feature:comfort-systems") "ComfortSystems" Optional (g "group:car1")
      , AddGroup (g "group:comfort1") And (f "feature:comfort-systems")
      , AddFeature (f "feature:parking-pilot") "Parking Pilot" Optional (g "group:comfort1")
      ]
    tp5 =
      [ to (TP 5) $ ChangeGroupType (g "group:info2") Or
      , ao (v (TP 5) Forever) $ AddGroup (g "group:pilot1") And (f "feature:parking-pilot")
      , ao (v (TP 5) Forever) $
          AddFeature
            (f "feature:distance-sensors")
            "Distance Sensors"
            Mandatory
            (g "group:pilot1")
      ]
    tp6 = []
    tp7 = []

-- Group groupID varType parent childFeatures

--   eq plan = at 1 do
--             addGroup("feature:infotainment", "group:info2", ALTERNATIVE)                              --- T1
--             addFeature("feature:android auto", "Android Auto", "group:info2", OPTIONAL)               --- T1
--             addFeature("feature:car-play", "Apple Car Play", "group:info2", OPTIONAL)     ; ;; --- T1

--             at 2 do
--             addFeature("feature:comfort-systems", "Comfort Systems", "group:car1", OPTIONAL)        --- T2
--             addGroup("feature:comfort-systems", "group:comfort1", AND)                              --- T2
--             addFeature("feature:parking-pilot", "Parking Pilot", "group:comfort1", OPTIONAL)      ; ;; --- T2

--             at 5 do
--             changeGroupType("group:info2", OR)                                      --- T5
--             addGroup("feature:parking-pilot", "group:pilot1", AND)                                     --- T5
--             addFeature("feature:distance-sensors", "Distance Sensors", "group:pilot1", MANDATORY) ; ;; --- T5

--             at 6 do
--             addFeature(sensors, "Sensors", "group:car1", OPTIONAL)                   --- T6
--             addGroup(sensors, "sensors1", AND)                                 --- T6 (implicit)
--             moveFeature("feature:distance-sensors", "sensors1")                                  --- T6
--             addFeature(brake, "Emergency Brake", "group:comfort1", OPTIONAL)         --- T6
--             renameFeature("feature:comfort-systems", "Assistance Systems")               ; ;; --- T6

--             at 7 do
--             removeFeature("feature:distance-sensors")                                            --- T7 |
--             addFeature(frontSensors, "Front Sensors", "sensors1", OPTIONAL)    --- T7 | Split Sensors feature
--             addFeature(rearSensors, "Rear Sensors", "sensors1", OPTIONAL)      --- T7 |
--             removeFeature(bluetooth)                                      ;    --- T7
--             .
