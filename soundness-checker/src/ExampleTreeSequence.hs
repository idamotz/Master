module ExampleTreeSequence where

import Types

exampleTimeSequence :: TreeSequence
exampleTimeSequence =
  [
    ( TP 0
    , FeatureModel
        ( Feature
            (FeatureID "feature:car")
            "Car"
            Mandatory
            [ Group
                (GroupID "group:car1")
                And
                [ Feature
                    (FeatureID "feature:infotainment")
                    "Infotainment System"
                    Mandatory
                    [ Group
                        (GroupID "group:info1")
                        And
                        [ Feature
                            (FeatureID "feature:bluetooth")
                            "Bluetooth"
                            Optional
                            []
                        ]
                    ]
                ]
            ]
        )
    )
  ,
    ( TP 1
    , FeatureModel
        ( Feature
            (FeatureID "feature:car")
            "Car"
            Mandatory
            [ Group
                (GroupID "group:car1")
                And
                [ Feature
                    (FeatureID "feature:infotainment")
                    "Infotainment System"
                    Mandatory
                    [ Group
                        (GroupID "group:info1")
                        And
                        [ Feature
                            (FeatureID "feature:bluetooth")
                            "Bluetooth"
                            Optional
                            []
                        ]
                    , Group
                        (GroupID "group:info2")
                        Alternative
                        [ Feature
                            (FeatureID "feature:android-auto")
                            "Android Auto"
                            Optional
                            []
                        , Feature
                            (FeatureID "feature:car-play")
                            "Apple Car Play"
                            Optional
                            []
                        ]
                    ]
                ]
            ]
        )
    )
  ,
    ( TP 2
    , FeatureModel
        ( Feature
            (FeatureID "feature:car")
            "Car"
            Mandatory
            [ Group
                (GroupID "group:car1")
                And
                [ Feature
                    (FeatureID "feature:infotainment")
                    "Infotainment System"
                    Mandatory
                    [ Group
                        (GroupID "group:info1")
                        And
                        [ Feature
                            (FeatureID "feature:bluetooth")
                            "Bluetooth"
                            Optional
                            []
                        ]
                    , Group
                        (GroupID "group:info2")
                        Alternative
                        [ Feature
                            (FeatureID "feature:android-auto")
                            "Android Auto"
                            Optional
                            []
                        , Feature
                            (FeatureID "feature:car-play")
                            "Apple Car Play"
                            Optional
                            []
                        ]
                    ]
                , Feature
                    (FeatureID "feature:comfort-systems")
                    "Comfort Systems"
                    Optional
                    [ Group
                        (GroupID "group:comfort1")
                        And
                        [ Feature
                            (FeatureID "feature:parking-pilot")
                            "Parking Pilot"
                            Optional
                            []
                        ]
                    ]
                ]
            ]
        )
    )
  ,
    ( TP 5
    , FeatureModel
        ( Feature
            (FeatureID "feature:car")
            "Car"
            Mandatory
            [ Group
                (GroupID "group:car1")
                And
                [ Feature
                    (FeatureID "feature:infotainment")
                    "Infotainment System"
                    Mandatory
                    [ Group
                        (GroupID "group:info1")
                        And
                        [ Feature
                            (FeatureID "feature:bluetooth")
                            "Bluetooth"
                            Optional
                            []
                        ]
                    , Group
                        (GroupID "group:info2")
                        Or
                        [ Feature
                            (FeatureID "feature:android-auto")
                            "Android Auto"
                            Optional
                            []
                        , Feature
                            (FeatureID "feature:car-play")
                            "Apple Car Play"
                            Optional
                            []
                        ]
                    ]
                , Feature
                    (FeatureID "feature:comfort-systems")
                    "Comfort Systems"
                    Optional
                    [ Group
                        (GroupID "group:comfort1")
                        And
                        [ Feature
                            (FeatureID "feature:parking-pilot")
                            "Parking Pilot"
                            Optional
                            [ Group
                                (GroupID "group:pilot1")
                                And
                                [ Feature
                                    (FeatureID "feature:distance-sensors")
                                    "Distance Sensors"
                                    Mandatory
                                    []
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        )
    )
  ,
    ( TP 6
    , FeatureModel
        ( Feature
            (FeatureID "feature:car")
            "Car"
            Mandatory
            [ Group
                (GroupID "group:car1")
                And
                [ Feature
                    (FeatureID "feature:infotainment")
                    "Infotainment System"
                    Mandatory
                    [ Group
                        (GroupID "group:info1")
                        And
                        [ Feature
                            (FeatureID "feature:bluetooth")
                            "Bluetooth"
                            Optional
                            []
                        ]
                    , Group
                        (GroupID "group:info2")
                        Or
                        [ Feature
                            (FeatureID "feature:android-auto")
                            "Android Auto"
                            Optional
                            []
                        , Feature
                            (FeatureID "feature:car-play")
                            "Apple Car Play"
                            Optional
                            []
                        ]
                    ]
                , Feature
                    (FeatureID "feature:comfort-systems")
                    "Assistance Systems"
                    Optional
                    [ Group
                        (GroupID "group:comfort1")
                        And
                        [ Feature
                            (FeatureID "feature:parking-pilot")
                            "Parking Pilot"
                            Optional
                            [ Group
                                (GroupID "group:pilot1")
                                And
                                []
                            ]
                        , Feature
                            (FeatureID "feature:emergency-brake")
                            "Emergency Brake"
                            Optional
                            []
                        ]
                    ]
                , Feature
                    (FeatureID "feature:sensors")
                    "Sensors"
                    Optional
                    [ Group
                        (GroupID "group:sensors1")
                        And
                        [ Feature
                            (FeatureID "feature:distance-sensors")
                            "Distance Sensors"
                            Mandatory
                            []
                        ]
                    ]
                ]
            ]
        )
    )
  ,
    ( TP 7
    , FeatureModel
        ( Feature
            (FeatureID "feature:car")
            "Car"
            Mandatory
            [ Group
                (GroupID "group:car1")
                And
                [ Feature
                    (FeatureID "feature:infotainment")
                    "Infotainment System"
                    Mandatory
                    [ Group
                        (GroupID "group:info1")
                        And
                        []
                    , Group
                        (GroupID "group:info2")
                        Or
                        [ Feature
                            (FeatureID "feature:android-auto")
                            "Android Auto"
                            Optional
                            []
                        , Feature
                            (FeatureID "feature:car-play")
                            "Apple Car Play"
                            Optional
                            []
                        ]
                    ]
                , Feature
                    (FeatureID "feature:comfort-systems")
                    "Assistance Systems"
                    Optional
                    [ Group
                        (GroupID "group:comfort1")
                        And
                        [ Feature
                            (FeatureID "feature:parking-pilot")
                            "Parking Pilot"
                            Optional
                            [ Group
                                (GroupID "group:pilot1")
                                And
                                []
                            ]
                        , Feature
                            (FeatureID "feature:emergency-brake")
                            "Emergency Brake"
                            Optional
                            []
                        ]
                    ]
                , Feature
                    (FeatureID "feature:sensors")
                    "Sensors"
                    Optional
                    [ Group
                        (GroupID "group:sensors1")
                        And
                        [ Feature
                            (FeatureID "feature:front-sensors")
                            "FrontSensors"
                            Optional
                            []
                        , Feature
                            (FeatureID "feature:rear-sensors")
                            "Rear Sensors"
                            Optional
                            []
                        ]
                    ]
                ]
            ]
        )
    )
  ]
