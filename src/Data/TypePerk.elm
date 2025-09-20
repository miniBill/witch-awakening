module Data.TypePerk exposing (Details, title)

import Generated.Types exposing (Race)


type alias Details =
    { race : Race
    , name : Maybe String
    , cost : Int
    , content : String
    , dlc : Maybe String
    }


title : String
title =
    "# Type Perks"
