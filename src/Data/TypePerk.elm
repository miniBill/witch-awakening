module Data.TypePerk exposing (Details, title)

import Generated.Types exposing (Race)
import Types exposing (RankedMagic)


type alias Details =
    { race : Race
    , name : Maybe String
    , gain : List RankedMagic
    , cost : Int
    , content : String
    , dlc : Maybe String
    }


title : String
title =
    "# Type Perks"
