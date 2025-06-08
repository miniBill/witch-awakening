module Data.Magic exposing (Affinities(..), Details)

import Generated.Types exposing (Affinity, Class, Faction, Magic)


type alias Details =
    { name : Magic
    , hasRankZero : Bool
    , class : Maybe Class
    , faction : Maybe Faction
    , affinities : Affinities
    , isElementalism : Bool
    , description : String
    , ranks : List String
    , dlc : Maybe String
    }


type Affinities
    = Regular (List Affinity)
    | Alternative (List (List Affinity))
