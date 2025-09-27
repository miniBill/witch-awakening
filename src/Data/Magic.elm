module Data.Magic exposing (Affinities(..), Details, MaybeClass(..))

import Generated.Types exposing (Affinity, Class, Faction, Magic)


type alias Details =
    { name : Magic
    , hasRankZero : Bool
    , class : MaybeClass
    , faction : Maybe Faction
    , affinities : Affinities
    , isElementalism : Bool
    , description : String
    , ranks : List String
    , dlc : Maybe String
    }


type MaybeClass
    = ClassOne Class
    | ClassSpecial
    | ClassNone


type Affinities
    = Regular (List Affinity)
    | Alternative (List (List Affinity))
