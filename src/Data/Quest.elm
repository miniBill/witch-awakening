module Data.Quest exposing (Details, Evil(..))

import Generated.Types exposing (Faction, Quest, Slot)


type alias Details =
    { name : Quest
    , evil : Evil
    , repeatable : Bool
    , slot : Slot
    , faction : Maybe Faction
    , threat : Maybe Int
    , conflict : Maybe Int
    , reward : Int
    , description : String
    , notes : List String
    , sidebars : List String
    , dlc : Maybe String
    }


type Evil
    = EvilYes
    | EvilMaybe
    | EvilNo
