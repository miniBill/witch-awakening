module Data.Complication exposing (Content(..), Details)

import Generated.Types exposing (Class, Complication, ComplicationCategory)


type alias Details =
    { name : Complication
    , class : Maybe Class
    , content : Content
    , category : Maybe ComplicationCategory
    , dlc : Maybe String
    }


type Content
    = WithTiers String (List ( String, Int )) String
    | Single Int String
    | WithChoices String (List ( String, Int )) String
    | WithGains (List Int) String
