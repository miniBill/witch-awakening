module Data.Class exposing (Details)

import Generated.Types exposing (Class)


type alias Details =
    { name : Class
    , dlc : Maybe String
    , color : Int
    , content : String
    }
