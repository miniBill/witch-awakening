module Data.Companion exposing (Details, MaybeClass(..), Score(..))

import Generated.Types exposing (Class, Companion, Race)


type alias Details =
    { name : Companion
    , class : MaybeClass
    , races : List Race
    , hasPerk : Bool
    , cost : Maybe Int
    , power : Score
    , teamwork : Score
    , sociability : Score
    , morality : Score
    , quote : String
    , description : String
    , positives : List String
    , negatives : List String
    , mixed : List String
    , has : String
    , dlc : Maybe String
    }


type MaybeClass
    = ClassAny
    | ClassOne Class
    | ClassSpecial
    | ClassNone


type Score
    = NormalScore Int
    | SpecialEffect { better : Int, worse : Maybe Int }
