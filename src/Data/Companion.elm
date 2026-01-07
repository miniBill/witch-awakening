module Data.Companion exposing (MaybeClass(..), Score(..))

import Generated.Types exposing (Class)


type MaybeClass
    = ClassAny
    | ClassOne Class
    | ClassSpecial
    | ClassNone


type Score
    = NormalScore Int
    | SpecialEffect { better : Int, worse : Maybe Int }
