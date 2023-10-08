module Types exposing (Class(..), Model, classToString)

import Browser.Navigation


type alias Model =
    { key : Browser.Navigation.Key
    , class : Maybe Class
    }


type Class
    = Academic
    | Sorceress
    | Warlock


classToString : Class -> String
classToString class =
    case class of
        Academic ->
            "Academic"

        Sorceress ->
            "Sorceress"

        Warlock ->
            "Warlock"
