module Types exposing (Choice(..), Class(..), Model, Race(..), classToString, raceToString)

import Browser.Navigation


type Choice
    = Class (Maybe Class)
    | Race (Maybe Race)


type alias Model =
    { key : Browser.Navigation.Key
    , class : Maybe Class
    , race : Maybe Race
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


type Race
    = Neutral


raceToString : Race -> String
raceToString race =
    case race of
        Neutral ->
            "Neutral"
