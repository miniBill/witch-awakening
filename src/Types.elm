module Types exposing (Choice(..), Model)

import Browser.Navigation
import Generated.Types exposing (Class, Race)


type Choice
    = Class (Maybe Class)
    | Race (Maybe Race)


type alias Model =
    { key : Browser.Navigation.Key
    , class : Maybe Class
    , race : Maybe Race
    }
