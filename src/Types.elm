module Types exposing (Affinity(..), Choice(..), Class(..), Model, Race(..), Size(..), affinityToString, classToString, raceToString, sizeToString)

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
    | Daeva
    | Ifrit


raceToString : Race -> String
raceToString race =
    case race of
        Neutral ->
            "Neutral"

        Daeva ->
            "Daeva"

        Ifrit ->
            "Ifrit"


type Size
    = Low
    | Medium
    | High


sizeToString : Size -> String
sizeToString size =
    case size of
        Low ->
            "Low"

        Medium ->
            "Med"

        High ->
            "High"


type Affinity
    = All
    | Beast
    | Blood
    | Body
    | Fire
    | Life
    | Metal
    | Mind
    | Nature
    | Necro
    | Soul
    | Wind


affinityToString : Affinity -> String
affinityToString affinity =
    case affinity of
        All ->
            "???"

        Beast ->
            "Beast"

        Blood ->
            "Blood"

        Body ->
            "Body"

        Fire ->
            "Fire"

        Life ->
            "Life"

        Metal ->
            "Metal"

        Mind ->
            "Mind"

        Nature ->
            "Nature"

        Necro ->
            "Necro"

        Soul ->
            "Soul"

        Wind ->
            "Wind"
