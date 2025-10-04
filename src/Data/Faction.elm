module Data.Faction exposing (Details, toShortString)

import Generated.Types exposing (Faction(..))
import Images exposing (Image)


type alias Details =
    { name : Faction
    , motto : String
    , isHuman : Bool
    , description : String
    , location : String
    , relations : String
    , perk : String
    , perkContent : String
    , dlc : Maybe String
    , images : { image1 : Image, image2 : Image, image3 : Image, image4 : Image, image5 : Image }
    }


toShortString : Faction -> String
toShortString raw =
    case raw of
        FactionTheCollegeOfArcadia ->
            "Arcadia"

        FactionHawthorneAcademia ->
            "Hawthorne"

        FactionTheWatchers ->
            "Watchers"

        FactionTheHespatianCoven ->
            "Hespatia"

        FactionLunabella ->
            "Lunabella"

        FactionAlfheimrAlliance ->
            "Alliance"

        FactionTheOutsiders ->
            "Outsiders"

        FactionTheORC ->
            "ORC"

        FactionAlphazonIndustries ->
            "Alphazon"

        FactionIndependents ->
            "Independent"

        FactionTheLodge ->
            "Lodge"
