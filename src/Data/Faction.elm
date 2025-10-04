module Data.Faction exposing (Details, toCollectiveName, toShortString)

import Generated.Image exposing (Image)
import Generated.Types exposing (Faction(..))


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


toCollectiveName : Maybe Faction -> String
toCollectiveName faction =
    case faction of
        Just FactionTheCollegeOfArcadia ->
            "The Arcadians"

        Just FactionHawthorneAcademia ->
            "Hawthorne"

        Just FactionTheWatchers ->
            "The Watchers"

        Just FactionTheHespatianCoven ->
            "The Hespatians"

        Just FactionLunabella ->
            "The Lunabellans"

        Just FactionAlfheimrAlliance ->
            "Alliance"

        Just FactionTheOutsiders ->
            "Outsiders"

        Just FactionTheORC ->
            "The ORCs / Badges"

        Just FactionAlphazonIndustries ->
            "The Alphazonians / Suits"

        Just FactionIndependents ->
            "Independents / Other"

        Just FactionTheLodge ->
            "Lodge"

        Nothing ->
            "Independents / Other"
