module Data.Companion exposing (Details, MaybeClass(..), Score(..), factionToCollectiveName)

import Generated.Types exposing (Class, Companion, Faction(..), Race)


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


factionToCollectiveName : Maybe Faction -> String
factionToCollectiveName faction =
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
