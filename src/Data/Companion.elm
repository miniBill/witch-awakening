module Data.Companion exposing (Details, MaybeClass(..), Score(..), factionToCollectiveName, intro)

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


intro : String
intro =
    """
    It’s dangerous to go alone, take this! A Companions section. {choice You have one free companion from your chosen faction, and one free that shares the same witch type} as you (either Race or Class). Companions have 4 main statistics giving a _rough idea_ of their general impact in a group dynamic, except the {choice POWER stat, which primarily serves to give free floating Power points to spend to _customize_ your companion’s abilities} on top of the default abilities they come with. Beyond that, interactions are inferred in how you interpret their personality and how different stats play out in relation to it and others. {choice You can buy additional companions using Reward Points}, listed to the left of their name. A {choice +} indicates they have their Type Perk.

    {choice You can spend your own Power on behalf of a member if you so choose, although they receive 1/2 the Reward value of Quests, so you don’t _need to_.}

    **Really, don’t stress the details too much and feel free to fill in the blanks. I do what I can to provide a reasonable summary of an array of characters, but it’s up to you to interpret it as you will.**

    Take your pick:
    """


factionToCollectiveName : Maybe Faction -> String
factionToCollectiveName faction =
    case faction of
        Just TheCollegeOfArcadia ->
            "The Arcadians"

        Just HawthorneAcademia ->
            "Hawthorne"

        Just TheWatchers ->
            "The Watchers"

        Just TheHespatianCoven ->
            "The Hespatians"

        Just Lunabella ->
            "The Lunabellans"

        Just AlfheimrAlliance ->
            "Alliance"

        Just TheOutsiders ->
            "Outsiders"

        Just TheORC ->
            "The ORCs / Badges"

        Just AlphazonIndustries ->
            "The Alphazonians / Suits"

        Just Independents ->
            "Independents / Other"

        Nothing ->
            "Independents / Other"
