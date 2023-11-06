module Types exposing (Choice(..), ComplicationKind(..), Display(..), Model, Msg(..), RankedComplication, RankedMagic, RankedPerk, RankedRelic, complicationKindToString, complicationToCategory, factionToMagic, gainToSlot, nextDisplay)

import Browser exposing (UrlRequest)
import Browser.Navigation
import Generated.Types exposing (Class, Companion, Complication(..), ComplicationCategory(..), Faction(..), GameMode, Magic, Perk, Race, Relic, Slot(..))


type Msg
    = UrlClicked UrlRequest
    | UrlChanged --Url.Url
    | Choice Choice
    | OpenMenu
    | CloseMenu
    | ScrollTo String
    | Nop


type Choice
    = ChoiceClass (Maybe Class)
    | DisplayClass Display
    | ChoiceRace (Maybe Race)
    | DisplayRace Display
    | ChoiceGameMode (Maybe GameMode)
    | DisplayGameMode Display
    | ChoiceComplication RankedComplication Bool
    | DisplayComplications Display
    | ChoiceTypePerk Race Bool
    | DisplayTypePerks Display
    | ChoiceMagic RankedMagic Bool
    | DisplayMagic Display
    | ChoicePerk RankedPerk Bool
    | DisplayPerks Display
    | ChoiceFaction (Maybe ( Faction, Bool ))
    | DisplayFaction Display
    | ChoiceRelic RankedRelic Bool
    | DisplayRelics Display
    | ChoiceCompanion Companion Bool
    | DisplayCompanions Display
    | TowardsCap Int


type alias Model =
    { key : Browser.Navigation.Key
    , towardsCap : Int
    , menuOpen : Bool
    , class : Maybe Class
    , classDisplay : Display
    , race : Maybe Race
    , raceDisplay : Display
    , gameMode : Maybe GameMode
    , gameModeDisplay : Display
    , complications : List RankedComplication
    , complicationsDisplay : Display
    , typePerks : List Race
    , typePerksDisplay : Display
    , magic : List RankedMagic
    , magicDisplay : Display
    , perks : List RankedPerk
    , perksDisplay : Display
    , faction : Maybe ( Faction, Bool )
    , factionDisplay : Display
    , companions : List Companion
    , companionsDisplay : Display
    , relics : List RankedRelic
    , relicsDisplay : Display
    }


type Display
    = DisplayFull
    | DisplayCompact
    | DisplayCollapsed


type alias RankedComplication =
    { name : Complication
    , kind : ComplicationKind
    }


type ComplicationKind
    = Tiered Int
    | Nontiered


type alias RankedMagic =
    { name : Magic
    , rank : Int
    }


type alias RankedPerk =
    { name : Perk
    , cost : Int
    }


type alias RankedRelic =
    { name : Relic
    , cost : Int
    }


complicationKindToString : ComplicationKind -> String
complicationKindToString kind =
    case kind of
        Tiered i ->
            String.fromInt i

        Nontiered ->
            ""


complicationToCategory : Complication -> Maybe ComplicationCategory
complicationToCategory name =
    case name of
        Brutality ->
            Just WorldShift

        Masquerade ->
            Just WorldShift

        TrueNames ->
            Just WorldShift

        Monsters ->
            Just WorldShift

        Population ->
            Just WorldShift

        Bonk ->
            Just WorldShift

        _ ->
            Nothing


gainToSlot : Int -> Slot
gainToSlot gain =
    if gain <= 4 then
        Folk

    else if gain <= 8 then
        Noble

    else if gain <= 12 then
        Heroic

    else
        Epic


factionToMagic : Faction -> String
factionToMagic faction =
    case faction of
        TheCollegeOfArcadia ->
            "Digicasting"

        HawthorneAcademia ->
            "Wands"

        TheWatchers ->
            "Ministrations"

        TheHespatianCoven ->
            "Occultism"

        Lunabella ->
            "Dominion"

        AlfheimrAlliance ->
            "Covenants"

        TheOutsiders ->
            "Monstrosity"

        TheORC ->
            "Gadgetry"

        AlphazonIndustries ->
            "Integration"


nextDisplay : Display -> Display
nextDisplay display =
    case display of
        DisplayFull ->
            DisplayCompact

        DisplayCompact ->
            DisplayCollapsed

        DisplayCollapsed ->
            DisplayFull
