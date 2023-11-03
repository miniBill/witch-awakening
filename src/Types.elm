module Types exposing (Choice(..), ComplicationKind(..), Model, Msg(..), RankedComplication, RankedMagic, RankedPerk, RankedRelic, complicationKindToString, complicationToCategory, factionToMagic, gainToSlot)

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
    | ChoiceRace (Maybe Race)
    | ChoiceGameMode (Maybe GameMode)
    | ChoiceComplication RankedComplication Bool
    | ChoiceTypePerk Race Bool
    | ChoiceMagic RankedMagic Bool
    | ChoicePerk RankedPerk Bool
    | ChoiceFaction (Maybe ( Faction, Bool ))
    | ChoiceRelic RankedRelic Bool
    | TowardsCap Int
    | ChoiceCompanion Companion Bool


type alias Model =
    { key : Browser.Navigation.Key
    , towardsCap : Int
    , menuOpen : Bool
    , class : Maybe Class
    , race : Maybe Race
    , gameMode : Maybe GameMode
    , complications : List RankedComplication
    , typePerks : List Race
    , magic : List RankedMagic
    , perks : List RankedPerk
    , faction : Maybe ( Faction, Bool )
    , companions : List Companion
    , relics : List RankedRelic
    }


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
