module Types exposing (Choice(..), Complication, ComplicationKind(..), Model, Msg(..), RankedMagic, RankedPerk, complicationKindToString, complicationNameToCategory, factionToMagic, gainToSlot)

import Browser exposing (UrlRequest)
import Browser.Navigation
import Generated.Types exposing (Class, ComplicationCategory(..), ComplicationName(..), Faction(..), GameMode, Magic, Perk, Race, Slot(..))


type Msg
    = UrlClicked UrlRequest
    | UrlChanged --Url.Url
    | Choice Choice
    | ToggleMenu
    | ScrollTo String
    | Nop


type Choice
    = ChoiceClass (Maybe Class)
    | ChoiceRace (Maybe Race)
    | ChoiceGameMode (Maybe GameMode)
    | ChoiceComplication Complication Bool
    | ChoiceTypePerk Race Bool
    | ChoiceMagic RankedMagic Bool
    | ChoicePerk RankedPerk Bool
    | ChoiceFaction (Maybe Faction)
    | TowardsCap Int


type alias Model =
    { key : Browser.Navigation.Key
    , towardsCap : Int
    , menuOpen : Bool
    , class : Maybe Class
    , race : Maybe Race
    , gameMode : Maybe GameMode
    , complications : List Complication
    , typePerks : List Race
    , magic : List RankedMagic
    , perks : List RankedPerk
    , faction : Maybe Faction
    }


type alias Complication =
    { name : ComplicationName
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


complicationKindToString : ComplicationKind -> String
complicationKindToString kind =
    case kind of
        Tiered i ->
            String.fromInt i

        Nontiered ->
            ""


complicationNameToCategory : ComplicationName -> Maybe ComplicationCategory
complicationNameToCategory name =
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
        Arcadia ->
            "Digicasting"

        Hawthorne ->
            "Wands"

        Watchers ->
            "Ministrations"

        Hespatian ->
            "Occultism"

        Lunabella ->
            "Dominion"
