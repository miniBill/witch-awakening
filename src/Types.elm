module Types exposing (Choice(..), Complication, ComplicationKind(..), Model, Msg(..), RankedMagic, complicationKindToString, complicationNameToCategory, gainToSlot)

import Browser exposing (UrlRequest)
import Browser.Navigation
import Generated.Types exposing (Class, ComplicationCategory(..), ComplicationName(..), GameMode, Magic, Race, Slot(..))


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
    }


type alias Complication =
    { name : ComplicationName
    , kind : ComplicationKind
    }


type alias RankedMagic =
    { name : Magic
    , rank : Int
    }


type ComplicationKind
    = Tiered Int
    | Nontiered


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
