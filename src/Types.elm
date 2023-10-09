module Types exposing (Choice(..), Complication, ComplicationKind(..), Model, complicationKindToString, complicationNameToCategory, complicationNameToImage)

import Browser.Navigation
import Generated.Types exposing (Class, ComplicationCategory(..), ComplicationName(..), Race)
import Images exposing (Image)


type Choice
    = ChoiceClass (Maybe Class)
    | ChoiceRace (Maybe Race)
    | ChoiceComplication Complication Bool


type alias Model =
    { key : Browser.Navigation.Key
    , class : Maybe Class
    , race : Maybe Race
    , complications : List Complication
    }


type alias Complication =
    { name : ComplicationName
    , kind : ComplicationKind
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


complicationNameToCategory : ComplicationName -> ComplicationCategory
complicationNameToCategory name =
    case name of
        Brutality ->
            WorldShift

        Masquerade ->
            WorldShift

        TrueNames ->
            WorldShift


complicationNameToImage : ComplicationName -> Image
complicationNameToImage name =
    case name of
        Brutality ->
            Images.complicationBrutality

        _ ->
            let
                _ =
                    Debug.todo
            in
            Images.complicationBrutality
