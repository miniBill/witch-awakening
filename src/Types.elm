module Types exposing (Choice(..), Complication, ComplicationKind(..), Model, complicationKindToString, complicationNameToCategory)

import Browser.Navigation
import Generated.Types exposing (Class, ComplicationCategory(..), ComplicationName(..), Race)


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

        StoryArc ->
            Just GameMode

        EarlyBird ->
            Just GameMode

        SkillTree ->
            Just GameMode

        Constellation ->
            Just GameMode

        _ ->
            Nothing
