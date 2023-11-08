module Types exposing (Choice(..), ComplicationKind(..), CosmicPearlData, Display(..), Model, Msg(..), RankedComplication, RankedMagic, RankedPerk, RankedRelic, affinities, allAffinities, baseAffinities, complicationKindToString, complicationToCategory, factionToMagic, gainToSlot, nextDisplay)

import Browser exposing (UrlRequest)
import Browser.Navigation
import Data.Race as Race
import Generated.Types exposing (Affinity(..), Class, Companion, Complication(..), ComplicationCategory(..), Faction(..), GameMode, Magic, Perk, Race, Relic, Slot(..))
import List.Extra


type Msg
    = UrlClicked UrlRequest
    | UrlChanged --Url.Url
    | Choice Choice
    | OpenMenu
    | CloseMenu
    | ScrollTo String
    | CompactAll
    | ExpandAll
    | Nop


type Choice
    = ChoiceClass (Maybe Class)
    | DisplayClass Display
    | ChoiceRace ( Race, Bool )
    | ChoiceMainRace (Maybe Race)
    | DisplayRace Display
    | ChoiceGameMode (Maybe GameMode)
    | DisplayGameMode Display
    | ChoiceComplication ( RankedComplication, Bool )
    | DisplayComplications Display
    | ChoiceTypePerk ( Race, Bool )
    | DisplayTypePerks Display
    | ChoiceMagic ( RankedMagic, Bool )
    | DisplayMagic Display
    | ChoicePerk ( RankedPerk, Bool )
    | DisplayPerks Display
    | ChoiceFaction (Maybe ( Faction, Bool ))
    | DisplayFaction Display
    | DisplayFactionalMagic Display
    | ChoiceCompanion ( Companion, Bool )
    | DisplayCompanions Display
    | ChoiceRelic ( RankedRelic, Bool )
    | DisplayRelics Display
    | ChoiceCosmicPearl CosmicPearlData
    | TowardsCap Int
    | PowerToRewards Int
    | ChoiceCapBuild Bool


type alias CosmicPearlData =
    { change : List ( Affinity, Affinity )
    , add : List Affinity
    }


type alias Model =
    { key : Browser.Navigation.Key
    , capBuild : Bool
    , towardsCap : Int
    , powerToRewards : Int
    , menuOpen : Bool
    , class : Maybe Class
    , classDisplay : Display
    , races : List Race
    , mainRace : Maybe Race
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
    , factionalMagicDisplay : Display
    , companions : List Companion
    , companionsDisplay : Display
    , relics : List RankedRelic
    , relicsDisplay : Display
    , cosmicPearl : CosmicPearlData
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


affinities : Model -> List Affinity
affinities { races, mainRace, cosmicPearl } =
    let
        base : List Affinity
        base =
            case ( mainRace, races ) of
                ( Just race, _ ) ->
                    baseAffinities race

                ( Nothing, [ race ] ) ->
                    baseAffinities race

                _ ->
                    []

        afterChange : List Affinity
        afterChange =
            List.foldl
                (\( from, to ) acc -> to :: List.Extra.remove from acc)
                base
                cosmicPearl.change
    in
    (afterChange ++ cosmicPearl.add)
        |> (::) All
        |> List.Extra.unique


baseAffinities : Race -> List Affinity
baseAffinities race =
    Race.all
        |> List.Extra.find (\{ name } -> name == race)
        |> Maybe.map .affinities
        |> Maybe.withDefault []


allAffinities : List Affinity
allAffinities =
    [ Beast, Blood, Body, Earth, Fire, Life, Metal, Mind, Nature, Necro, Soul, Water, Wind ]
