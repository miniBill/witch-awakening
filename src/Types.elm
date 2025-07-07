module Types exposing (Choice(..), ComplicationKind(..), CosmicPearlData, Display(..), Model, Msg(..), RankedComplication, RankedMagic, RankedPerk, RankedRelic, complicationKindToString, gainToSlot, nextDisplay)

import Browser exposing (UrlRequest)
import Generated.Types exposing (Affinity, Class, Companion, Complication, Faction, GameMode, Magic, Perk, Quest, Race, Relic, Slot(..))
import Set exposing (Set)


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
    | ChoiceQuest ( Quest, Bool )
    | DisplayQuests Display
    | ChoiceRelic ( RankedRelic, Bool )
    | DisplayRelics Display
    | ChoiceCosmicPearl CosmicPearlData
    | TowardsCap Int
    | PowerToRewards Int
    | ChoiceCapBuild Bool
    | ToggleMenuSectionExpansion String


type alias CosmicPearlData =
    { change : List ( Affinity, Affinity )
    , add : List Affinity
    }


type alias Model key =
    { key : key
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
    , quests : List Quest
    , questsDisplay : Display
    , relics : List RankedRelic
    , relicsDisplay : Display
    , cosmicPearl : CosmicPearlData
    , expandedMenuSections : Set String
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


nextDisplay : Display -> Display
nextDisplay display =
    case display of
        DisplayFull ->
            DisplayCompact

        DisplayCompact ->
            DisplayCollapsed

        DisplayCollapsed ->
            DisplayFull
