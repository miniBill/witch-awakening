module Main exposing (Flags, main)

import Browser exposing (UrlRequest(..))
import Browser.Dom
import Browser.Events
import Browser.Navigation as Nav
import Data.Perk
import Dict
import Element exposing (Element, fill, rgb, width)
import Element.Font as Font
import Element.Lazy
import Generated.Image as Image
import Generated.Magic as Magic
import Generated.TypePerk as TypePerk
import Generated.Types as Types exposing (Perk(..))
import Json.Decode as JD
import List.Extra
import Set
import Set.Extra
import String.Extra
import Task
import Theme
import Types exposing (Choice(..), Display(..), Model, Msg(..))
import Url
import UrlCodec
import View.Class as Class
import View.Companion as Companion
import View.Complication as Complications
import View.Faction as Faction
import View.FactionalMagic as FactionalMagic
import View.GameMode as GameMode
import View.Intro as Intro
import View.Magic as Magic
import View.Menu as Menu
import View.Perk as Perk
import View.Quest as Quest
import View.Race as Race
import View.Relic as Relic
import View.TypePerk as TypePerk


type alias Flags =
    {}


main : Program Flags (Model Nav.Key) Msg
main =
    Browser.application
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = \_ -> UrlChanged
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : flags -> Url.Url -> Nav.Key -> ( Model Nav.Key, Cmd Msg )
init _ url key =
    ( UrlCodec.parseUrl key url
    , Cmd.none
    )


update : Msg -> Model Nav.Key -> ( Model Nav.Key, Cmd Msg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged ->
            ( model, Cmd.none )

        Choice choice ->
            let
                newModel : Model Nav.Key
                newModel =
                    updateOnChoice choice model
                        |> fixupModel
            in
            ( newModel
            , Nav.replaceUrl model.key (UrlCodec.toUrl newModel)
            )

        OpenMenu ->
            ( { model | menuOpen = True }, Cmd.none )

        CloseMenu ->
            ( { model | menuOpen = False }, Cmd.none )

        ScrollTo kind id ->
            ( model
            , (Types.idKindToString kind ++ "-" ++ String.Extra.underscored id)
                |> Browser.Dom.getElement
                |> Task.andThen
                    (\{ element } ->
                        Browser.Dom.setViewport
                            0
                            (element.y - Theme.rhythm)
                    )
                |> Task.attempt (\_ -> Nop)
            )

        CompactAll ->
            ( { model
                | classDisplay = DisplayCompact
                , raceDisplay = DisplayCompact
                , gameModeDisplay = DisplayCompact
                , complicationsDisplay = DisplayCompact
                , typePerksDisplay = DisplayCompact
                , magicDisplay = DisplayCompact
                , perksDisplay = DisplayCompact
                , factionDisplay = DisplayCompact
                , factionalMagicDisplay = DisplayCompact
                , companionsDisplay = DisplayCompact
                , questsDisplay = DisplayCompact
                , relicsDisplay = DisplayCompact
              }
            , Cmd.none
            )

        ExpandAll ->
            ( { model
                | classDisplay = DisplayFull
                , raceDisplay = DisplayFull
                , gameModeDisplay = DisplayFull
                , complicationsDisplay = DisplayFull
                , typePerksDisplay = DisplayFull
                , magicDisplay = DisplayFull
                , perksDisplay = DisplayFull
                , factionDisplay = DisplayFull
                , factionalMagicDisplay = DisplayFull
                , companionsDisplay = DisplayFull
                , questsDisplay = DisplayFull
                , relicsDisplay = DisplayFull
              }
            , Cmd.none
            )

        ShowDLC name show ->
            if show then
                ( { model | hideDLCs = Set.remove name model.hideDLCs }, Cmd.none )

            else
                ( { model | hideDLCs = Set.insert name model.hideDLCs }, Cmd.none )

        HideMeta hideMeta ->
            ( { model | hideMeta = hideMeta }, Cmd.none )

        Nop ->
            ( model, Cmd.none )

        Reset ->
            let
                newModel : Model Nav.Key
                newModel =
                    model |> emptyModel
            in
            ( newModel
            , Nav.replaceUrl model.key (UrlCodec.toUrl newModel)
            )


emptyModel : Model key -> Model key
emptyModel model =
    { model
        | capBuild = False
        , towardsCap = 0
        , powerToRewards = 0
        , class = Nothing
        , races = []
        , mainRace = Nothing
        , gameMode = Nothing
        , complications = []
        , typePerks = []
        , magic = []
        , perks = []
        , factions = []
        , factionPerks = []
        , companions = []
        , quests = []
        , relics = []
    }


fixupModel : Model key -> Model key
fixupModel model =
    { model
        | mainRace =
            case model.races of
                [] ->
                    Nothing

                [ _ ] ->
                    Nothing

                _ ->
                    case model.mainRace of
                        Nothing ->
                            model.mainRace

                        Just m ->
                            if List.member m model.races then
                                model.mainRace

                            else
                                Nothing
        , perks =
            model.perks
                |> List.filter
                    (\{ name } ->
                        (name /= PerkHybridize)
                            && (name /= PerkMultiFactional)
                    )
                |> (if List.length model.races > 1 then
                        (::)
                            { name = PerkHybridize
                            , cost = Data.Perk.hybridizeCost * (List.length model.races - 1)
                            }

                    else
                        identity
                   )
                |> (if
                        (List.length model.factions > 1)
                            || (List.length model.factionPerks > 1)
                    then
                        (::)
                            { name = PerkMultiFactional
                            , cost = 10
                            }

                    else if List.any (\faction -> not (List.member faction model.factions)) model.factionPerks then
                        (::)
                            { name = PerkMultiFactional
                            , cost = 4
                            }

                    else
                        identity
                   )
    }


toggle : (b -> b -> Bool) -> Bool -> b -> List b -> List b
toggle isSame selected item list =
    if selected then
        item :: List.Extra.removeWhen (isSame item) list

    else
        List.Extra.remove item list


updateOnChoice : Choice -> Model key -> Model key
updateOnChoice choice model =
    case choice of
        ChoiceClass class ->
            { model | class = class }

        DisplayClass classDisplay ->
            { model | classDisplay = classDisplay }

        ChoiceRace ( race, selected ) ->
            { model | races = toggle Types.isSameRace selected race model.races }

        ChoiceMainRace mainRace ->
            { model | mainRace = mainRace }

        DisplayRace raceDisplay ->
            { model | raceDisplay = raceDisplay }

        ChoiceGameMode gameMode ->
            { model | gameMode = gameMode }

        DisplayGameMode gameModeDisplay ->
            { model | gameModeDisplay = gameModeDisplay }

        ChoiceComplication ( complication, selected ) ->
            { model | complications = toggle (onName (==)) selected complication model.complications }

        DisplayComplications complicationsDisplay ->
            { model | complicationsDisplay = complicationsDisplay }

        ChoiceTypePerk ( race, selected ) ->
            { model
                | typePerks = toggle Types.isSameRace selected race model.typePerks
                , magic =
                    if selected then
                        case
                            List.Extra.findMap
                                (\typePerk ->
                                    if typePerk.race == race then
                                        Just typePerk.gain

                                    else
                                        Nothing
                                )
                                TypePerk.all
                        of
                            Just gained ->
                                List.foldl
                                    (\magic -> Dict.insert (Magic.toString magic.name) magic)
                                    Dict.empty
                                    (gained ++ model.magic)
                                    |> Dict.values

                            Nothing ->
                                model.magic

                    else
                        model.magic
            }

        DisplayTypePerks typePerksDisplay ->
            { model | typePerksDisplay = typePerksDisplay }

        ChoiceMagic ( magic, selected ) ->
            { model | magic = toggle (onName (==)) selected magic model.magic }

        DisplayMagic magicDisplay ->
            { model | magicDisplay = magicDisplay }

        ChoicePerk ( perk, selected ) ->
            { model | perks = toggle (onName (==)) selected perk model.perks }

        DisplayPerks perksDisplay ->
            { model | perksDisplay = perksDisplay }

        ChoiceFaction ( faction, selected ) ->
            { model | factions = toggle (==) selected faction model.factions }

        ChoiceFactionPerk ( factionPerk, selected ) ->
            { model | factionPerks = toggle (==) selected factionPerk model.factionPerks }

        DisplayFaction factionDisplay ->
            { model | factionDisplay = factionDisplay }

        DisplayFactionalMagic factionalMagicDisplay ->
            { model | factionalMagicDisplay = factionalMagicDisplay }

        ChoiceCompanion ( companion, selected ) ->
            { model | companions = toggle Types.isSameCompanion selected companion model.companions }

        DisplayCompanions companionsDisplay ->
            { model | companionsDisplay = companionsDisplay }

        ChoiceQuest ( quest, selected ) ->
            { model | quests = toggle Types.isSameQuest selected quest model.quests }

        DisplayQuests questsDisplay ->
            { model | questsDisplay = questsDisplay }

        ChoiceRelic ( relic, selected ) ->
            { model | relics = toggle (onName Types.isSameRelic) selected relic model.relics }

        DisplayRelics relicsDisplay ->
            { model | relicsDisplay = relicsDisplay }

        ChoiceCapBuild capBuild ->
            { model | capBuild = capBuild }

        TowardsCap towardsCap ->
            { model | towardsCap = towardsCap }

        PowerToRewards powerToRewards ->
            { model | powerToRewards = powerToRewards }

        ToggleMenuSectionExpansion label ->
            { model | expandedMenuSections = Set.Extra.toggle label model.expandedMenuSections }


onName : (name -> name -> Bool) -> { a | name : name } -> { a | name : name } -> Bool
onName f arg1 arg2 =
    f arg1.name arg2.name


view : Model key -> Browser.Document Msg
view model =
    { title = "Witch Awakening"
    , body =
        [ Element.layoutWith
            { options =
                [ Element.focusStyle
                    { borderColor = Nothing
                    , backgroundColor = Nothing
                    , shadow = Nothing
                    }
                ]
            }
            [ Font.size 16
            , Element.inFront (Menu.viewMenu model)
            ]
            (innerView model)
        ]
    }


innerView : Model key -> Element Msg
innerView model =
    let
        allCompact : Bool
        allCompact =
            model.classDisplay /= DisplayFull && model.raceDisplay /= DisplayFull && model.gameModeDisplay /= DisplayFull && model.complicationsDisplay /= DisplayFull && model.typePerksDisplay /= DisplayFull && model.magicDisplay /= DisplayFull && model.perksDisplay /= DisplayFull && model.factionDisplay /= DisplayFull && model.factionalMagicDisplay /= DisplayFull && model.companionsDisplay /= DisplayFull && model.questsDisplay /= DisplayFull && model.relicsDisplay /= DisplayFull
    in
    Theme.column
        [ width fill
        , Font.color <| rgb 1 1 1
        , Theme.style "background" ("url(" ++ Image.pattern.src ++ ")")
        ]
        [ Intro.viewTitle allCompact
        , if allCompact then
            Element.none

          else
            Intro.viewIntro
        , Theme.column
            [ width fill
            , Theme.style "border-image-source" ("url(" ++ Image.border.src ++ ")")
            , Theme.style "border-image-slice" "60 30 0"
            , Theme.style "border-image-width" "60px 30px 0"
            , Theme.style "border-image-repeat" "space round"
            , Theme.style "border-width" "0 30px"
            ]
            [ Theme.column
                [ if allCompact then
                    Element.paddingEach { top = 0, left = 16, right = 16, bottom = 16 }

                  else
                    Element.paddingXY 16 0
                ]
                [ Element.Lazy.lazy3 Class.viewClass model.hideDLCs model.classDisplay model.class
                , Element.Lazy.lazy3 Race.viewRace model.hideDLCs model.raceDisplay model.races
                , Element.Lazy.lazy3 GameMode.viewGameMode model.hideDLCs model.gameModeDisplay model.gameMode
                , Element.Lazy.lazy3 Complications.viewComplications model.hideDLCs model.complicationsDisplay model.complications
                , Element.Lazy.lazy4 TypePerk.viewTypePerks model.hideDLCs model.races model.typePerksDisplay model.typePerks
                , Element.Lazy.lazy3 Magic.viewMagics model.hideDLCs model.magicDisplay model.magic
                , Element.Lazy.lazy6 Perk.viewPerks model.hideDLCs model.hideMeta model.perksDisplay model.mainRace model.races model.perks
                , Element.Lazy.lazy4 Faction.viewFaction model.hideDLCs model.factionDisplay model.factions model.factionPerks
                , Element.Lazy.lazy3 FactionalMagic.viewFactionalMagics model.hideDLCs model.factionalMagicDisplay model.magic
                , Element.Lazy.lazy3 Companion.viewCompanions model.hideDLCs model.companionsDisplay model.companions
                , Element.Lazy.lazy3 Quest.viewQuests model.hideDLCs model.questsDisplay model.quests
                , Element.Lazy.lazy6 Relic.viewRelics model.hideDLCs model.relicsDisplay model.mainRace model.races model.typePerks model.relics
                ]
            ]
        ]
        |> Element.map Choice


subscriptions : Model key -> Sub Msg
subscriptions _ =
    Browser.Events.onKeyUp keyDecoder


keyDecoder : JD.Decoder Msg
keyDecoder =
    JD.field "key" JD.string
        |> JD.andThen
            (\key ->
                case key of
                    "Escape" ->
                        JD.succeed CloseMenu

                    _ ->
                        JD.fail "ignored"
            )
