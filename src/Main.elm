module Main exposing (Flags, main)

import AppUrl exposing (AppUrl)
import Browser exposing (UrlRequest(..))
import Browser.Dom
import Browser.Events
import Browser.Navigation as Nav
import Data.Perk
import Dict
import Element exposing (Element, fill, rgb, width)
import Element.Background as Background
import Element.Font as Font
import Element.Lazy
import Generated.Types as Types exposing (Perk(..), Relic(..))
import Json.Decode as JD
import List.Extra
import Maybe.Extra
import Task
import Theme
import Types exposing (Choice(..), Display(..), Model, Msg(..))
import Url
import Url.Builder exposing (QueryParameter)
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
    ( parseUrl key url
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
            , Nav.replaceUrl model.key (toUrl newModel)
            )

        OpenMenu ->
            ( { model | menuOpen = True }, Cmd.none )

        CloseMenu ->
            ( { model | menuOpen = False }, Cmd.none )

        ScrollTo id ->
            ( model
            , Browser.Dom.getElement id
                |> Task.andThen
                    (\{ element } ->
                        Browser.Dom.setViewport
                            0
                            (element.y - Theme.rythm)
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
                , relicsDisplay = DisplayFull
              }
            , Cmd.none
            )

        Nop ->
            ( model, Cmd.none )


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
        , cosmicPearl =
            if List.any (\{ name } -> name == CosmicPearl) model.relics then
                model.cosmicPearl

            else
                { add = []
                , change = []
                }
        , perks =
            let
                removed : List { cost : Int, name : Perk }
                removed =
                    List.filter (\{ name } -> name /= Hybridize) model.perks
            in
            if List.length model.races > 1 then
                { name = Hybridize
                , cost = Data.Perk.hybridizeCost * (List.length model.races - 1)
                }
                    :: removed

            else
                removed
    }


toggle : Bool -> b -> List b -> List b
toggle selected item list =
    if selected then
        item :: list

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
            { model | races = toggle selected race model.races }

        ChoiceMainRace mainRace ->
            { model | mainRace = mainRace }

        DisplayRace raceDisplay ->
            { model | raceDisplay = raceDisplay }

        ChoiceGameMode gameMode ->
            { model | gameMode = gameMode }

        DisplayGameMode gameModeDisplay ->
            { model | gameModeDisplay = gameModeDisplay }

        ChoiceComplication ( complication, selected ) ->
            { model | complications = toggle selected complication model.complications }

        DisplayComplications complicationsDisplay ->
            { model | complicationsDisplay = complicationsDisplay }

        ChoiceTypePerk ( race, selected ) ->
            { model | typePerks = toggle selected race model.typePerks }

        DisplayTypePerks typePerksDisplay ->
            { model | typePerksDisplay = typePerksDisplay }

        ChoiceMagic ( magic, selected ) ->
            { model | magic = toggle selected magic model.magic }

        DisplayMagic magicDisplay ->
            { model | magicDisplay = magicDisplay }

        ChoicePerk ( perk, selected ) ->
            { model | perks = toggle selected perk model.perks }

        DisplayPerks perksDisplay ->
            { model | perksDisplay = perksDisplay }

        ChoiceFaction faction ->
            { model | faction = faction }

        DisplayFaction factionDisplay ->
            { model | factionDisplay = factionDisplay }

        DisplayFactionalMagic factionalMagicDisplay ->
            { model | factionalMagicDisplay = factionalMagicDisplay }

        ChoiceCompanion ( companion, selected ) ->
            { model | companions = toggle selected companion model.companions }

        DisplayCompanions companionsDisplay ->
            { model | companionsDisplay = companionsDisplay }

        ChoiceRelic ( relic, selected ) ->
            { model | relics = toggle selected relic model.relics }

        DisplayRelics relicsDisplay ->
            { model | relicsDisplay = relicsDisplay }

        ChoiceCosmicPearl cosmicPearl ->
            { model | cosmicPearl = cosmicPearl }

        ChoiceCapBuild capBuild ->
            { model | capBuild = capBuild }

        TowardsCap towardsCap ->
            { model | towardsCap = towardsCap }

        PowerToRewards powerToRewards ->
            { model | powerToRewards = powerToRewards }


toUrl : Model key -> String
toUrl model =
    let
        one : String -> (a -> String) -> Maybe a -> List QueryParameter
        one key f value =
            case value of
                Just v ->
                    [ Url.Builder.string key (f v) ]

                Nothing ->
                    []

        list : String -> (a -> String) -> List a -> List QueryParameter
        list key f values =
            List.map
                (\value -> Url.Builder.string key (f value))
                values

        withDefault : a -> a -> Maybe a
        withDefault default value =
            if value == default then
                Nothing

            else
                Just value

        int : String -> Int -> List QueryParameter
        int key value =
            one key String.fromInt (withDefault 0 value)
    in
    [ one "capBuild" boolToString (withDefault False model.capBuild)
    , int "towardsCap" model.towardsCap
    , int "powerToRewards" model.powerToRewards
    , one "class" Types.classToString model.class
    , list "race" Types.raceToString model.races
    , one "mainRace" Types.raceToString model.mainRace
    , one "gameMode" Types.gameModeToString model.gameMode
    , list "typePerk" Types.raceToString model.typePerks
    , list "complication"
        (\{ name, kind } ->
            Types.complicationToString name ++ Types.complicationKindToString kind
        )
        model.complications
    , list "magic"
        (\{ name, rank } ->
            Types.magicToString name ++ String.fromInt rank
        )
        model.magic
    , list "perk"
        (\{ name, cost } ->
            Types.perkToString name ++ String.fromInt cost
        )
        model.perks
    , one "faction" (\( name, _ ) -> Types.factionToString name) model.faction
    , one "factionPerk" (\( _, perk ) -> boolToString perk) model.faction
    , list "companion" Types.companionToString model.companions
    , list "relic"
        (\{ name, cost } ->
            Types.relicToString name ++ String.fromInt cost
        )
        model.relics
    , list "addAffinity" Types.affinityToString model.cosmicPearl.add
    , list "changeAffinity"
        (\( from, to ) ->
            Types.affinityToString from
                ++ "-"
                ++ Types.affinityToString to
        )
        model.cosmicPearl.change
    ]
        |> List.concat
        |> Url.Builder.toQuery
        |> (\s ->
                if String.isEmpty s then
                    "/"

                else
                    s
           )


boolToString : Bool -> String
boolToString bool =
    if bool then
        "True"

    else
        "False"


parseUrl : Nav.Key -> Url.Url -> Model Nav.Key
parseUrl navKey url =
    let
        appUrl : AppUrl
        appUrl =
            AppUrl.fromUrl url

        parseOne : String -> (String -> Maybe a) -> Maybe a
        parseOne key parser =
            case parseMany key parser of
                [ one ] ->
                    Just one

                _ ->
                    Nothing

        parseMany : String -> (String -> Maybe a) -> List a
        parseMany key parser =
            Dict.get key appUrl.queryParameters
                |> Maybe.andThen (Maybe.Extra.traverse parser)
                |> Maybe.withDefault []

        pair : (String -> Maybe a) -> (a -> Maybe Int -> Maybe b) -> String -> Maybe b
        pair parser builder value =
            let
                ( after, before ) =
                    value
                        |> String.toList
                        |> List.reverse
                        |> List.Extra.break (\c -> not (Char.isDigit c || c == '-'))
                        |> Tuple.mapBoth List.reverse List.reverse
            in
            (parser <| String.fromList before)
                |> Maybe.andThen
                    (\parsed ->
                        let
                            number : Maybe Int
                            number =
                                String.toInt (String.fromList after)
                        in
                        builder parsed number
                    )

        parseComplication : String -> Maybe Types.RankedComplication
        parseComplication =
            pair Types.complicationFromString <|
                \name maybeTier ->
                    { name = name
                    , kind =
                        maybeTier
                            |> Maybe.map Types.Tiered
                            |> Maybe.withDefault
                                Types.Nontiered
                    }
                        |> Just

        parseMagic : String -> Maybe Types.RankedMagic
        parseMagic =
            pair Types.magicFromString <|
                \magic maybeRank ->
                    maybeRank
                        |> Maybe.map
                            (\rank ->
                                { name = magic
                                , rank = rank
                                }
                            )

        parsePerk : String -> Maybe Types.RankedPerk
        parsePerk =
            withCost
                (\raw ->
                    if String.startsWith "Charge Swap" raw && not (String.contains "-" raw) then
                        Types.perkFromString (raw ++ "-Neutral")

                    else
                        Types.perkFromString raw
                )

        parseRelic : String -> Maybe Types.RankedRelic
        parseRelic =
            withCost Types.relicFromString

        withCost : (String -> Maybe a) -> String -> Maybe { name : a, cost : Int }
        withCost fromString =
            pair fromString <|
                \name maybeCost ->
                    maybeCost
                        |> Maybe.map
                            (\cost ->
                                { name = name
                                , cost = cost
                                }
                            )

        parseBool : String -> Bool
        parseBool key =
            parseOne key stringToBool
                |> Maybe.withDefault False

        parseInt : String -> Int
        parseInt key =
            parseOne key String.toInt
                |> Maybe.withDefault 0
    in
    { key = navKey
    , menuOpen = False
    , towardsCap = parseInt "towardsCap"
    , capBuild = parseBool "capBuild"
    , powerToRewards = parseInt "powerToRewards"
    , class = parseOne "class" Types.classFromString
    , classDisplay = DisplayFull
    , races = parseMany "race" Types.raceFromString
    , mainRace = parseOne "mainRace" Types.raceFromString
    , raceDisplay = DisplayFull
    , gameMode = parseOne "gameMode" Types.gameModeFromString
    , gameModeDisplay = DisplayFull
    , complications = parseMany "complication" parseComplication
    , complicationsDisplay = DisplayFull
    , typePerks = parseMany "typePerk" Types.raceFromString
    , typePerksDisplay = DisplayFull
    , magic = parseMany "magic" parseMagic
    , magicDisplay = DisplayFull
    , perks = parseMany "perk" parsePerk
    , perksDisplay = DisplayFull
    , faction =
        parseOne "faction" Types.factionFromString
            |> Maybe.map
                (\faction ->
                    ( faction
                    , parseBool "factionPerk"
                    )
                )
    , factionDisplay = DisplayFull
    , factionalMagicDisplay = DisplayFull
    , companions = parseMany "companion" Types.companionFromString
    , companionsDisplay = DisplayFull
    , relics = parseMany "relic" parseRelic
    , relicsDisplay = DisplayFull
    , cosmicPearl =
        { add = parseMany "addAffinity" Types.affinityFromString
        , change =
            parseMany "changeAffinity"
                (\s ->
                    case String.split "-" s of
                        [ from, to ] ->
                            Maybe.map2 Tuple.pair
                                (Types.affinityFromString from)
                                (Types.affinityFromString to)

                        _ ->
                            Nothing
                )
        }
    }


stringToBool : String -> Maybe Bool
stringToBool bool =
    case bool of
        "True" ->
            Just True

        "False" ->
            Just False

        _ ->
            Nothing


view : Model key -> Browser.Document Msg
view model =
    { title = ""
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
            [ Font.size 16, Element.inFront (Menu.viewMenu model) ]
            (innerView model)
        ]
    }


innerView : Model key -> Element Msg
innerView model =
    let
        allCompact : Bool
        allCompact =
            model.classDisplay /= DisplayFull && model.raceDisplay /= DisplayFull && model.gameModeDisplay /= DisplayFull && model.complicationsDisplay /= DisplayFull && model.typePerksDisplay /= DisplayFull && model.magicDisplay /= DisplayFull && model.perksDisplay /= DisplayFull && model.factionDisplay /= DisplayFull && model.factionalMagicDisplay /= DisplayFull && model.companionsDisplay /= DisplayFull && model.relicsDisplay /= DisplayFull
    in
    Theme.column
        [ width fill
        , Font.color <| rgb 1 1 1
        , Background.color <| rgb 0 0 0
        , Element.padding 16
        ]
        [ Intro.viewTitle allCompact
        , if allCompact then
            Element.none

          else
            Intro.viewIntro
        , Element.Lazy.lazy2 Class.viewClass model.classDisplay model.class
        , Element.Lazy.lazy2 Race.viewRace model.raceDisplay model.races
        , Element.Lazy.lazy2 GameMode.viewGameMode model.gameModeDisplay model.gameMode
        , Element.Lazy.lazy2 Complications.viewComplications model.complicationsDisplay model.complications
        , Element.Lazy.lazy3 TypePerk.viewTypePerks model.races model.typePerksDisplay model.typePerks
        , Element.Lazy.lazy2 Magic.viewMagics model.magicDisplay model.magic
        , Element.Lazy.lazy4 Perk.viewPerks model.perksDisplay model.mainRace model.races model.perks
        , Element.Lazy.lazy2 Faction.viewFaction model.factionDisplay model.faction
        , Element.Lazy.lazy2 FactionalMagic.viewFactionalMagics model.factionalMagicDisplay model.magic
        , Element.Lazy.lazy2 Companion.viewCompanions model.companionsDisplay model.companions
        , Element.Lazy.lazy5 Relic.viewRelics model.relicsDisplay model.cosmicPearl model.mainRace model.races model.relics
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
