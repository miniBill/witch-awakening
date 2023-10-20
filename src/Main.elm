module Main exposing (Flags, main)

import AppUrl exposing (AppUrl)
import Browser exposing (UrlRequest(..))
import Browser.Dom
import Browser.Events
import Browser.Navigation as Nav
import Dict
import Element exposing (Element, fill, rgb, width)
import Element.Background as Background
import Element.Font as Font
import Element.Lazy
import Generated.Types as Types
import Json.Decode as JD
import List.Extra
import Maybe.Extra
import Task
import Theme
import Types exposing (Choice(..), Model, Msg(..))
import Url
import Url.Builder exposing (QueryParameter)
import View.Class as Class
import View.Complication as Complications
import View.Faction as Faction
import View.FactionalMagic as FactionalMagic
import View.GameMode as GameMode
import View.Intro as Intro
import View.Magic as Magic
import View.Menu as Menu
import View.Perk as Perk
import View.Race as Race
import View.TypePerk as TypePerk


type alias Flags =
    {}


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = \_ -> UrlChanged
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( parseUrl key url
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
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
                newModel : Model
                newModel =
                    updateOnChoice choice model
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
            , Task.andThen
                (\{ element } ->
                    Browser.Dom.setViewport
                        0
                        (element.y - Theme.rythm)
                )
                (Browser.Dom.getElement id)
                |> Task.attempt (\_ -> Nop)
            )

        Nop ->
            ( model, Cmd.none )


updateOnChoice : Choice -> Model -> Model
updateOnChoice choice model =
    case choice of
        ChoiceClass class ->
            { model | class = class }

        ChoiceRace race ->
            { model | race = race }

        ChoiceGameMode gameMode ->
            { model | gameMode = gameMode }

        ChoiceComplication complication selected ->
            if selected then
                { model | complications = complication :: model.complications }

            else
                { model | complications = List.Extra.remove complication model.complications }

        ChoiceTypePerk race selected ->
            if selected then
                { model | typePerks = race :: model.typePerks }

            else
                { model | typePerks = List.Extra.remove race model.typePerks }

        ChoiceMagic magic selected ->
            if selected then
                { model | magic = magic :: model.magic }

            else
                { model | magic = List.Extra.remove magic model.magic }

        ChoicePerk perk selected ->
            if selected then
                { model | perks = perk :: model.perks }

            else
                { model | perks = List.Extra.remove perk model.perks }

        ChoiceFaction faction ->
            { model | faction = faction }

        TowardsCap towardsCap ->
            { model | towardsCap = towardsCap }


toUrl : Model -> String
toUrl model =
    let
        pair : String -> (a -> String) -> Maybe a -> List QueryParameter
        pair key f value =
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
    in
    [ pair "towardsCap" String.fromInt (Just model.towardsCap)
    , pair "class" Types.classToString model.class
    , pair "race" Types.raceToString model.race
    , pair "gameMode" Types.gameModeToString model.gameMode
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
    , pair "faction" (\( name, _ ) -> Types.factionToString name) model.faction
    , pair "factionPerk" (\( _, perk ) -> boolToString perk) model.faction
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


parseUrl : Nav.Key -> Url.Url -> Model
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
                ( before, after ) =
                    value
                        |> String.toList
                        |> List.Extra.break Char.isDigit
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
            pair Types.perkFromString <|
                \magic maybeCost ->
                    maybeCost
                        |> Maybe.map
                            (\cost ->
                                { name = magic
                                , cost = cost
                                }
                            )

        parseBool : String -> Maybe Bool
        parseBool bool =
            case bool of
                "True" ->
                    Just True

                "False" ->
                    Just False

                _ ->
                    Nothing
    in
    { key = navKey
    , menuOpen = False
    , towardsCap =
        parseOne "towardsCap" String.toInt
            |> Maybe.withDefault 0
    , class = parseOne "class" Types.classFromString
    , race = parseOne "race" Types.raceFromString
    , gameMode = parseOne "gameMode" Types.gameModeFromString
    , complications = parseMany "complication" parseComplication
    , typePerks = parseMany "typePerk" Types.raceFromString
    , magic = parseMany "magic" parseMagic
    , perks = parseMany "perk" parsePerk
    , faction =
        parseOne "faction" Types.factionFromString
            |> Maybe.map
                (\faction ->
                    ( faction
                    , parseOne "factionPerk" parseBool
                        |> Maybe.withDefault False
                    )
                )
    }


view : Model -> Browser.Document Msg
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


innerView : Model -> Element Msg
innerView model =
    Theme.column
        [ width fill
        , Font.color <| rgb 1 1 1
        , Background.color <| rgb 0 0 0
        , Element.padding 16
        ]
        [ Intro.viewTitle
        , Intro.viewIntro
        , Element.Lazy.lazy Class.viewClass model.class
        , Element.Lazy.lazy Race.viewRace model.race
        , Element.Lazy.lazy GameMode.viewGameMode model.gameMode
        , Element.Lazy.lazy Complications.viewComplications model.complications
        , Element.Lazy.lazy TypePerk.viewTypePerks model.typePerks
        , Element.Lazy.lazy Magic.viewMagics model.magic
        , Element.Lazy.lazy Perk.viewPerks model.perks
        , Element.Lazy.lazy Faction.viewFaction model.faction
        , Element.Lazy.lazy FactionalMagic.viewFactionalMagics model.magic
        ]
        |> Element.map Choice


subscriptions : Model -> Sub Msg
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
