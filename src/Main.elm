module Main exposing (Flags, Msg, main)

import AppUrl exposing (AppUrl)
import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Dict
import Element exposing (Element, fill, height, rgb, scrollbarY, width)
import Element.Background as Background
import Element.Font as Font
import Element.Lazy
import Generated.Types as Types
import List.Extra
import Maybe.Extra
import Theme
import Types exposing (Choice(..), Model)
import Url
import Url.Builder exposing (QueryParameter)
import View.Class as Class
import View.Complication as Complications
import View.GameMode as GameMode
import View.Intro as Intro
import View.Magic as Magic
import View.Race as Race
import View.TypePerk as TypePerk


type Msg
    = UrlClicked UrlRequest
    | UrlChanged --Url.Url
    | Choice Choice


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
    [ pair "class" Types.classToString model.class
    , pair "race" Types.raceToString model.race
    , pair "gameMode" Types.gameModeToString model.gameMode
    , list "typePerk" Types.raceToString model.typePerks
    , list "complication"
        (\{ name, kind } ->
            Types.complicationNameToString name ++ Types.complicationKindToString kind
        )
        model.complications
    , list "magic"
        (\{ name, rank } ->
            Types.magicToString name ++ String.fromInt rank
        )
        model.magic
    ]
        |> List.concat
        |> Url.Builder.toQuery
        |> (\s ->
                if String.isEmpty s then
                    "/"

                else
                    s
           )


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
            Maybe.andThen
                (\parsed ->
                    let
                        number : Maybe Int
                        number =
                            String.toInt (String.fromList after)
                    in
                    builder parsed number
                )
                (parser <| String.fromList before)

        parseComplication : String -> Maybe Types.Complication
        parseComplication =
            pair Types.complicationNameFromString <|
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
    in
    { key = navKey
    , class = parseOne "class" Types.classFromString
    , race = parseOne "race" Types.raceFromString
    , gameMode = parseOne "gameMode" Types.gameModeFromString
    , complications = parseMany "complication" parseComplication
    , typePerks = parseMany "typePerk" Types.raceFromString
    , magic = parseMany "magic" parseMagic
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
            [ Font.size 16 ]
            (innerView model)
        ]
    }


innerView : Model -> Element Msg
innerView model =
    Theme.column
        [ width fill
        , height fill
        , scrollbarY
        , Font.color <| rgb 1 1 1
        , Background.color <| rgb 0 0 0
        , Theme.padding
        ]
        [ Intro.viewTitle
        , Intro.viewIntro
        , Element.Lazy.lazy Class.viewClass model.class
        , Element.Lazy.lazy Race.viewRace model.race
        , Element.Lazy.lazy GameMode.viewGameMode model.gameMode
        , Element.Lazy.lazy Complications.viewComplications model.complications
        , Element.Lazy.lazy TypePerk.viewTypePerks model.typePerks
        , Element.Lazy.lazy Magic.viewMagics model.magic
        ]
        |> Element.map Choice


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
