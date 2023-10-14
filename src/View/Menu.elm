module View.Menu exposing (viewMenu)

import Data.Complication as Complication exposing (Content(..))
import Data.Magic as Magic exposing (Affinities(..))
import Data.Race as Race
import Data.TypePerk as TypePerk
import Element exposing (Element, alignBottom, alignRight, alignTop, centerX, centerY, el, fill, height, padding, paragraph, px, rgb, scrollbarY, shrink, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Generated.Types exposing (Affinity(..), Class, GameMode(..), Race)
import Gradients
import List.Extra
import Maybe.Extra
import String.Extra
import Theme
import Types exposing (Choice(..), ComplicationKind(..), Model, Msg(..), RankedMagic)


viewMenu : Model -> Element Msg
viewMenu model =
    let
        power : Maybe Int
        power =
            calculatePower model

        menuLabel : String
        menuLabel =
            case
                ( Maybe.map String.fromInt power
                , Maybe.map String.fromInt (powerCap model)
                )
            of
                ( Just p, Just c ) ->
                    "[" ++ p ++ "] [/] [" ++ c ++ "]"

                _ ->
                    "(epic)"
    in
    Theme.column
        [ alignTop
        , alignRight
        , height fill
        , padding 16
        ]
        [ Input.button
            [ Border.width 1
            , Background.color <| rgb 0.7 0.7 1
            , Border.rounded 999
            , Theme.padding
            , alignRight
            ]
            { onPress = Just ToggleMenu
            , label =
                Theme.blocks
                    [ centerX
                    , centerY
                    ]
                    menuLabel
            }
        , if model.menuOpen then
            viewCalculations model

          else
            Element.none
        ]


viewCalculations : Model -> Element Msg
viewCalculations model =
    let
        row : String -> (Model -> Maybe Int) -> Element Msg
        row label toValue =
            let
                target : String
                target =
                    case label of
                        "Initial power" ->
                            "Game Mode"

                        "Power cap" ->
                            "Game Mode"

                        "Magic" ->
                            "The Magic"

                        _ ->
                            label
            in
            case toValue model of
                Nothing ->
                    Theme.row [ width fill ]
                        [ Input.button []
                            { onPress = Just <| ScrollTo <| String.Extra.underscored target
                            , label = el [ Font.bold ] <| text label
                            }
                        , rightNumber "?"
                        ]

                Just value ->
                    Theme.row [ width fill ]
                        [ Input.button []
                            { onPress = Just <| ScrollTo <| String.Extra.underscored target
                            , label = el [ Font.bold ] <| text label
                            }
                        , rightNumber <| String.fromInt value
                        ]

        rightNumber : String -> Element msg
        rightNumber value =
            el
                [ alignRight
                , Theme.captureIt
                , Font.size 20
                ]
                (Theme.gradientText 4 Gradients.yellowGradient value)
    in
    Theme.column
        [ Background.color <| rgb 1 1 1
        , Theme.padding
        , scrollbarY
        , height fill
        , Border.rounded Theme.rythm
        , width <| Element.maximum 200 shrink
        ]
        [ paragraph
            [ Font.bold
            , Font.center
            , Font.size 24
            ]
            [ text "🐱culations" ]
        , row "Initial power" initialPower
        , row "Power cap" powerCap
        , row "Complications" complicationsValue
        , row "Type perks" typePerksValue
        , Input.slider
            [ width fill
            , Element.behindContent <|
                el
                    [ width fill
                    , height (px 2)
                    , centerY
                    , Background.color <| rgb 0.7 0.7 0.7
                    , Border.rounded 2
                    ]
                    Element.none
            ]
            { onChange = Choice << TowardsCap << round
            , label =
                Input.labelAbove [] <|
                    Theme.row []
                        [ paragraph [ Font.bold ]
                            [ text "Assign power to cap instead of initial"
                            , rightNumber <| String.fromInt model.towardsCap
                            ]
                        ]
            , min = 0
            , max = toFloat <| Maybe.withDefault 0 <| complicationsValue model
            , value = toFloat model.towardsCap
            , step = Just 1
            , thumb = Input.defaultThumb
            }
        , row "Magic" magicsValue
        , el [ alignBottom, width fill ] <| row "Result" calculatePower
        ]


calculatePower : Model -> Maybe Int
calculatePower model =
    [ initialPower
    , typePerksValue
    , magicsValue
    ]
        |> Maybe.Extra.traverse (\f -> f model)
        |> Maybe.map List.sum


initialPower : Model -> Maybe Int
initialPower model =
    case ( model.gameMode, complicationsValue model ) of
        ( Nothing, Just complications ) ->
            Just <| 30 + complications - model.towardsCap

        ( Just StoryArc, Just complications ) ->
            Just <| 10 + complications - model.towardsCap

        ( Just EarlyBird, Just complications ) ->
            Just <| 75 + complications - model.towardsCap

        _ ->
            Nothing


powerCap : Model -> Maybe Int
powerCap model =
    case model.gameMode of
        Nothing ->
            Just <| 100 + model.towardsCap

        Just StoryArc ->
            Just <| 150 + model.towardsCap

        Just EarlyBird ->
            Just <| 75 + model.towardsCap

        Just SkillTree ->
            Nothing

        Just Constellation ->
            Nothing


complicationsValue : Model -> Maybe Int
complicationsValue =
    maybeSum complicationValue .complications


complicationValue : Types.Complication -> Maybe Int
complicationValue complication =
    let
        get : Int -> List ( a, Int ) -> Maybe Int
        get tier list =
            List.Extra.getAt (tier - 1) list
                |> Maybe.map Tuple.second
    in
    combinedComplications
        |> List.Extra.find (\{ name } -> name == complication.name)
        |> Maybe.andThen
            (\details ->
                case ( details.content, complication.kind ) of
                    ( Single value _, _ ) ->
                        Just value

                    ( WithTiers _ tiers _, Tiered tier ) ->
                        get tier tiers

                    ( WithChoices _ choices _, Tiered tier ) ->
                        get tier choices

                    ( _, Nontiered ) ->
                        Nothing
            )


combinedComplications : List Complication.Details
combinedComplications =
    Complication.worldShifts ++ Complication.all


typePerksValue : Model -> Maybe Int
typePerksValue =
    maybeSum typePerkValue .typePerks


typePerkValue : Race -> Maybe Int
typePerkValue race =
    List.Extra.find (\perk -> perk.race == race) TypePerk.all
        |> Maybe.map (\{ cost } -> -cost)


magicsValue : Model -> Maybe Int
magicsValue model =
    let
        affinities : List Affinity
        affinities =
            case model.race of
                Nothing ->
                    [ All ]

                Just race ->
                    Race.all
                        |> List.Extra.find (\{ name } -> name == race)
                        |> Maybe.map .affinities
                        |> Maybe.withDefault []
                        |> (::) All
    in
    maybeSum (magicValue affinities model.class) .magic model


magicValue : List Affinity -> Maybe Class -> RankedMagic -> Maybe Int
magicValue affinities class { name, rank } =
    combinedMagics
        |> List.Extra.find (\magic -> magic.name == name)
        |> Maybe.andThen
            (\magic ->
                let
                    isClass : Bool
                    isClass =
                        Just magic.class == class

                    isAffinity : Bool
                    isAffinity =
                        case magic.affinities of
                            Regular regular ->
                                List.any
                                    (\affinity -> List.member affinity affinities)
                                    regular

                            Alternative alternatives ->
                                alternatives
                                    |> List.any
                                        (\alternative ->
                                            List.all
                                                (\affinity -> List.member affinity affinities)
                                                alternative
                                        )

                    cases : Int -> Int -> Int -> Int -> Int
                    cases basicCost affinityCost classCost bothCost =
                        if isClass then
                            if isAffinity then
                                bothCost

                            else
                                classCost

                        else if isAffinity then
                            affinityCost

                        else
                            basicCost
                in
                case rank of
                    1 ->
                        Just <| cases -1 -1 1 1

                    2 ->
                        Just <| cases -3 -2 -1 0

                    3 ->
                        Just <| cases -6 -4 -4 -2

                    4 ->
                        Just <| cases -10 -6 -8 -4

                    5 ->
                        Just <| cases -15 -9 -13 -7

                    _ ->
                        Nothing
            )


combinedMagics : List Magic.Details
combinedMagics =
    Magic.all ++ Magic.elementalism


maybeSum : (item -> Maybe Int) -> (Model -> List item) -> Model -> Maybe Int
maybeSum toValue selector model =
    model
        |> selector
        |> Maybe.Extra.traverse toValue
        |> Maybe.map List.sum
