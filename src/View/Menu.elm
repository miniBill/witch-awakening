module View.Menu exposing (viewMenu)

import Data.Costs as Costs
import Element exposing (Element, alignBottom, alignRight, alignTop, centerX, centerY, el, fill, height, padding, paragraph, px, rgb, scrollbarY, shrink, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Generated.Types as Types exposing (GameMode(..))
import Gradients
import List.Extra
import Maybe.Extra
import String.Extra
import Theme
import Types exposing (Choice(..), Model, Msg(..))


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
                , Maybe.map String.fromInt (Costs.powerCap model)
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
        , height <|
            if model.menuOpen then
                fill

            else
                shrink
        , padding 16
        ]
        [ Input.button
            [ Border.width 1
            , Background.color <| rgb 0.7 0.7 1
            , Border.rounded 999
            , Theme.padding
            , alignRight
            ]
            { onPress =
                if model.menuOpen then
                    Just CloseMenu

                else
                    Just OpenMenu
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
        row : String -> (Model -> Maybe Int) -> Maybe String -> Element Msg
        row label toValue target =
            case toValue model of
                Nothing ->
                    Theme.row [ width fill ]
                        [ linkLabel label target
                        , rightNumber "?"
                        ]

                Just value ->
                    Theme.row [ width fill ]
                        [ linkLabel label target
                        , rightNumber <| String.fromInt value
                        ]

        link : String -> Maybe String -> Element Msg
        link label target =
            Theme.row [ width fill ]
                [ linkLabel label target
                , rightNumber "-"
                ]

        rightNumber : String -> Element msg
        rightNumber value =
            el
                [ alignRight
                , Theme.captureIt
                , Font.size 20
                ]
                (Theme.gradientText 4 Gradients.yellowGradient value)

        button : { onPress : Maybe msg, label : Element msg } -> Element msg
        button =
            Input.button
                [ Border.width 1
                , padding 4
                , Border.rounded Theme.rythm
                , width fill
                , Font.center
                ]
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
        , row "Class" Costs.classPower <| Just "True Form - Class"
        , link "Race" <| Just "True Form - Race"
        , row "Initial power" Costs.initialPower <| Just "Game Mode"
        , row "Power cap" Costs.powerCap <| Just "Game Mode"
        , row "Complications" Costs.complicationsValue Nothing
        , row "Type perks" Costs.typePerksValue Nothing
        , if model.gameMode == Just StoryArc then
            Element.none

          else
            Input.slider
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
                , max = toFloat <| Maybe.withDefault 0 <| Costs.complicationsValue model
                , value = toFloat model.towardsCap
                , step = Just 1
                , thumb = Input.defaultThumb
                }
        , row "Magic" Costs.magicsValue <| Just "The Magic"
        , row "Perks" Costs.perksValue Nothing
        , row "Faction" Costs.factionValue <| Just "Factions"
        , row "Companions" Costs.companionsValue Nothing
        , row "Relics" Costs.relicsValue Nothing
        , el [ Font.bold ] <| text "Affinities"
        , Theme.wrappedRow [] <|
            List.map Theme.viewAffinity <|
                List.Extra.remove Types.All <|
                    Types.affinities model
        , el [ alignBottom, width fill ] <| row "Result" calculatePower Nothing
        , button
            { onPress = Just CompactAll
            , label = text "Compact all"
            }
        , button
            { onPress = Just ExpandAll
            , label = text "Expand all"
            }
        ]


linkLabel : String -> Maybe String -> Element Msg
linkLabel label target =
    Input.button []
        { onPress = Just <| ScrollTo <| String.Extra.underscored <| Maybe.withDefault label target
        , label = el [ Font.bold ] <| text label
        }


calculatePower : Model -> Maybe Int
calculatePower model =
    [ Costs.classPower
    , Costs.initialPower
    , Costs.typePerksValue
    , Costs.magicsValue
    , Costs.perksValue
    , Costs.factionValue
    , Costs.companionsValue
    , Costs.relicsValue
    ]
        |> Maybe.Extra.traverse (\f -> f model)
        |> Maybe.map List.sum
