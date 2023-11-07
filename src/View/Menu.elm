module View.Menu exposing (viewMenu)

import Data.Costs as Costs exposing (Points)
import Element exposing (Element, alignBottom, alignRight, alignTop, centerX, centerY, el, fill, height, padding, paragraph, px, rgb, scrollbarY, shrink, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Generated.Types as Types
import List.Extra
import Results exposing (Results(..))
import String.Extra
import Theme
import Types exposing (Choice(..), Model, Msg(..))


viewMenu : Model -> Element Msg
viewMenu model =
    let
        power : Results Points
        power =
            calculatePower model

        menuLabel : String
        menuLabel =
            case
                Results.map2 Tuple.pair
                    power
                    (Costs.initialPower model)
            of
                Oks ( p, c ) ->
                    let
                        warningsIcon : String
                        warningsIcon =
                            if List.isEmpty warnings then
                                ""

                            else
                                "[W] "

                        powerString : String
                        powerString =
                            warningsIcon ++ "[" ++ String.fromInt p.power ++ "] [/] [" ++ String.fromInt c.power ++ "]"
                    in
                    if p.rewardPoints == 0 && c.rewardPoints == 0 then
                        powerString

                    else
                        "p: " ++ powerString ++ "\n\nrp: {" ++ String.fromInt p.rewardPoints ++ "} {/} {" ++ String.fromInt c.rewardPoints ++ "}"

                Errs _ ->
                    "[E]"

        ( warnings, errors ) =
            case power of
                Oks points ->
                    ( List.Extra.unique points.warnings, [] )

                Errs es ->
                    ( [], List.Extra.unique es )
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
            , if List.isEmpty errors then
                if List.isEmpty warnings then
                    Background.color <| rgb 0.7 0.7 1

                else
                    Background.color <| rgb 1 0.9 0.8

              else
                Background.color <| rgb 1 0.8 0.8
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
            viewCalculations warnings model

          else
            Element.none
        ]


viewCalculations : List String -> Model -> Element Msg
viewCalculations warnings model =
    let
        row : String -> (Model -> Results Points) -> Maybe String -> Element Msg
        row label toValue target =
            case toValue model of
                Errs es ->
                    let
                        errorViews : List (Element msg)
                        errorViews =
                            List.map
                                (\e ->
                                    paragraph
                                        [ Background.color <| rgb 1 0.8 0.8
                                        , Theme.padding
                                        , Theme.rounded
                                        ]
                                        [ text e ]
                                )
                                es
                    in
                    Theme.column [ width fill ] <|
                        linkLabel label target
                            :: errorViews

                Oks value ->
                    Theme.row [ width fill ]
                        [ linkLabel label target
                        , rightPoints value
                        ]

        resultRow : Element Msg
        resultRow =
            row "Result"
                (calculatePower
                    >> Results.mapErrors (\_ -> [ "There are errors in the computation" ])
                )
                Nothing

        link : String -> Maybe String -> Element Msg
        link label target =
            Theme.row [ width fill ]
                [ linkLabel label target
                , rightText "-"
                ]

        button : { onPress : Maybe msg, label : Element msg } -> Element msg
        button =
            Input.button
                [ Border.width 1
                , padding 4
                , Theme.rounded
                , width fill
                , Font.center
                ]
    in
    Theme.column
        [ Background.color <| rgb 1 1 1
        , Theme.padding
        , scrollbarY
        , height fill
        , Theme.rounded
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
        , row "Complications" Costs.complicationsValue Nothing
        , capSlider model
        , row "Type perks" Costs.typePerksValue Nothing
        , row "Magic" Costs.magicsValue <| Just "The Magic"
        , row "Perks" Costs.perksValue Nothing
        , row "Faction" Costs.factionValue <| Just "Factions"
        , row "Companions" Costs.companionsValue Nothing
        , row "Relics" Costs.relicsValue Nothing
        , el [ width fill, height <| px 1, Background.color <| rgb 0 0 0 ] Element.none
        , resultRow
        , if List.isEmpty warnings then
            Element.none

          else
            el [ alignBottom, Font.bold ] <| text "Warnings"
        , if List.isEmpty warnings then
            Element.none

          else
            Theme.column []
                (List.map
                    (\warning ->
                        paragraph
                            [ Background.color <| rgb 0.9 0.9 0.5
                            , Theme.padding
                            , Theme.rounded
                            ]
                            [ text warning ]
                    )
                    warnings
                )
        , el [ alignBottom, Font.bold ] <| text "Affinities"
        , Theme.wrappedRow [] <|
            List.map Theme.viewAffinity <|
                List.Extra.remove Types.All <|
                    Types.affinities model
        , row "Power cap" Costs.powerCap <| Just "Game Mode"
        , button
            { onPress = Just CompactAll
            , label = text "Compact all"
            }
        , button
            { onPress = Just ExpandAll
            , label = text "Expand all"
            }
        ]


rightPoints : Points -> Element msg
rightPoints value =
    let
        wrap : String -> Int -> String -> String
        wrap before v after =
            if v == 0 then
                ""

            else
                before ++ String.fromInt v ++ after

        joined : String
        joined =
            String.join " "
                [ wrap "[" value.power "]"
                , wrap "{" value.rewardPoints "}"
                ]

        fullString : String
        fullString =
            if String.isEmpty joined then
                "{-}"

            else
                joined
    in
    rightText fullString


rightText : String -> Element msg
rightText value =
    el
        [ alignRight
        , Theme.captureIt
        , Font.size 20
        ]
        (Theme.blocks [] value)



--(Theme.gradientText 4 Gradients.yellowGradient value)


capSlider : Model -> Element Msg
capSlider model =
    if model.gameMode /= Nothing then
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
            { onChange = \newValue -> Choice <| TowardsCap <| round newValue
            , label =
                Input.labelAbove [] <|
                    Theme.row []
                        [ paragraph [ Font.bold ]
                            [ text "Assign power to cap instead of initial"
                            , rightPoints <| Costs.powerToPoints model.towardsCap
                            ]
                        ]
            , min = 0
            , max = toFloat <| Results.withDefault 0 <| Costs.complicationsRawValue model
            , value = toFloat model.towardsCap
            , step = Just 1
            , thumb = Input.defaultThumb
            }


linkLabel : String -> Maybe String -> Element Msg
linkLabel label target =
    Input.button []
        { onPress = Just <| ScrollTo <| String.Extra.underscored <| Maybe.withDefault label target
        , label = el [ Font.bold ] <| text label
        }


calculatePower : Model -> Results Points
calculatePower model =
    [ Costs.classPower model
    , Costs.initialPower model
    , Costs.complicationsValue model
    , Costs.typePerksValue model
    , Costs.magicsValue model
    , Costs.perksValue model
    , Costs.factionValue model
    , Costs.companionsValue model
    , Costs.relicsValue model
    , -- This is used to collect warnings and errors from the power cap
      Costs.powerCap model
        |> Results.map (\points -> { points | power = 0, rewardPoints = 0 })
    ]
        |> Results.combine
        |> Results.map (List.foldl Costs.sum Costs.zero)
