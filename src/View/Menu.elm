module View.Menu exposing (viewMenu)

import Data.Costs as Costs exposing (CostsResult(..), Points)
import Element exposing (Element, alignBottom, alignRight, alignTop, centerX, centerY, el, fill, height, padding, paragraph, px, rgb, scrollbarY, shrink, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Generated.Types as Types
import Gradients
import List.Extra
import String.Extra
import Theme
import Types exposing (Choice(..), Model, Msg(..))


viewMenu : Model -> Element Msg
viewMenu model =
    let
        power : CostsResult Points
        power =
            calculatePower model

        menuLabel : String
        menuLabel =
            case
                ( power
                , Costs.initialPower model
                )
            of
                ( CostsOk p, CostsOk c ) ->
                    let
                        warningsIcon : String
                        warningsIcon =
                            if List.isEmpty warnings then
                                ""

                            else
                                "[W] "
                    in
                    warningsIcon ++ "[" ++ String.fromInt p.power ++ "] [/] [" ++ String.fromInt c.power ++ "]"

                _ ->
                    "(epic)"

        ( warnings, errors ) =
            case Costs.map2 Costs.sum power (Costs.powerCap model) of
                CostsOk points ->
                    ( List.Extra.unique points.warnings, [] )

                CostsErr es ->
                    ( [], es )
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
        row : String -> (Model -> CostsResult Points) -> Maybe String -> Element Msg
        row label toValue target =
            case toValue model of
                CostsErr es ->
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

                CostsOk value ->
                    Theme.row [ width fill ]
                        [ linkLabel label target
                        , rightNumber <| String.fromInt value.power
                        ]

        resultRow : Element Msg
        resultRow =
            row "Result"
                (calculatePower
                    >> Costs.mapErrors (\_ -> [ "There are errors in the computation" ])
                )
                Nothing

        link : String -> Maybe String -> Element Msg
        link label target =
            Theme.row [ width fill ]
                [ linkLabel label target
                , rightNumber "-"
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
        , row "Type perks" (upcast Costs.typePerksValue) Nothing
        , capSlider model
        , row "Magic" (upcast Costs.magicsValue) <| Just "The Magic"
        , row "Perks" (upcast Costs.perksValue) Nothing
        , row "Faction" (upcast Costs.factionValue) <| Just "Factions"
        , row "Companions" (upcast Costs.companionsValue) Nothing
        , row "Relics" (upcast Costs.relicsValue) Nothing
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


rightNumber : String -> Element msg
rightNumber value =
    el
        [ alignRight
        , Theme.captureIt
        , Font.size 20
        ]
        (Theme.gradientText 4 Gradients.yellowGradient value)


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
                            , rightNumber <| String.fromInt model.towardsCap
                            ]
                        ]
            , min = 0
            , max = toFloat <| Costs.withDefault 0 <| Costs.complicationsRawValue model
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


calculatePower : Model -> CostsResult Points
calculatePower model =
    [ Costs.classPower model
    , Costs.initialPower model
    , Costs.complicationsValue model
    , Costs.powerCap model
        -- This is used to collect warnings
        |> Costs.map (\points -> { points | power = 0, rewardPoints = 0 })
    , upcast Costs.typePerksValue model
    , upcast Costs.magicsValue model
    , upcast Costs.perksValue model
    , upcast Costs.factionValue model
    , upcast Costs.companionsValue model
    , upcast Costs.relicsValue model
    ]
        |> Costs.combine
        |> Costs.map (List.foldl Costs.sum Costs.zero)


upcast : (Model -> Maybe Int) -> Model -> CostsResult Points
upcast f model =
    case f model of
        Just p ->
            CostsOk { power = p, rewardPoints = 0, warnings = [] }

        Nothing ->
            CostsErr
                [ let
                    _ =
                        Debug.todo
                  in
                  "TODO"
                ]
