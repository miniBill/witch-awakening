module View.Menu exposing (viewMenu)

import Data.Costs as Costs exposing (Points)
import Element exposing (Attribute, Element, alignBottom, alignRight, alignTop, centerX, centerY, el, fill, height, padding, paragraph, px, rgb, scrollbarY, shrink, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Keyed
import Generated.Types as Types
import List.Extra
import Results exposing (Results(..))
import String.Extra
import Theme
import Types exposing (Choice(..), Model, Msg(..))


viewMenu : Model -> Element Msg
viewMenu model =
    let
        totalPoints : Results Points
        totalPoints =
            Costs.totalPoints model

        ( warnings, errors ) =
            case totalPoints of
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
                    (menuLabel model totalPoints warnings)
            }
        , if model.menuOpen then
            viewCalculations model totalPoints warnings

          else
            Element.none
        ]


menuLabel : Model -> Results Points -> List String -> String
menuLabel model totalPointsResult warnings =
    let
        availablePoints : Results Points
        availablePoints =
            if model.capBuild || model.gameMode == Just Types.EarlyBird then
                Costs.powerCap model

            else
                Costs.startingValue model
    in
    case
        Results.map3 (\t a r -> ( t, a, r ))
            totalPointsResult
            availablePoints
            (Costs.totalRewards model)
    of
        Oks ( totalPoints, available, rewards ) ->
            let
                warningsIcon : String
                warningsIcon =
                    if List.isEmpty warnings then
                        ""

                    else
                        "[W] "

                pointDisplay : String -> Int -> Int -> String -> String
                pointDisplay before used total after =
                    [ wrapInt before (used + total) after
                    , before ++ "/" ++ after
                    , wrapInt before total after
                    ]
                        |> String.join " "

                powerString : String
                powerString =
                    warningsIcon
                        ++ pointDisplay
                            "["
                            totalPoints.power
                            available.power
                            "]"
            in
            if totalPoints.rewardPoints == 0 && rewards.rewardPoints == 0 then
                powerString

            else
                let
                    rewardString : String
                    rewardString =
                        pointDisplay
                            "{"
                            totalPoints.rewardPoints
                            rewards.rewardPoints
                            "}"
                in
                "{center} " ++ powerString ++ "\n\n{center} " ++ rewardString

        Errs _ ->
            "[E]"


wrapInt : String -> Int -> String -> String
wrapInt before value after =
    before ++ String.fromInt value ++ after


viewCalculations : Model -> Results Points -> List String -> Element Msg
viewCalculations model power warnings =
    let
        resultRow : ( String, Element Msg )
        resultRow =
            keyedRow
                "Result"
                (power
                    |> Results.map Costs.negate
                    |> Results.mapErrors (\_ -> [ "There are errors in the computation" ])
                )
                Nothing

        link : String -> Maybe String -> ( String, Element Msg )
        link label target =
            ( label
            , Theme.row [ width fill ]
                [ linkLabel label target
                , rightText emptyRowContent
                ]
            )

        button : { onPress : msg, label : String } -> ( String, Element msg )
        button config =
            ( config.label
            , Input.button
                [ Border.width 1
                , padding 4
                , Theme.rounded
                , width fill
                , Font.center
                ]
                { onPress = Just config.onPress
                , label = text config.label
                }
            )

        section : List (Attribute msg) -> String -> ( String, Element msg )
        section attrs content =
            ( content, el (Font.bold :: attrs) <| text content )
    in
    Element.Keyed.column
        [ Theme.spacing
        , Background.color <| rgb 1 1 1
        , Theme.padding
        , scrollbarY
        , height fill
        , Theme.rounded
        , width <| Element.maximum 200 shrink
        ]
        [ ( "Header"
          , paragraph
                [ Font.bold
                , Font.center
                , Font.size 24
                ]
                [ text "🐱culations" ]
          )
        , section [] "Build kind"
        , ( "Switch", capBuildSwitch model )
        , keyedRow "Class" (Costs.classValue model) <| Just "True Form - Class"
        , link "Race" <| Just "True Form - Race"
        , keyedRow "Starting power" (Costs.startingValue model) <| Just "Game Mode"
        , keyedRow "Complications" (Costs.complicationsValue model) Nothing
        , keyedRow "Type perks" (Costs.typePerksValue model) Nothing
        , keyedRow "Magic" (Costs.magicsValue model) <| Just "The Magic"
        , keyedRow "Perks" (Costs.perksValue model) Nothing
        , keyedRow "Faction" (Costs.factionValue model) <| Just "Factions"
        , keyedRow "Companions" (Costs.companionsValue model) Nothing
        , ( "RelicSlider", relicSlider model )
        , keyedRow "Relics" (Costs.relicsValue model) Nothing
        , ( "Separator", el [ width fill, height <| px 1, Background.color <| rgb 0 0 0 ] Element.none )
        , resultRow
        , if List.isEmpty warnings then
            ( "Warnings", Element.none )

          else
            section [ alignBottom ] "Warnings"
        , ( "WarningsColumn"
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
          )
        , section [ alignBottom ] "Affinities"
        , ( "AffinitiesColumn"
          , Theme.wrappedRow [] <|
                List.map Theme.viewAffinity <|
                    List.Extra.remove Types.All <|
                        Types.affinities model
          )
        , ( "Cap Slider", capSlider model )
        , keyedRow "Power cap" (Costs.powerCap model) <| Just "Game Mode"
        , button
            { onPress = CompactAll
            , label = "Compact all"
            }
        , button
            { onPress = ExpandAll
            , label = "Expand all"
            }
        ]


keyedRow : String -> Results Points -> Maybe String -> ( String, Element Msg )
keyedRow label result target =
    ( label, row label result target )


row : String -> Results Points -> Maybe String -> Element Msg
row label result target =
    case result of
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
            paragraph [ width fill ]
                [ linkLabel label target
                , rightPoints value
                ]


capBuildSwitch : Model -> Element Msg
capBuildSwitch { capBuild } =
    let
        roundIf : Bool -> number
        roundIf b =
            if b then
                Theme.rythm

            else
                0

        button : Bool -> String -> Element Msg
        button target label =
            Input.button
                [ Theme.padding
                , width fill
                , Font.center
                , Font.bold
                , if target == capBuild then
                    Background.color <| rgb 0.9 0.9 0.7

                  else
                    Border.width 0
                , Border.roundEach
                    { topLeft = roundIf (not target)
                    , topRight = roundIf target
                    , bottomLeft = roundIf (not target)
                    , bottomRight = roundIf target
                    }
                ]
                { label = text label
                , onPress = Just <| Choice <| ChoiceCapBuild target
                }
    in
    Element.row [ Theme.rounded, Border.width 1, width fill ]
        [ button False "Initial"
        , el [ height fill, width <| px 1, Background.color <| rgb 0 0 0 ] Element.none
        , button True "Cap"
        ]


rightPoints : Points -> Element msg
rightPoints value =
    let
        wrap : String -> Int -> String -> String
        wrap before v after =
            if v == 0 then
                ""

            else
                before ++ fromInt v ++ after

        fromInt : Int -> String
        fromInt v =
            if v < 0 then
                String.fromInt v

            else
                "+" ++ String.fromInt v

        joined : String
        joined =
            String.join " " <|
                List.filter (not << String.isEmpty) <|
                    [ wrap "[" value.power "]"
                    , wrap "{" value.rewardPoints "}"
                    ]

        fullString : String
        fullString =
            if String.isEmpty joined then
                emptyRowContent

            else
                joined
    in
    rightText fullString


emptyRowContent : String
emptyRowContent =
    "[-] {-}"


rightText : String -> Element msg
rightText value =
    el
        [ alignRight
        , Theme.captureIt
        , Font.size 20
        ]
        (Theme.blocks [] value)


capSlider : Model -> Element Msg
capSlider model =
    let
        label : String
        label =
            "Complications power to cap power"
    in
    case model.gameMode of
        Nothing ->
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
                        paragraph [ Font.bold ]
                            [ text label
                            , rightPoints <| Costs.powerToPoints model.towardsCap
                            ]
                , min = 0
                , max = toFloat <| Results.withDefault 0 <| Costs.complicationsRawValue model
                , value = toFloat model.towardsCap
                , step = Just 1
                , thumb = Input.defaultThumb
                }

        Just Types.EarlyBird ->
            Element.none

        Just Types.StoryArc ->
            row label
                (Results.map Costs.powerToPoints <| Costs.complicationsRawValue model)
                (Just "Game Mode")

        Just Types.SkillTree ->
            Element.none

        Just Types.Constellation ->
            Element.none


relicSlider : Model -> Element Msg
relicSlider model =
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
        { onChange = \newValue -> Choice <| PowerToRewards <| round newValue
        , label =
            Input.labelAbove [] <|
                Theme.row []
                    [ paragraph [ Font.bold ]
                        [ text "Convert power into reward points or vice versa"
                        , let
                            zero : Points
                            zero =
                                Costs.zero
                          in
                          rightPoints
                            { zero
                                | power = -model.powerToRewards
                                , rewardPoints = model.powerToRewards
                            }
                        ]
                    ]
        , min = negate <| toFloat <| Results.withDefault 0 <| Results.map .rewardPoints <| Costs.classValue model
        , max = negate <| toFloat <| Results.withDefault 0 <| Results.map .rewardPoints <| Costs.relicsValue model
        , value = toFloat model.powerToRewards
        , step = Just 1
        , thumb = Input.defaultThumb
        }


linkLabel : String -> Maybe String -> Element Msg
linkLabel label target =
    Input.button []
        { onPress = Just <| ScrollTo <| String.Extra.underscored <| Maybe.withDefault label target
        , label = el [ Font.bold, width fill ] <| text label
        }
