module View.Menu exposing (viewMenu)

import Data.Affinity as Affinity
import Data.Costs as Costs exposing (Points)
import Data.Costs.Monad as CostsMonad
import Element exposing (Attribute, Element, alignBottom, alignRight, alignTop, centerX, centerY, el, fill, height, padding, paragraph, px, rgb, scrollbarY, shrink, text, textColumn, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Keyed
import Generated.Types as Types exposing (Affinity)
import List.Extra
import List.Nonempty
import Parser exposing ((|.), (|=), Parser)
import Set exposing (Set)
import String.Extra
import Theme
import Types exposing (Choice(..), Model, Msg(..))


viewMenu : Model key -> Element Msg
viewMenu model =
    let
        totalPoints : CostsMonad.Monad Points
        totalPoints =
            Costs.totalCost model

        ( rawWarnings, errors ) =
            case totalPoints of
                Ok points ->
                    ( List.Extra.unique points.warnings, [] )

                Err es ->
                    ( [], List.Extra.unique <| List.Nonempty.toList es )

        affinities : List Affinity
        affinities =
            Affinity.fromModel model
                |> List.Extra.remove Types.All

        warnings : List String
        warnings =
            if List.isEmpty affinities then
                "No main race selected" :: rawWarnings

            else
                rawWarnings
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
        [ Theme.button
            [ if List.isEmpty errors then
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
            viewCalculations model totalPoints warnings affinities

          else
            Element.none
        ]


menuLabel : Model key -> CostsMonad.Monad Points -> List String -> String
menuLabel model totalPointsResult warnings =
    let
        availablePoints : CostsMonad.Monad Points
        availablePoints =
            if model.capBuild || model.gameMode == Just Types.EarlyBird then
                Costs.powerCap model

            else
                Costs.startingValue model
    in
    case
        CostsMonad.map3 (\t a r -> ( t, a, r ))
            totalPointsResult
            availablePoints
            (Costs.totalRewards model)
    of
        Ok { value } ->
            let
                ( totalPoints, available, rewards ) =
                    value

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

        Err _ ->
            "[E]"


wrapInt : String -> Int -> String -> String
wrapInt before value after =
    before ++ String.fromInt value ++ after


viewCalculations : Model key -> CostsMonad.Monad Points -> List String -> List Affinity -> Element Msg
viewCalculations model power warnings affinities =
    let
        resultRow : ( String, Element Msg )
        resultRow =
            keyedRow
                "Result"
                model.showInfo
                (power
                    |> CostsMonad.map Costs.negate
                    |> Result.mapError (\_ -> List.Nonempty.singleton "There are errors in the computation")
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
            , Theme.button
                [ width fill
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
        , keyedRow "Class" model.showInfo (Costs.classValue model) <| Just "True Form - Class"
        , link "Race" <| Just "True Form - Race"
        , keyedRow "Starting power" model.showInfo (Costs.startingValue model) <| Just "Game Mode"
        , keyedRow "Complications" model.showInfo (Costs.complicationsValue model) Nothing
        , keyedRow "Type perks" model.showInfo (Costs.typePerksValue model) Nothing
        , keyedRow "Magic" model.showInfo (Costs.magicsValue model) <| Just "The Magic"
        , keyedRow "Perks" model.showInfo (Costs.perksValue model) Nothing
        , keyedRow "Faction" model.showInfo (CostsMonad.succeed (Costs.factionValue model)) <| Just "Factions"
        , keyedRow "Companions" model.showInfo (Costs.companionsValue model) Nothing
        , ( "RelicSlider", relicSlider model )
        , keyedRow "Relics" model.showInfo (Costs.relicsValue model) Nothing
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
        , if List.isEmpty affinities then
            ( "Affinities", Element.none )

          else
            section [ alignBottom ] "Affinities"
        , ( "AffinitiesColumn"
          , if List.isEmpty affinities then
                Element.none

            else
                Theme.wrappedRow [] <|
                    List.map Theme.viewAffinity affinities
          )
        , ( "Cap Slider", capSlider model )
        , keyedRow "Power cap" model.showInfo (Costs.powerCap model) <| Just "Game Mode"
        , button
            { onPress = CompactAll
            , label = "Compact all"
            }
        , button
            { onPress = ExpandAll
            , label = "Expand all"
            }
        ]


keyedRow : String -> Set String -> CostsMonad.Monad Points -> Maybe String -> ( String, Element Msg )
keyedRow label showInfo result target =
    ( label, row label showInfo result target )


row : String -> Set String -> CostsMonad.Monad Points -> Maybe String -> Element Msg
row label showInfo result target =
    case result of
        Err es ->
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
                        (List.Nonempty.toList es)
            in
            Theme.column [ width fill ] <|
                linkLabel label target
                    :: errorViews

        Ok value ->
            let
                lineParser : Parser ( String, Int )
                lineParser =
                    Parser.succeed Tuple.pair
                        |= (Parser.chompWhile (\c -> c /= ':')
                                |> Parser.getChompedString
                                |> Parser.map String.trim
                           )
                        |. Parser.symbol ":"
                        |. Parser.spaces
                        |= (Parser.succeed identity
                                |= Parser.oneOf
                                    [ Parser.succeed negate |. Parser.symbol "-"
                                    , Parser.succeed identity
                                    ]
                                |= (Parser.chompWhile Char.isDigit
                                        |> Parser.getChompedString
                                        |> Parser.andThen
                                            (\raw ->
                                                case String.toInt raw of
                                                    Nothing ->
                                                        Parser.problem (raw ++ " is not a valid number")

                                                    Just n ->
                                                        Parser.succeed n
                                            )
                                   )
                           )
                        |. Parser.end

                viewInfoBlock : String -> List (Element msg)
                viewInfoBlock line =
                    case Parser.run lineParser line of
                        Ok ( key, lineValue ) ->
                            [ paragraph [ width fill ] [ text key ]
                            , el [ alignRight ] <|
                                rightPoints { rewardPoints = 0, power = lineValue }
                            ]

                        Err _ ->
                            [ paragraph [ width fill ] [ text line ], Element.none ]
            in
            Theme.column [ width fill ]
                [ paragraph [ width fill ]
                    [ linkLabel label target
                    , if List.isEmpty value.infos then
                        rightPoints value.value

                      else
                        Input.button [ alignRight ]
                            { label = rightPoints value.value
                            , onPress = ToggleInfo label |> Choice |> Just
                            }
                    ]
                , if Set.member label showInfo then
                    Theme.doubleColumn
                        [ width fill ]
                        ( fill, shrink )
                        (List.concatMap viewInfoBlock value.infos)

                  else
                    Element.none
                ]


capBuildSwitch : Model key -> Element Msg
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


capSlider : Model key -> Element Msg
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
                , max =
                    Costs.complicationsRawValue model
                        |> Result.map .value
                        |> Result.withDefault 0
                        |> toFloat
                , value = toFloat model.towardsCap
                , step = Just 1
                , thumb = Input.defaultThumb
                }

        Just Types.EarlyBird ->
            Element.none

        Just Types.StoryArc ->
            row
                label
                model.showInfo
                (Costs.complicationsRawValue model
                    |> CostsMonad.map Costs.powerToPoints
                )
                (Just "Game Mode")

        Just Types.SkillTree ->
            Element.none

        Just Types.Constellation ->
            Element.none


relicSlider : Model key -> Element Msg
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
        , min =
            Costs.classValue model
                |> Result.map (\{ value } -> value.rewardPoints)
                |> Result.withDefault 0
                |> toFloat
                |> negate
        , max =
            Costs.relicsValue model
                |> Result.map (\{ value } -> value.rewardPoints)
                |> Result.withDefault 0
                |> toFloat
                |> negate
        , value = toFloat model.powerToRewards
        , step = Just 1
        , thumb = Input.defaultThumb
        }


linkLabel : String -> Maybe String -> Element Msg
linkLabel label target =
    if label == "Result" then
        el [ Font.bold, width fill ] <| text label

    else
        Input.button [ Font.underline ]
            { onPress = Just <| ScrollTo <| String.Extra.underscored <| Maybe.withDefault label target
            , label = el [ Font.bold, width fill ] <| text label
            }
