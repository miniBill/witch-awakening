module View.Menu exposing (viewMenu)

import Data.Affinity as Affinity
import Data.Costs as Costs
import Data.Costs.Class
import Data.Costs.Companions
import Data.Costs.Complications
import Data.Costs.Factions
import Data.Costs.Magic
import Data.Costs.Monad as CostsMonad
import Data.Costs.Perks
import Data.Costs.Quests
import Data.Costs.Race
import Data.Costs.Relics
import Data.Costs.TypePerks
import Data.Costs.Utils as Costs exposing (Points)
import Dict exposing (Dict)
import Dict.Extra
import Element exposing (Attribute, Element, alignBottom, alignRight, alignTop, centerX, centerY, el, fill, height, padding, paragraph, px, rgb, scrollbarY, shrink, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Keyed
import Generated.Types as Types exposing (Affinity)
import List.Extra
import List.Nonempty
import Set exposing (Set)
import Theme
import Types exposing (Choice(..), Model, Msg(..))
import View.MagicPyramid


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

        warnIf : Bool -> a -> List a -> List a
        warnIf b msg list =
            if b then
                msg :: list

            else
                list

        warnings : List String
        warnings =
            (rawWarnings
                |> warnIf (List.isEmpty affinities) "No main race selected."
            )
                ++ badPyramid model.magic
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
            , Element.padding (Theme.rhythm * 3 // 2)
            , alignRight
            ]
            { onPress =
                if model.menuOpen then
                    Just CloseMenu

                else
                    Just OpenMenu
            , label = menuLabel totalPoints warnings
            }
        , if model.menuOpen then
            viewCalculations model totalPoints warnings affinities

          else
            Element.none
        ]


badPyramid : List Types.RankedMagic -> List String
badPyramid magics =
    let
        grouped : Dict Int Int
        grouped =
            magics
                |> Dict.Extra.groupBy .rank
                |> Dict.map (\_ group -> List.length group)

        get : Int -> Int
        get r =
            Dict.get r grouped
                |> Maybe.withDefault 0

        check5 : Maybe String
        check5 =
            if get 5 > get 4 then
                Just
                    ("Unbalanced magic pyramid: rank 5 has "
                        ++ String.fromInt (get 5)
                        ++ " magics, while rank 4 has "
                        ++ String.fromInt (get 4)
                        ++ "."
                    )

            else
                Nothing

        check4 : Maybe String
        check4 =
            if get 4 > get 1 + get 2 + get 3 then
                Just
                    ("Unbalanced magic pyramid: rank 4 has "
                        ++ String.fromInt (get 4)
                        ++ " magics, while rank 1, 2 and 3 have "
                        ++ String.fromInt (get 1 + get 2 + get 3)
                        ++ "."
                    )

            else
                Nothing
    in
    [ check5, check4 ]
        |> List.filterMap identity


menuLabel : CostsMonad.Monad Points -> List String -> Element msg
menuLabel result warnings =
    let
        children : List String
        children =
            case result of
                Ok { value } ->
                    let
                        warningsIcon : String
                        warningsIcon =
                            if List.isEmpty warnings then
                                ""

                            else
                                "[W] "

                        powerString : String
                        powerString =
                            "[" ++ String.fromInt -value.power ++ "]"
                    in
                    if value.rewardPoints == 0 then
                        [ warningsIcon
                        , powerString
                        ]

                    else
                        [ warningsIcon
                        , "{center} "
                            ++ powerString
                            ++ "\n\n{center} {"
                            ++ String.fromInt -value.rewardPoints
                            ++ "}"
                        ]

                Err _ ->
                    [ "[E]" ]
    in
    children
        |> List.map
            (\s ->
                if String.isEmpty s then
                    Element.none

                else
                    Theme.blocks [ centerX, centerY ] s
            )
        |> Theme.row [ centerX, centerY ]


viewCalculations : Model key -> CostsMonad.Monad Points -> List String -> List Affinity -> Element Msg
viewCalculations model power warnings affinities =
    let
        resultRow : ( String, Element Msg )
        resultRow =
            keyedRow
                "Result"
                model.expandedMenuSections
                (power
                    |> CostsMonad.map Costs.negate
                    |> Result.mapError (\_ -> List.Nonempty.singleton "There are errors in the computation")
                )
                Nothing

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
        , keyedRow "Class" model.expandedMenuSections (Data.Costs.Class.value model) <| Just "True Form - Class"
        , keyedRow "Race" model.expandedMenuSections (Data.Costs.Race.value model) <| Just "True Form - Race"
        , keyedRow "Starting power" model.expandedMenuSections (Costs.startingValue model) <| Just "Game Mode"
        , keyedRow "Complications" model.expandedMenuSections (Data.Costs.Complications.value model) Nothing
        , ( "Cap Slider", capSlider model )
        , keyedRow "Type perks" model.expandedMenuSections (Data.Costs.TypePerks.value model) Nothing
        , keyedRow "Magic" model.expandedMenuSections (Data.Costs.Magic.value { ignoreSorceressBonus = False } model) <| Just "The Magic"
        , magicPyramidRow model
        , keyedRow "Perks" model.expandedMenuSections (Data.Costs.Perks.value model) Nothing
        , keyedRow "Faction" model.expandedMenuSections (Data.Costs.Factions.value model) <| Just "Factions"
        , keyedRow "Companions" model.expandedMenuSections (Data.Costs.Companions.value model) Nothing
        , keyedRow "Quests" model.expandedMenuSections (Data.Costs.Quests.value model) Nothing
        , keyedRow "Relics" model.expandedMenuSections (Data.Costs.Relics.value model) Nothing
        , ( "RelicSlider", relicSlider model )
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
        , keyedRow "Power cap" model.expandedMenuSections (Data.Costs.Complications.powerCap model) <| Just "Game Mode"
        , button
            { onPress = CompactAll
            , label = "Compact all"
            }
        , button
            { onPress = ExpandAll
            , label = "Expand all"
            }
        ]


magicPyramidRow : Model key -> ( String, Element Msg )
magicPyramidRow model =
    let
        label : String
        label =
            "Magic pyramid"
    in
    ( label
    , Theme.column []
        [ Theme.row []
            [ chevronButton label model.expandedMenuSections
            , Input.button [ Font.bold ]
                { label = text label
                , onPress = ToggleMenuSectionExpansion label |> Choice |> Just
                }
            ]
        , if Set.member label model.expandedMenuSections then
            View.MagicPyramid.view model.magic
                |> Element.html
                |> el []

          else
            Element.none
        ]
    )


keyedRow : String -> Set String -> CostsMonad.Monad Points -> Maybe String -> ( String, Element Msg )
keyedRow label expandedMenuSections result target =
    ( label, row label expandedMenuSections result target )


row : String -> Set String -> CostsMonad.Monad Points -> Maybe String -> Element Msg
row label expandedMenuSections result target =
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
                viewInfoBlock : CostsMonad.Info -> List (Element Msg)
                viewInfoBlock info =
                    [ paragraph [ width fill, centerY ]
                        [ text "- "
                        , linkLabel info.label info.anchor
                        ]
                    , el [ alignRight, centerY ] <|
                        case info.value of
                            CostsMonad.Power power ->
                                rightPoints { rewardPoints = 0, power = power }

                            CostsMonad.RewardPoints rewardPoints ->
                                rightPoints { rewardPoints = rewardPoints, power = 0 }

                            CostsMonad.FreeBecause message ->
                                Theme.blocks [] message
                    ]
            in
            Theme.column [ width fill ]
                [ Theme.row [ width fill ]
                    [ if List.isEmpty value.infos then
                        Element.none

                      else
                        chevronButton label expandedMenuSections
                    , linkLabel label target
                    , if List.isEmpty value.infos then
                        rightPoints value.value

                      else
                        Input.button [ alignRight ]
                            { label = rightPoints value.value
                            , onPress = ToggleMenuSectionExpansion label |> Choice |> Just
                            }
                    ]
                , if Set.member label expandedMenuSections then
                    Theme.doubleColumn
                        [ width fill ]
                        ( fill, shrink )
                        (List.concatMap viewInfoBlock value.infos)

                  else
                    Element.none
                ]


chevronButton : String -> Set String -> Element Msg
chevronButton label expandedMenuSections =
    Input.button []
        { label =
            if Set.member label expandedMenuSections then
                text Theme.triangleDown

            else
                text Theme.triangleRight
        , onPress = ToggleMenuSectionExpansion label |> Choice |> Just
        }


capBuildSwitch : Model key -> Element Msg
capBuildSwitch { capBuild } =
    let
        roundIf : Bool -> number
        roundIf b =
            if b then
                Theme.rhythm

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
            let
                rawComplicationsValue : Int
                rawComplicationsValue =
                    Data.Costs.Complications.complicationsRawValue model
                        |> Result.map .value
                        |> Result.withDefault 0
            in
            Theme.slider [ width fill ]
                { onChange = \newValue -> Choice <| TowardsCap <| round newValue
                , label =
                    Input.labelAbove [] <|
                        paragraph [ Font.bold ]
                            [ text label
                            , rightPoints <| Costs.powerToPoints -model.towardsCap
                            ]
                , min =
                    (rawComplicationsValue - 30)
                        |> max 0
                        |> toFloat
                , max =
                    rawComplicationsValue
                        |> min 30
                        |> toFloat
                , value = toFloat model.towardsCap
                , step = Just 1
                , thumb = Nothing
                }

        Just Types.EarlyBird ->
            Element.none

        Just Types.StoryArc ->
            row
                label
                model.expandedMenuSections
                (Data.Costs.Complications.complicationsRawValue model
                    |> CostsMonad.map Costs.powerToPoints
                )
                (Just "Game Mode")

        Just Types.SkillTree ->
            Element.none

        Just Types.Constellation ->
            Element.none


relicSlider : Model key -> Element Msg
relicSlider model =
    Theme.slider [ width fill ]
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
            Data.Costs.Class.value model
                |> Result.map (\{ value } -> value.rewardPoints)
                |> Result.withDefault 0
                |> toFloat
                |> negate
        , max =
            Data.Costs.Relics.value model
                |> Result.map (\{ value } -> value.rewardPoints)
                |> Result.withDefault 0
                |> toFloat
                |> negate
        , value = toFloat model.powerToRewards
        , step = Just 1
        , thumb = Nothing
        }


linkLabel : String -> Maybe String -> Element Msg
linkLabel label target =
    if label == "Result" then
        el [ Font.bold, width fill ] <| text label

    else
        Input.button [ Font.underline ]
            { onPress = Just <| ScrollTo <| Maybe.withDefault label target
            , label = el [ Font.bold, width fill ] <| text label
            }
