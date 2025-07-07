module View.Perk exposing (perkToShortString, viewPerks)

import Data.Perk as Perk exposing (Content(..))
import Element exposing (Attribute, Element, alignBottom, alignRight, centerX, el, fill, height, moveDown, moveLeft, moveUp, paragraph, px, rgba, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Generated.Perk
import Generated.Race
import Generated.Types as Types exposing (Perk(..), Race, Slot(..))
import Gradients
import Images
import List.Extra
import Maybe.Extra
import String.Extra
import Theme exposing (gradientText)
import Types exposing (Choice(..), Display, RankedPerk)
import View
import View.Race


viewPerks : Display -> Maybe Race -> List Race -> List RankedPerk -> Element Choice
viewPerks display mainRace races perks =
    let
        sorted : List Perk.Details
        sorted =
            Generated.Perk.all perks
    in
    View.collapsible (Theme.topBackground Images.perkIntro)
        display
        DisplayPerks
        identity
        "# Perks"
        [ introBlock
        , sorted
            |> List.filterMap (perkBox display perks mainRace races)
            |> Theme.wrappedRow
                [ centerX
                , spacing <| Theme.rhythm * 3
                ]
        ]
        [ sorted
            |> List.filter isOverlong
            |> List.filterMap (perkBox display perks mainRace races)
            |> Theme.wrappedRow
                [ centerX
                , spacing <| Theme.rhythm * 3
                ]
        , sorted
            |> List.Extra.removeWhen isOverlong
            |> List.filterMap (perkBox display perks mainRace races)
            |> Theme.wrappedRow
                [ centerX
                , spacing <| Theme.rhythm * 3
                ]
        ]


isOverlong : Perk.Details -> Bool
isOverlong { content } =
    case content of
        Perk.WithChoicesChargeSwap _ _ ->
            True

        Perk.Single _ _ ->
            False

        Perk.WithChoices _ _ _ ->
            False

        Perk.WithCosts _ _ ->
            False


introBlock : Element msg
introBlock =
    Theme.column
        [ width fill
        , spacing <| Theme.rhythm * 2
        ]
        [ let
            blackish : Element.Color
            blackish =
                rgba 0 0 0 0.75
          in
          Theme.blocks
            [ width <| Element.maximum 800 fill
            , centerX
            , Background.color blackish
            , Border.shadow
                { offset = ( 0, 0 )
                , size = 5
                , blur = 5
                , color = blackish
                }
            ]
            Perk.intro
        , el [ height <| px 200 ] Element.none
        ]


perkBox :
    Display
    -> List RankedPerk
    -> Maybe Race
    -> List Race
    -> Perk.Details
    -> Maybe (Element Choice)
perkBox display selected mainRace races ({ name, affinity, class, content, isMeta, dlc } as perk) =
    let
        isSelected : Maybe RankedPerk
        isSelected =
            List.Extra.find (\sel -> sel.name == name) selected

        msg : Maybe Choice
        msg =
            case ( content, isSelected ) of
                ( _, Just selectedPerk ) ->
                    Just <| ChoicePerk ( selectedPerk, False )

                ( Single cost _, Nothing ) ->
                    Just <| ChoicePerk ( { name = name, cost = cost }, True )

                ( WithChoices _ _ _, Nothing ) ->
                    Nothing

                ( WithChoicesChargeSwap _ _, Nothing ) ->
                    Nothing

                ( WithCosts _ _, Nothing ) ->
                    Nothing

        costs : List Int
        costs =
            (case content of
                WithChoices _ choices _ ->
                    List.map Tuple.second choices

                WithChoicesChargeSwap _ choices ->
                    List.map Tuple.second choices

                WithCosts k _ ->
                    k

                Single cost _ ->
                    [ cost ]
            )
                |> List.filter ((/=) 0)
                |> List.Extra.unique

        costToString : Int -> String
        costToString cost =
            if cost > 0 then
                "-" ++ String.fromInt cost

            else
                "+" ++ String.fromInt -cost

        costGradient : Element msg
        costGradient =
            if List.length costs >= 4 then
                (List.take 1 costs ++ List.take 1 (List.reverse costs))
                    |> List.map costToString
                    |> String.join "/.../"
                    |> gradientText 4 Gradients.yellowGradient

            else
                costs
                    |> List.map costToString
                    |> String.join "/"
                    |> gradientText 4 Gradients.yellowGradient

        viewSlot : Slot -> Element msg
        viewSlot slot =
            Types.slotToImage slot
                |> Theme.image [ width <| px 40 ]
                |> el [ alignBottom, moveUp 48 ]

        color : Int
        color =
            0x00F3EA6F

        nameString : String
        nameString =
            perkToShortString name
    in
    Theme.card [ Theme.id nameString ]
        { display = display
        , forceShow = False
        , glow = color
        , isSelected = isSelected /= Nothing
        , imageAttrs = []
        , imageHeight = 400
        , image = Types.perkToImage name
        , inFront =
            [ el
                [ alignRight
                , Font.size 32
                , Theme.captureIt
                , moveLeft 4
                , moveDown 4
                ]
                costGradient
            , el
                [ moveLeft 8
                , moveDown 4
                ]
                (Theme.viewAffinity affinity)
            , if isMeta then
                el
                    [ centerX
                    , Theme.captureIt
                    , Font.size 32
                    , moveDown 4
                    ]
                    (Theme.gradientText 4 Gradients.yellowGradient "META")

              else
                Element.none
            , case dlc of
                Nothing ->
                    Element.none

                Just dlcName ->
                    el
                        [ centerX
                        , Theme.captureIt
                        , Font.size 24
                        , moveDown
                            (if isMeta then
                                40

                             else
                                8
                            )
                        ]
                        (Theme.gradientText 4 Gradients.purpleGradient dlcName)
            , Theme.classToBadge class
                |> Theme.image [ width <| px 40 ]
                |> el [ alignBottom ]
            , case costs of
                [] ->
                    Element.none

                [ g ] ->
                    viewSlot (Types.gainToSlot g)

                _ ->
                    viewSlot White
            , nameString
                |> String.Extra.softBreak 16
                |> List.map (gradientText 4 Gradients.blueGradient)
                |> paragraph
                    [ alignBottom
                    , Theme.celticHand
                    , Font.size 36
                    , centerX
                    , Font.center
                    ]
            ]
        , content = viewContent mainRace races color selected perk
        , onPress = msg
        }


perkToShortString : Perk -> String
perkToShortString name =
    if name == JackOfAll then
        Types.perkToString name

    else
        Types.perkToString name
            |> String.split "-"
            |> List.take 1
            |> String.concat


viewContent : Maybe Race -> List Race -> Int -> List RankedPerk -> Perk.Details -> List (Element Choice)
viewContent mainRace races color selected { content, name } =
    case content of
        Single _ block ->
            [ Theme.blocks [] block
            ]

        WithChoices before choices after ->
            let
                choicesView : List (Element Choice)
                choicesView =
                    List.map (viewChoice color selected name) choices
            in
            if name == Hybridize then
                Theme.blocks [] before
                    :: choicesView
                    ++ racePicker "Pick your main race:" ChoiceMainRace color mainRace races
                    ++ [ Theme.blocks [] after ]

            else
                Theme.blocks [] before
                    :: choicesView
                    ++ [ Theme.blocks [] after ]

        WithChoicesChargeSwap before choices ->
            let
                choicesView : List (Element Choice)
                choicesView =
                    List.map (viewChoice color selected name) choices

                ( cost, swapRace ) =
                    List.Extra.findMap
                        (\perk ->
                            case perk.name of
                                ChargeSwap r ->
                                    Just ( perk.cost, Just r )

                                _ ->
                                    Nothing
                        )
                        selected
                        |> Maybe.Extra.withDefaultLazy
                            (\_ ->
                                ( choices
                                    |> List.head
                                    |> Maybe.map Tuple.second
                                    |> Maybe.withDefault 999
                                , Nothing
                                )
                            )
            in
            Theme.blocks [] before
                :: choicesView
                ++ racePicker "Pick the race:"
                    (\newRace ->
                        case newRace of
                            Just r ->
                                ChoicePerk
                                    ( { name = ChargeSwap r
                                      , cost = cost
                                      }
                                    , True
                                    )

                            Nothing ->
                                ChoicePerk
                                    ( { name = ChargeSwap <| Maybe.withDefault Types.Neutral swapRace
                                      , cost = cost
                                      }
                                    , False
                                    )
                    )
                    color
                    swapRace
                    (Generated.Race.all races
                        |> List.map .name
                    )

        WithCosts costs before ->
            List.map (Element.map ChoicePerk) <|
                View.costButtons "Cost" color selected before costs <|
                    \_ cost -> { name = name, cost = cost }


racePicker : String -> (Maybe Race -> Choice) -> Int -> Maybe Race -> List Race -> List (Element Choice)
racePicker label toMsg color mainRace races =
    let
        viewRace : Race -> Element Choice
        viewRace race =
            let
                ( attrs, newValue ) =
                    if Just race == mainRace then
                        ( [ Theme.backgroundColor color ], Nothing )

                    else
                        ( [], Just race )
            in
            Theme.button (width fill :: Font.center :: attrs)
                { label =
                    View.Race.raceToShortString race
                        |> text
                , onPress = Just <| toMsg newValue
                }
    in
    if List.length races > 1 then
        [ el [ Font.bold ] <| text label
        , races
            |> List.map viewRace
            |> Theme.wrappedRow [ width fill ]
        ]

    else
        []


viewChoice : Int -> List RankedPerk -> Types.Perk -> ( String, Int ) -> Element Choice
viewChoice color selected name ( label, cost ) =
    let
        perk : RankedPerk
        perk =
            { name = name
            , cost = cost
            }

        isChoiceSelected : Bool
        isChoiceSelected =
            List.member perk selected

        attrs : List (Attribute msg)
        attrs =
            if isChoiceSelected then
                [ Theme.backgroundColor color ]

            else
                []
    in
    Theme.button
        (width fill :: attrs)
        { label =
            Theme.blocks []
                (if String.endsWith ";" label || String.endsWith "," label then
                    "- " ++ label

                 else
                    "- " ++ label ++ "."
                )
        , onPress = Just <| ChoicePerk ( perk, not isChoiceSelected )
        }
