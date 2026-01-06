module View.Perk exposing (perkToShortString, viewPerks)

import Color exposing (Color)
import Data.Perk exposing (Content(..))
import Element exposing (Attribute, Element, alignBottom, alignRight, centerX, el, fill, height, moveDown, moveLeft, moveUp, paddingXY, px, rgba, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Generated.Gradient as Gradient
import Generated.Image as Image
import Generated.Perk as Perk
import Generated.Race
import Generated.Types as Types exposing (Perk(..), Race(..), Slot(..))
import List.Extra
import Maybe.Extra
import Set exposing (Set)
import Theme
import Types exposing (Choice(..), Display, IdKind(..), RankedPerk)
import View
import View.Race


viewPerks : Set String -> Bool -> Display -> Maybe Race -> List Race -> List RankedPerk -> Element Choice
viewPerks hideDLC hideMeta display mainRace races perks =
    let
        filtered : List Perk.Details
        filtered =
            if hideMeta then
                Perk.all perks
                    |> List.filter (\perk -> not perk.isMeta)
                    |> View.filterDLC hideDLC

            else
                Perk.all perks
                    |> View.filterDLC hideDLC
    in
    if List.isEmpty filtered then
        Element.none

    else
        View.collapsible (Theme.topBackground Image.perkIntro)
            display
            DisplayPerks
            identity
            IdKindPerk
            "# Perks"
            [ introBlock
            , filtered
                |> List.filterMap (perkBox display perks mainRace races)
                |> Theme.wrappedRow
                    [ centerX
                    , spacing <| Theme.rhythm * 3
                    ]
            ]
            [ filtered
                |> List.filter isOverlong
                |> List.filterMap (perkBox display perks mainRace races)
                |> Theme.wrappedRow
                    [ centerX
                    , spacing <| Theme.rhythm * 3
                    ]
            , filtered
                |> List.Extra.removeWhen isOverlong
                |> List.filterMap (perkBox display perks mainRace races)
                |> Theme.wrappedRow
                    [ centerX
                    , spacing <| Theme.rhythm * 3
                    ]
            ]


isOverlong : Perk.Details -> Bool
isOverlong { name } =
    case name of
        PerkChargeSwap _ ->
            True

        _ ->
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
            IdKindPerk
            intro
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

                ( WithCosts _ _, Nothing ) ->
                    Nothing

        costs : List Int
        costs =
            (case content of
                WithChoices _ choices _ ->
                    List.map Tuple.second choices

                WithCosts k _ ->
                    k

                Single cost _ ->
                    [ cost ]
            )
                |> List.sort
                |> List.Extra.unique

        costToString : Int -> String
        costToString cost =
            if cost == 0 then
                "0"

            else if cost > 0 then
                "-" ++ String.fromInt cost

            else
                "+" ++ String.fromInt -cost

        costGradient : Element msg
        costGradient =
            if List.length costs >= 4 then
                (List.take 1 costs ++ List.take 1 (List.reverse costs))
                    |> List.map costToString
                    |> String.join "/.../"
                    |> Theme.gradientText 4 Gradient.yellowGradient

            else
                costs
                    |> List.map costToString
                    |> String.join "/"
                    |> Theme.gradientText 4 Gradient.yellowGradient

        viewSlot : Slot -> Element msg
        viewSlot slot =
            Types.slotToImage slot
                |> Theme.image [ width <| px 40 ]
                |> el [ alignBottom, moveUp 48 ]

        color : Color
        color =
            Color.rgb255 0xF3 0xEA 0x6F

        nameString : String
        nameString =
            perkToShortString name
    in
    Theme.card [ Theme.id IdKindPerk nameString ]
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
            , affinity
                |> List.map Theme.viewAffinity
                |> List.intersperse (Theme.gradientText 4 Gradient.yellowGradient "OR")
                |> Theme.column
                    [ moveLeft 8
                    , moveDown 4
                    ]
            , Theme.column
                [ centerX
                , paddingXY 64 8
                ]
                [ if isMeta then
                    Theme.gradientTextWrapped
                        [ centerX
                        , Theme.captureIt
                        , Font.size 32
                        ]
                        4
                        Gradient.yellowGradient
                        "META"

                  else
                    Element.none
                , case dlc of
                    Nothing ->
                        Element.none

                    Just dlcName ->
                        Theme.gradientTextWrapped
                            [ centerX
                            , Theme.captureIt
                            , Font.size 24
                            ]
                            4
                            Gradient.purpleGradient
                            dlcName
                ]
            , Theme.classToBadge class
                |> Theme.image [ width <| px 40 ]
                |> el [ alignBottom ]
            , case costs of
                [] ->
                    Element.none

                [ g ] ->
                    viewSlot (Types.gainToSlot g)

                _ ->
                    viewSlot SlotWhite
            , Theme.gradientTextWrapped
                [ alignBottom
                , Theme.celticHand
                , Font.size 36
                , centerX
                , Font.center
                , Element.paddingXY 32 0
                ]
                4
                Gradient.blueGradient
                nameString
            ]
        , content =
            case perk.requires of
                Nothing ->
                    viewContent mainRace races color selected perk

                Just req ->
                    View.viewRequirements IdKindPerk req :: viewContent mainRace races color selected perk
        , onPress = msg
        }


perkToShortString : Perk -> String
perkToShortString name =
    if Perk.containsDash name then
        Perk.toString name

    else
        Perk.toString name
            |> String.split "-"
            |> List.take 1
            |> String.concat


viewContent : Maybe Race -> List Race -> Color -> List RankedPerk -> Perk.Details -> List (Element Choice)
viewContent mainRace races color perks { content, name } =
    case content of
        Single _ block ->
            [ Theme.blocks [] IdKindPerk block
            ]

        WithChoices before choices after ->
            case name of
                PerkChargeSwap _ ->
                    let
                        choicesView : List (Element Choice)
                        choicesView =
                            List.map (viewChoice color perks name) choices

                        ( cost, swapRace ) =
                            perks
                                |> List.Extra.findMap
                                    (\perk ->
                                        case perk.name of
                                            PerkChargeSwap r ->
                                                Just ( perk.cost, Just r )

                                            _ ->
                                                Nothing
                                    )
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
                    Theme.blocks [] IdKindPerk before
                        :: choicesView
                        ++ racePicker "Pick the race:"
                            (\newRace ->
                                case newRace of
                                    Just r ->
                                        ChoicePerk
                                            ( { name = PerkChargeSwap r
                                              , cost = cost
                                              }
                                            , True
                                            )

                                    Nothing ->
                                        ChoicePerk
                                            ( { name = PerkChargeSwap <| Maybe.withDefault RaceNeutral swapRace
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

                _ ->
                    let
                        choicesView : List (Element Choice)
                        choicesView =
                            List.map (viewChoice color perks name) choices
                    in
                    if name == PerkHybridize then
                        Theme.blocks [] IdKindPerk before
                            :: choicesView
                            ++ racePicker "Pick your main race:" ChoiceMainRace color mainRace races
                            ++ [ Theme.blocks [] IdKindPerk after ]

                    else
                        Theme.blocks [] IdKindPerk before
                            :: choicesView
                            ++ [ Theme.blocks [] IdKindPerk after ]

        WithCosts costs before ->
            List.map (Element.map ChoicePerk) <|
                View.costButtons "Cost" color perks IdKindPerk before costs <|
                    \_ cost -> { name = name, cost = cost }


racePicker : String -> (Maybe Race -> Choice) -> Color -> Maybe Race -> List Race -> List (Element Choice)
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


viewChoice : Color -> List RankedPerk -> Types.Perk -> ( String, Int ) -> Element Choice
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
                IdKindPerk
                (if String.endsWith ";" label || String.endsWith "," label then
                    "- " ++ label

                 else
                    "- " ++ label ++ "."
                )
        , onPress = Just <| ChoicePerk ( perk, not isChoiceSelected )
        }


intro : String
intro =
    """
    Perks are an assortment of bonus traits ranging from added benefits of your new witch true form, to special magical abilities you learn or manifest in your transformation that differ from anything already shown. {choice *Perks cost POWER as shown in the corner. Like Magic, Perks have half the cost if you have the Affinity shown in the other corner. [???] is universally halved (Not pre-factored).*}
    """
