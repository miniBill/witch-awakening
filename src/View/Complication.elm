module View.Complication exposing (viewComplications)

import Color exposing (Color)
import Data.Complication as Complication exposing (Content(..))
import Element exposing (Attribute, Element, alignBottom, alignRight, alignTop, centerX, el, fill, height, moveDown, moveRight, moveUp, px, spacing, width)
import Element.Border as Border
import Element.Font as Font
import Generated.Complication
import Generated.Types as Types exposing (ComplicationCategory(..), Slot(..))
import Gradients
import List.Extra
import Set exposing (Set)
import Theme exposing (gradientText)
import Types exposing (Choice(..), ComplicationKind(..), Display, RankedComplication)
import View


viewComplications : Set String -> Display -> List RankedComplication -> Element Choice
viewComplications hideDLC display complications =
    let
        filtered : List Complication.Details
        filtered =
            Generated.Complication.all
                |> View.filterDLC hideDLC
    in
    if List.isEmpty filtered then
        Element.none

    else
        let
            wrappedRow : List (Element msg) -> Element msg
            wrappedRow items =
                items
                    |> Theme.wrappedRow
                        [ centerX
                        , spacing <| Theme.rhythm * 3
                        ]

            isWorldShift : Complication.Details -> Bool
            isWorldShift complication =
                case complication.category of
                    Just ComplicationCategoryWorldShift ->
                        True

                    Nothing ->
                        False
        in
        View.collapsible []
            display
            DisplayComplications
            ChoiceComplication
            Complication.title
            [ Theme.blocks [] Complication.intro
            , Theme.blocks [] "# World Shifts"
            , ((filtered
                    |> List.filter isWorldShift
                    |> List.filterMap
                        (complicationBox display complications)
               )
                ++ [ Theme.blocks
                        [ width <| Element.maximum 400 fill
                        , alignTop
                        , Border.width 1
                        , Theme.padding
                        , Theme.borderColor Theme.colors.worldShift
                        ]
                        Complication.worldShiftsDescription
                   ]
              )
                |> wrappedRow
            , Theme.blocks [] "# Generic Complications"
            , filtered
                |> List.filter (\complication -> not (isWorldShift complication))
                |> List.filterMap (complicationBox display complications)
                |> wrappedRow
            ]
            [ filtered
                |> List.filterMap (complicationBox display complications)
                |> wrappedRow
            ]


complicationBox :
    Display
    -> List RankedComplication
    -> Complication.Details
    -> Maybe (Element ( RankedComplication, Bool ))
complicationBox display selected ({ name, class, category, content, dlc } as complication) =
    let
        isSelected : Maybe RankedComplication
        isSelected =
            List.Extra.find (\sel -> sel.name == name) selected

        msg : Maybe ( RankedComplication, Bool )
        msg =
            case ( content, isSelected ) of
                ( _, Just selectedComplication ) ->
                    Just ( selectedComplication, False )

                ( Single _ _, Nothing ) ->
                    Just ( { name = name, kind = Nontiered }, True )

                ( WithTiers _ _ _, Nothing ) ->
                    Nothing

                ( WithChoices _ _ _, Nothing ) ->
                    Nothing

                ( WithGains _ _, Nothing ) ->
                    Nothing

        gradient : List ( Int, Int, Int )
        gradient =
            category
                |> Maybe.map Theme.complicationCategoryToGradient
                |> Maybe.withDefault Gradients.blueGradient

        color : Color
        color =
            category
                |> Maybe.map Theme.complicationCategoryToColor
                |> Maybe.withDefault Theme.colors.folk

        gains : List Int
        gains =
            (case content of
                WithTiers _ tiers _ ->
                    List.map Tuple.second tiers

                WithChoices _ choices _ ->
                    List.map Tuple.second choices

                WithGains costs _ ->
                    costs

                Single gain _ ->
                    [ gain ]
            )
                |> List.filter ((/=) 0)
                |> List.Extra.unique

        gainGradient : Element msg
        gainGradient =
            if List.length gains >= 4 then
                (List.take 1 gains ++ List.take 1 (List.reverse gains))
                    |> List.map (\gain -> "+" ++ String.fromInt gain)
                    |> String.join "/.../"
                    |> gradientText 4 Gradients.yellowGradient

            else
                gains
                    |> List.map (\gain -> "+" ++ String.fromInt gain)
                    |> String.join "/"
                    |> gradientText 4 Gradients.yellowGradient

        viewSlot : Slot -> Element msg
        viewSlot slot =
            el [ alignRight ] <|
                Theme.image [ width <| px 40 ] <|
                    Types.slotToImage slot

        inFront : List (Element msg)
        inFront =
            [ case class of
                Nothing ->
                    Element.none

                Just c ->
                    el [ alignBottom ] <|
                        Theme.image [ width <| px 40 ] <|
                            Theme.classToBadge c
            , case gains of
                [] ->
                    Element.none

                [ g ] ->
                    viewSlot (Types.gainToSlot g)

                _ ->
                    viewSlot SlotWhite
            , case category of
                Just c ->
                    Element.column
                        [ alignTop
                        , Font.size 28
                        , centerX
                        , moveDown 8
                        ]
                        [ el [ centerX, Theme.captureIt ] <|
                            gradientText 4 gradient <|
                                Types.complicationCategoryToString c
                        , el [ centerX, Theme.captureIt ] gainGradient
                        ]

                Nothing ->
                    el
                        [ moveDown 16
                        , moveRight 16
                        , Font.size 28
                        , Theme.captureIt
                        ]
                        gainGradient
            , case dlc of
                Nothing ->
                    Element.none

                Just dlcName ->
                    el
                        [ centerX
                        , Theme.captureIt
                        , Font.size 24
                        , case ( category, content ) of
                            ( Just _, WithTiers _ _ _ ) ->
                                moveDown 64

                            ( Just _, _ ) ->
                                moveDown 40

                            ( Nothing, WithTiers _ _ _ ) ->
                                moveDown 40

                            ( Nothing, _ ) ->
                                moveDown 18
                        ]
                        (Theme.gradientText 4 Gradients.purpleGradient dlcName)
            , el
                [ alignBottom
                , Theme.celticHand
                , Font.size 32
                , centerX
                , moveUp 4
                ]
                (gradientText 4 gradient <|
                    Types.complicationToString name
                )
            ]
    in
    Theme.card [ Theme.id (Types.complicationToString name) ]
        { display = display
        , forceShow = False
        , glow = color
        , isSelected = isSelected /= Nothing
        , imageAttrs =
            [ Border.width 4
            , Theme.borderColor color
            ]
        , imageHeight = 400
        , image = Types.complicationToImage name
        , inFront = inFront
        , content = viewContent selected complication color
        , onPress = msg
        }


viewContent : List RankedComplication -> Complication.Details -> Color -> List (Element ( RankedComplication, Bool ))
viewContent selected { content, name } color =
    case content of
        Single _ block ->
            [ Theme.blocks
                [ height fill
                , Theme.padding
                ]
                block
            ]

        WithTiers before tiers after ->
            let
                viewTier : Int -> ( String, Int ) -> Element ( RankedComplication, Bool )
                viewTier tier ( label, _ ) =
                    let
                        complication : RankedComplication
                        complication =
                            { name = name
                            , kind = Tiered (tier + 1)
                            }

                        isTierSelected : Bool
                        isTierSelected =
                            List.member complication selected

                        attrs : List (Attribute msg)
                        attrs =
                            if isTierSelected then
                                [ Theme.backgroundColor color ]

                            else
                                []
                    in
                    Theme.button
                        (width fill :: attrs)
                        { label =
                            Theme.blocks []
                                ("- *Tier "
                                    ++ String.fromInt (tier + 1)
                                    ++ "*: "
                                    ++ label
                                    ++ "."
                                )
                        , onPress = Just ( complication, not isTierSelected )
                        }

                tiersView : List (Element ( RankedComplication, Bool ))
                tiersView =
                    List.indexedMap viewTier tiers
            in
            Theme.blocks [] before
                :: tiersView
                ++ [ Theme.blocks [] after ]

        WithChoices before choices after ->
            let
                choicesView : List (Element ( RankedComplication, Bool ))
                choicesView =
                    List.indexedMap viewChoice choices

                viewChoice : Int -> ( String, Int ) -> Element ( RankedComplication, Bool )
                viewChoice choice ( label, _ ) =
                    let
                        complication : RankedComplication
                        complication =
                            { name = name
                            , kind = Tiered (choice + 1)
                            }

                        isChoiceSelected : Bool
                        isChoiceSelected =
                            List.member complication selected

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
                                ("- "
                                    ++ label
                                    ++ "."
                                )
                        , onPress = Just ( complication, not isChoiceSelected )
                        }
            in
            Theme.blocks [] before
                :: choicesView
                ++ [ Theme.blocks [] after ]

        WithGains costs before ->
            View.costButtons "Gain" color selected before costs <|
                \tier _ -> { name = name, kind = Tiered (tier + 1) }
