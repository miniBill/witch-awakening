module View.Complication exposing (viewComplications)

import Data.Complication as Complication exposing (Content(..))
import Element exposing (Attribute, Element, alignBottom, alignRight, alignTop, centerX, centerY, el, fill, height, moveDown, moveRight, moveUp, padding, px, spacing, text, width)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Generated.Types as Types exposing (ComplicationCategory, Slot(..))
import Gradients
import List.Extra
import Theme exposing (gradientText)
import Types exposing (Choice(..), ComplicationKind(..), RankedComplication)


viewComplications : List RankedComplication -> Element Choice
viewComplications complications =
    Theme.column
        [ width fill
        , spacing <| Theme.rythm * 2
        ]
        [ Theme.blocks [] Complication.intro
        , Theme.blocks [] "# World Shifts"
        , (List.map
            (complicationBox complications)
            Complication.worldShifts
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
            |> Theme.wrappedRow
                [ centerX
                , spacing <| Theme.rythm * 3
                ]
            |> Element.map (\( complication, selected ) -> ChoiceComplication complication selected)
        , Theme.blocks [] "# Generic Complications"
        , Complication.generic
            |> List.map (complicationBox complications)
            |> Theme.wrappedRow
                [ centerX
                , spacing <| Theme.rythm * 3
                ]
            |> Element.map (\( complication, selected ) -> ChoiceComplication complication selected)
        ]


complicationBox :
    List RankedComplication
    -> Complication.Details
    -> Element ( RankedComplication, Bool )
complicationBox selected ({ name, class, content } as complication) =
    let
        isSelected : Maybe RankedComplication
        isSelected =
            List.Extra.find (\sel -> sel.name == name) selected

        category : Maybe ComplicationCategory
        category =
            Types.complicationToCategory name

        glow : Maybe Int
        glow =
            if isSelected == Nothing then
                Nothing

            else
                Just color

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

                ( WithCosts _ _, Nothing ) ->
                    Nothing

        gradient : List ( Int, Int, Int )
        gradient =
            category
                |> Maybe.map Theme.complicationCategoryToGradient
                |> Maybe.withDefault Gradients.blueGradient

        color : Int
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

                WithCosts _ costs ->
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
    in
    Theme.card []
        { glow = glow
        , imageAttrs =
            [ Border.width 4
            , Theme.borderColor color
            ]
        , imageHeight = 400
        , image = Types.complicationToImage name
        , inFront =
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
                    viewSlot White
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
        , content = viewContent selected complication color
        , onPress = msg
        }


viewContent : List RankedComplication -> Complication.Details -> Int -> List (Element ( RankedComplication, Bool ))
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
            [ Theme.column [ height fill, Theme.padding ] <|
                Theme.blocks [] before
                    :: List.indexedMap
                        (\tier ( label, _ ) ->
                            let
                                complication : RankedComplication
                                complication =
                                    { name = name
                                    , kind = Tiered (tier + 1)
                                    }

                                isTierSelected : Bool
                                isTierSelected =
                                    List.member complication selected
                            in
                            Input.button
                                [ if isTierSelected then
                                    Theme.backgroundColor color

                                  else
                                    Border.width 1
                                , Border.width 1
                                , Border.rounded 4
                                , padding 4
                                ]
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
                        )
                        tiers
                    ++ [ Theme.blocks [] after ]
            ]

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

                        attrs : List (Attribute msg) -> List (Attribute msg)
                        attrs =
                            if isChoiceSelected then
                                (::) (Theme.backgroundColor color)

                            else
                                identity
                    in
                    Input.button
                        (attrs
                            [ Border.rounded 4
                            , padding 4
                            , Border.width 1
                            , width fill
                            ]
                        )
                        { label =
                            Theme.blocks []
                                ("- "
                                    ++ label
                                    ++ "."
                                )
                        , onPress = Just ( complication, not isChoiceSelected )
                        }
            in
            [ Theme.column [ height fill, Theme.padding ] <|
                Theme.blocks [] before
                    :: choicesView
                    ++ [ Theme.blocks [] after ]
            ]

        WithCosts before costs ->
            let
                choicesView : List (Element ( RankedComplication, Bool ))
                choicesView =
                    [ el [ Font.bold ] <| text "Cost:"
                    , costs
                        |> List.indexedMap viewChoice
                        |> Theme.wrappedRow []
                    ]

                viewChoice : Int -> Int -> Element ( RankedComplication, Bool )
                viewChoice choice value =
                    let
                        complication : RankedComplication
                        complication =
                            { name = name
                            , kind = Tiered (choice + 1)
                            }

                        isChoiceSelected : Bool
                        isChoiceSelected =
                            List.member complication selected

                        attrs : List (Attribute msg) -> List (Attribute msg)
                        attrs =
                            if isChoiceSelected then
                                (::) (Theme.backgroundColor color)

                            else
                                identity
                    in
                    Input.button
                        (attrs
                            [ Border.rounded 4
                            , padding 4
                            , Border.width 1
                            , width <| px 24
                            ]
                        )
                        { label =
                            el [ centerX, centerY, Theme.captureIt ] <|
                                Theme.gradientText 4 Gradients.yellowGradient <|
                                    String.fromInt value
                        , onPress = Just ( complication, not isChoiceSelected )
                        }
            in
            [ Theme.column [ height fill, Theme.padding ] <|
                Theme.blocks [] before
                    :: choicesView
            ]
