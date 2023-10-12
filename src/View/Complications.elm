module View.Complications exposing (viewComplications)

import Data.Complication as Complication exposing (Content(..))
import Element exposing (Element, alignBottom, alignRight, alignTop, centerX, el, fill, height, moveDown, moveRight, moveUp, padding, px, spacing, width)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Generated.Types as Types exposing (ComplicationCategory, Slot(..))
import Gradients
import List.Extra
import Theme exposing (gradientText)
import Types exposing (Choice(..), Complication, ComplicationKind(..))


viewComplications : List Complication -> Element Choice
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
        , Complication.all
            |> List.map (complicationBox complications)
            |> Theme.wrappedRow
                [ centerX
                , spacing <| Theme.rythm * 3
                ]
            |> Element.map (\( complication, selected ) -> ChoiceComplication complication selected)
        ]


complicationBox :
    List Complication
    -> Complication.Details
    -> Element ( Complication, Bool )
complicationBox selected { name, class, content } =
    let
        isSelected : Maybe Complication
        isSelected =
            List.Extra.find (\sel -> sel.name == name) selected

        category : Maybe ComplicationCategory
        category =
            Types.complicationNameToCategory name

        glow : Maybe Int
        glow =
            if isSelected == Nothing then
                Nothing

            else
                Just color

        msg : Maybe ( Complication, Bool )
        msg =
            case ( content, isSelected ) of
                ( _, Just complication ) ->
                    Just ( complication, False )

                ( Single _ _, Nothing ) ->
                    Just ( { name = name, kind = Nontiered }, True )

                ( WithTiers _ _ _, Nothing ) ->
                    Nothing

                ( WithChoices _ _ _, Nothing ) ->
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

                Single gain _ ->
                    [ gain ]
            )
                |> List.filter ((/=) 0)
                |> List.Extra.unique

        gainGradient : Element msg
        gainGradient =
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
    Theme.card
        { glow = glow
        , imageAttrs =
            [ Border.width 4
            , Theme.borderColor color
            ]
        , imageHeight = 400
        , image = Types.complicationNameToImage name
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
                    Types.complicationNameToString name
                )
            ]
        , content =
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
                                        complication : Complication
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
                    [ Theme.column [ height fill, Theme.padding ] <|
                        Theme.blocks [] before
                            :: List.indexedMap
                                (\choice ( label, _ ) ->
                                    let
                                        complication : Complication
                                        complication =
                                            { name = name
                                            , kind = Tiered (choice + 1)
                                            }

                                        isChoiceSelected : Bool
                                        isChoiceSelected =
                                            List.member complication selected
                                    in
                                    Input.button
                                        (if isChoiceSelected then
                                            [ Theme.backgroundColor color, Border.rounded 4, padding 4 ]

                                         else
                                            [ Border.rounded 4, padding 4 ]
                                        )
                                        { label =
                                            Theme.blocks []
                                                ("- "
                                                    ++ label
                                                    ++ "."
                                                )
                                        , onPress = Just ( complication, not isChoiceSelected )
                                        }
                                )
                                choices
                            ++ [ Theme.blocks [] after ]
                    ]
        , onPress = msg
        }
