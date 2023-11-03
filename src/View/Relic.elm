module View.Relic exposing (viewRelics)

import Data.Relic as Relic exposing (Content(..))
import Element exposing (Attribute, Element, alignBottom, alignRight, centerX, centerY, el, fill, height, moveDown, moveLeft, padding, paragraph, px, spacing, text, width)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Generated.Types as Types exposing (Slot(..))
import Gradients
import List.Extra
import String.Extra
import Theme exposing (gradientText)
import Types exposing (Choice(..), RankedRelic)


viewRelics : List RankedRelic -> Element Choice
viewRelics relics =
    Theme.column
        [ width fill
        , spacing <| Theme.rythm * 2
        ]
        [ Theme.column
            [ width fill
            , spacing <| Theme.rythm * 2
            ]
            [ Theme.blocks [ centerX ] Relic.intro
            ]
        , Relic.all
            |> List.map (relicBox relics)
            |> Theme.wrappedRow
                [ centerX
                , spacing <| Theme.rythm * 3
                ]
            |> Element.map (\( relic, selected ) -> ChoiceRelic relic selected)
        ]


relicBox :
    List RankedRelic
    -> Relic.Details
    -> Element ( RankedRelic, Bool )
relicBox selected ({ name, class, content } as relic) =
    let
        isSelected : Maybe RankedRelic
        isSelected =
            List.Extra.find (\sel -> sel.name == name) selected

        glow : Maybe Int
        glow =
            if isSelected == Nothing then
                Nothing

            else
                Just color

        msg : Maybe ( RankedRelic, Bool )
        msg =
            case ( content, isSelected ) of
                ( _, Just selectedRelic ) ->
                    Just ( selectedRelic, False )

                ( Single cost _, Nothing ) ->
                    Just ( { name = name, cost = cost }, True )

                ( WithChoices _ _, Nothing ) ->
                    Nothing

        costs : List Int
        costs =
            (case content of
                WithChoices _ choices ->
                    choices

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
                |> el [ moveDown 4 ]

        color : Int
        color =
            0x00F3EA6F
    in
    Theme.card []
        { glow = glow
        , imageAttrs = []
        , imageHeight = 400
        , image = Types.relicToImage name
        , inFront =
            [ el
                [ alignRight
                , Font.size 32
                , Theme.captureIt
                , moveLeft 4
                , moveDown 4
                ]
                costGradient
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
            , Types.relicToString name
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
        , content = viewContent selected relic color
        , onPress = msg
        }


viewContent : List RankedRelic -> Relic.Details -> Int -> List (Element ( RankedRelic, Bool ))
viewContent selected { content, name } color =
    case content of
        Single _ block ->
            [ Theme.blocks
                [ height fill
                , Theme.padding
                ]
                block
            ]

        WithChoices before choices ->
            let
                choicesView : List (Element ( RankedRelic, Bool ))
                choicesView =
                    [ el [ Font.bold ] <| text "Cost:"
                    , choices
                        |> List.map viewChoice
                        |> Theme.wrappedRow []
                    ]

                viewChoice : Int -> Element ( RankedRelic, Bool )
                viewChoice cost =
                    let
                        relic : RankedRelic
                        relic =
                            { name = name
                            , cost = cost
                            }

                        isChoiceSelected : Bool
                        isChoiceSelected =
                            List.member relic selected

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
                            String.fromInt cost
                                |> Theme.gradientText 4 Gradients.yellowGradient
                                |> el [ centerX, centerY, Theme.captureIt ]
                        , onPress = Just ( relic, not isChoiceSelected )
                        }
            in
            [ Theme.column [ height fill, Theme.padding ] <|
                Theme.blocks [] before
                    :: choicesView
            ]
