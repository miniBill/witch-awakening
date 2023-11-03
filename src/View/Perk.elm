module View.Perk exposing (viewPerks)

import Data.Perk as Perk exposing (Content(..))
import Element exposing (Attribute, Element, alignBottom, alignRight, centerX, centerY, el, fill, height, moveDown, moveLeft, moveUp, padding, paragraph, px, rgba, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Generated.Types as Types exposing (Slot(..))
import Gradients
import Images
import List.Extra
import String.Extra
import Theme exposing (gradientText)
import Types exposing (Choice(..), RankedPerk)


viewPerks : List RankedPerk -> Element Choice
viewPerks perks =
    Theme.column
        [ width fill
        , spacing <| Theme.rythm * 2
        ]
        [ Theme.column
            ([ width fill
             , spacing <| Theme.rythm * 2
             ]
                ++ Theme.topBackground Images.perkIntro
            )
            [ Theme.blocks [] "# Perks"
            , let
                color : Element.Color
                color =
                    rgba 0 0 0 0.75
              in
              Theme.blocks
                [ width <| Element.maximum 800 fill
                , centerX
                , Background.color color
                , Border.shadow
                    { offset = ( 0, 0 )
                    , size = 5
                    , blur = 5
                    , color = color
                    }
                ]
                Perk.intro
            , el [ height <| px 200 ] Element.none
            ]
        , Perk.all
            |> List.map (perkBox perks)
            |> Theme.wrappedRow
                [ centerX
                , spacing <| Theme.rythm * 3
                ]
            |> Element.map (\( perk, selected ) -> ChoicePerk perk selected)
        ]


perkBox :
    List RankedPerk
    -> Perk.Details
    -> Element ( RankedPerk, Bool )
perkBox selected ({ name, affinity, class, content, isMeta } as perk) =
    let
        isSelected : Maybe RankedPerk
        isSelected =
            List.Extra.find (\sel -> sel.name == name) selected

        glow : Maybe Int
        glow =
            if isSelected == Nothing then
                Nothing

            else
                Just color

        msg : Maybe ( RankedPerk, Bool )
        msg =
            case ( content, isSelected ) of
                ( _, Just selectedPerk ) ->
                    Just ( selectedPerk, False )

                ( Single cost _, Nothing ) ->
                    Just ( { name = name, cost = cost }, True )

                ( WithChoices _ _ _, Nothing ) ->
                    Nothing

        costs : List Int
        costs =
            (case content of
                WithChoices _ choices _ ->
                    List.map Tuple.second choices

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
    in
    Theme.card []
        { glow = glow
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
            , Types.perkToString name
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
        , content = viewContent selected perk color
        , onPress = msg
        }


viewContent : List RankedPerk -> Perk.Details -> Int -> List (Element ( RankedPerk, Bool ))
viewContent selected { content, name } color =
    case content of
        Single _ block ->
            [ Theme.blocks
                [ height fill
                , Theme.padding
                ]
                block
            ]

        WithChoices before choices after ->
            let
                choicesView : List (Element ( RankedPerk, Bool ))
                choicesView =
                    if List.all (\( label, _ ) -> label == "-") choices then
                        [ el [ Font.bold ] <| text "Cost:"
                        , choices
                            |> List.map viewChoice
                            |> Theme.wrappedRow []
                        ]

                    else
                        List.map viewChoice choices

                viewChoice : ( String, Int ) -> Element ( RankedPerk, Bool )
                viewChoice ( label, cost ) =
                    let
                        perk : RankedPerk
                        perk =
                            { name = name
                            , cost = cost
                            }

                        isChoiceSelected : Bool
                        isChoiceSelected =
                            List.member perk selected

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
                            , if label == "-" then
                                width <| px 24

                              else
                                width fill
                            ]
                        )
                        { label =
                            if label == "-" then
                                String.fromInt cost
                                    |> Theme.gradientText 4 Gradients.yellowGradient
                                    |> el [ centerX, centerY, Theme.captureIt ]

                            else
                                Theme.blocks []
                                    (if String.endsWith ";" label || String.endsWith "," label then
                                        "- " ++ label

                                     else
                                        "- " ++ label ++ "."
                                    )
                        , onPress = Just ( perk, not isChoiceSelected )
                        }
            in
            [ Theme.column [ height fill, Theme.padding ] <|
                Theme.blocks [] before
                    :: choicesView
                    ++ [ Theme.blocks [] after ]
            ]
