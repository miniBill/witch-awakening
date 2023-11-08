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
import Types exposing (Choice(..), Display, RankedPerk)
import View


viewPerks : Display -> List RankedPerk -> Element Choice
viewPerks display perks =
    View.collapsible (Theme.topBackground Images.perkIntro)
        display
        DisplayPerks
        ChoicePerk
        "# Perks"
        [ introBlock
        , Perk.all
            |> List.map (perkBox display perks)
            |> Theme.wrappedRow
                [ centerX
                , spacing <| Theme.rythm * 3
                ]
        ]
        [ Perk.all
            |> List.map (perkBox display perks)
            |> Theme.column
                [ centerX
                , spacing <| Theme.rythm * 3
                ]
        ]


introBlock : Element msg
introBlock =
    Theme.column
        [ width fill
        , spacing <| Theme.rythm * 2
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
    -> Perk.Details
    -> Element ( RankedPerk, Bool )
perkBox display selected ({ name, affinity, class, content, isMeta } as perk) =
    let
        isSelected : Maybe RankedPerk
        isSelected =
            List.Extra.find (\sel -> sel.name == name) selected

        msg : Maybe ( RankedPerk, Bool )
        msg =
            case ( content, isSelected ) of
                ( _, Just selectedPerk ) ->
                    Just ( selectedPerk, False )

                ( Single cost _, Nothing ) ->
                    Just ( { name = name, cost = cost }, True )

                ( WithChoices _ _ _, Nothing ) ->
                    Nothing

                ( WithChoicesHybridize _ _, Nothing ) ->
                    Nothing

                ( WithCosts _ _, Nothing ) ->
                    Nothing

        costs : List Int
        costs =
            (case content of
                WithChoices _ choices _ ->
                    List.map Tuple.second choices

                WithChoicesHybridize _ choices ->
                    List.map Tuple.second choices

                WithCosts _ k ->
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
    in
    Theme.card []
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
        , content = viewContent selected perk
        , onPress = msg
        }


viewContent : List RankedPerk -> Perk.Details -> List (Element ( RankedPerk, Bool ))
viewContent selected { content, name } =
    case content of
        Single _ block ->
            [ Theme.blocks
                [ height fill
                , width fill
                , Theme.padding
                ]
                block
            ]

        WithChoices before choices after ->
            let
                choicesView : List (Element ( RankedPerk, Bool ))
                choicesView =
                    List.map (viewChoice selected name) choices
            in
            [ Theme.column [ height fill, width fill, Theme.padding ] <|
                Theme.blocks [] before
                    :: choicesView
                    ++ [ Theme.blocks [] after ]
            ]

        WithChoicesHybridize before choices ->
            let
                choicesView : List (Element ( RankedPerk, Bool ))
                choicesView =
                    List.map (viewChoice selected name) choices
            in
            [ Theme.column [ height fill, width fill, Theme.padding ] <|
                Theme.blocks [] before
                    :: choicesView
            ]

        WithCosts before costs ->
            let
                costsView : List (Element ( RankedPerk, Bool ))
                costsView =
                    [ el [ Font.bold ] <| text "Cost:"
                    , costs
                        |> List.map (viewCost selected name)
                        |> Theme.wrappedRow []
                    ]
            in
            [ Theme.column [ height fill, width fill, Theme.padding ] <|
                Theme.blocks [] before
                    :: costsView
            ]


viewChoice : List RankedPerk -> Types.Perk -> ( String, Int ) -> Element ( RankedPerk, Bool )
viewChoice selected name ( label, cost ) =
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
    Theme.button
        (attrs [ width fill ])
        { label =
            Theme.blocks []
                (if String.endsWith ";" label || String.endsWith "," label then
                    "- " ++ label

                 else
                    "- " ++ label ++ "."
                )
        , onPress = Just ( perk, not isChoiceSelected )
        }


viewCost : List RankedPerk -> Types.Perk -> Int -> Element ( RankedPerk, Bool )
viewCost selected name cost =
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
    Theme.button
        (attrs [ width <| px 24 ])
        { label =
            String.fromInt cost
                |> Theme.gradientText 4 Gradients.yellowGradient
                |> el [ centerX, centerY, Theme.captureIt ]
        , onPress = Just ( perk, not isChoiceSelected )
        }


color : Int
color =
    0x00F3EA6F
