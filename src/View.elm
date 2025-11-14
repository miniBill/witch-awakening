module View exposing (collapsible, costButtons, filterDLC, viewRequirements)

import Color exposing (Color)
import Data.Costs.Utils as Utils
import Element exposing (Attribute, Element, centerX, centerY, el, fill, px, spacing, text, width)
import Element.Font as Font
import Generated.Class as Class
import Generated.Gradient as Gradient
import Generated.Magic as Magic
import Generated.Perk as Perk
import Generated.Quest as Quest
import List.Extra
import Parser
import Set exposing (Set)
import Theme
import Types exposing (Display(..), IdKind)


collapsible :
    List (Attribute msg)
    -> Display
    -> (Display -> msg)
    -> (innerMsg -> msg)
    -> IdKind
    -> String
    -> List (Element innerMsg)
    -> List (Element innerMsg)
    -> Element msg
collapsible attrs display displayMsg choiceMsg kind title full compact =
    case display of
        DisplayFull ->
            Theme.column
                ([ width fill
                 , spacing <| Theme.rhythm * 2
                 ]
                    ++ attrs
                )
            <|
                Theme.collapsibleBlocks displayMsg display [] kind title
                    :: List.map (Element.map choiceMsg) full

        DisplayCompact ->
            Theme.column
                [ width fill
                , spacing <| Theme.rhythm * 2
                ]
            <|
                Theme.collapsibleBlocks displayMsg display [] kind title
                    :: List.map (Element.map choiceMsg) compact

        DisplayCollapsed ->
            Theme.collapsibleBlocks displayMsg display [] kind title


costButtons :
    String
    -> Color
    -> List a
    -> IdKind
    -> String
    -> List Int
    -> (Int -> Int -> a)
    -> List (Element ( a, Bool ))
costButtons label color selected kind before costs builder =
    let
        children : List (Element ( a, Bool ))
        children =
            [ el [ Font.bold ] <| text <| label ++ ":"
            , costs
                |> List.indexedMap
                    (\index cost ->
                        costButton
                            color
                            selected
                            (builder index cost)
                            cost
                    )
                |> Theme.wrappedRow []
            ]
    in
    Theme.blocks [] kind before
        :: children


costButton : Color -> List c -> c -> Int -> Element ( c, Bool )
costButton color selected item label =
    let
        isChoiceSelected : Bool
        isChoiceSelected =
            List.member item selected

        attrs : List (Attribute msg)
        attrs =
            if isChoiceSelected then
                [ Theme.backgroundColor color ]

            else
                []
    in
    Theme.button
        ((width <| px 24) :: attrs)
        { label =
            String.fromInt label
                |> Theme.gradientText 4 Gradient.yellowGradient
                |> el [ centerX, centerY, Theme.captureIt ]
        , onPress = Just ( item, not isChoiceSelected )
        }


filterDLC : Set String -> List { a | dlc : Maybe String } -> List { a | dlc : Maybe String }
filterDLC hideDLC list =
    List.Extra.removeWhen
        (\item ->
            case item.dlc of
                Just dlc ->
                    Set.member dlc hideDLC

                Nothing ->
                    Set.member "Core" hideDLC
        )
        list


viewRequirements : IdKind -> String -> Element msg
viewRequirements kind req =
    case Parser.run Utils.requisitesParser req of
        Ok requisites ->
            let
                requisitesString : String
                requisitesString =
                    requisites
                        |> List.map
                            (\requisite ->
                                case requisite of
                                    Utils.RequiresClass reqClass ->
                                        "[" ++ Class.toString reqClass ++ "] " ++ Class.toString reqClass

                                    Utils.RequiresMagic reqMagic rank ->
                                        "[" ++ Magic.toString reqMagic ++ "] " ++ String.fromInt rank

                                    Utils.RequiresQuest reqQuest ->
                                        "[" ++ Quest.toString reqQuest ++ "]"

                                    Utils.RequiresPerk reqPerk ->
                                        "[" ++ Perk.toString reqPerk ++ "]"
                            )
                        |> String.join ", "
            in
            Theme.blocks [] kind ("_Requires " ++ requisitesString ++ "._")

        Err _ ->
            Theme.blocks [] kind ("_Requires " ++ req ++ "._")
