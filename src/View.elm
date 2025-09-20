module View exposing (collapsible, costButtons, filterDLC)

import Element exposing (Attribute, Element, centerX, centerY, el, fill, px, spacing, text, width)
import Element.Font as Font
import Gradients
import List.Extra
import Set exposing (Set)
import Theme
import Types exposing (Display(..))


collapsible :
    List (Attribute msg)
    -> Display
    -> (Display -> msg)
    -> (innerMsg -> msg)
    -> String
    -> List (Element innerMsg)
    -> List (Element innerMsg)
    -> Element msg
collapsible attrs display displayMsg choiceMsg title full compact =
    case display of
        DisplayFull ->
            Theme.column
                ([ width fill
                 , spacing <| Theme.rhythm * 2
                 ]
                    ++ attrs
                )
            <|
                Theme.collapsibleBlocks displayMsg display [] title
                    :: List.map (Element.map choiceMsg) full

        DisplayCompact ->
            Theme.column
                [ width fill
                , spacing <| Theme.rhythm * 2
                ]
            <|
                Theme.collapsibleBlocks displayMsg display [] title
                    :: List.map (Element.map choiceMsg) compact

        DisplayCollapsed ->
            Theme.collapsibleBlocks displayMsg display [] title


costButtons :
    String
    -> Int
    -> List a
    -> String
    -> List Int
    -> (Int -> Int -> a)
    -> List (Element ( a, Bool ))
costButtons label color selected before costs builder =
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
    Theme.blocks [] before
        :: children


costButton : Int -> List c -> c -> Int -> Element ( c, Bool )
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
                |> Theme.gradientText 4 Gradients.yellowGradient
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
