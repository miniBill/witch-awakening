module View exposing (collapsible)

import Element exposing (Element, fill, spacing, width)
import Theme
import Types exposing (Display(..))


collapsible :
    Display
    -> (Display -> msg)
    -> (innerMsg -> msg)
    -> String
    -> List (Element innerMsg)
    -> List (Element innerMsg)
    -> Element msg
collapsible display displayMsg choiceMsg title full compact =
    case display of
        DisplayFull ->
            Theme.column
                [ width fill
                , spacing <| Theme.rythm * 2
                ]
            <|
                Theme.collapsibleBlocks displayMsg display [] title
                    :: List.map (Element.map choiceMsg) full

        DisplayCompact ->
            Theme.column
                [ width fill
                , spacing <| Theme.rythm * 2
                ]
            <|
                Theme.collapsibleBlocks displayMsg display [] title
                    :: List.map (Element.map choiceMsg) compact

        DisplayCollapsed ->
            Theme.collapsibleBlocks displayMsg display [] title
