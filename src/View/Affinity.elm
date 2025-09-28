module View.Affinity exposing (button)

import Element exposing (Element)
import Element.Border as Border
import Element.Input as Input
import Generated.Affinity
import Generated.Types exposing (Affinity)
import Theme


button : Bool -> msg -> Affinity -> Element msg
button isSelected msg to =
    Input.button
        [ if isSelected then
            Border.glow (Theme.colorToElmUi (Generated.Affinity.affinityToColor to)) 4

          else
            Border.width 0
        , Border.rounded 999
        ]
        { onPress = Just msg
        , label = Theme.viewAffinity to
        }
