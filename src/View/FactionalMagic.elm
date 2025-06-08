module View.FactionalMagic exposing (viewFactionalMagics)

import Data.FactionalMagic as FactionalMagic
import Element exposing (Element, centerX, fill, width)
import Generated.Magics
import Theme
import Types exposing (Choice(..), Display, RankedMagic)
import View
import View.Magic as Magic


viewFactionalMagics : Display -> List RankedMagic -> Element Choice
viewFactionalMagics display selected =
    let
        boxes : Element ( RankedMagic, Bool )
        boxes =
            Generated.Magics.all
                |> List.filter (\{ faction } -> faction /= Nothing)
                |> List.indexedMap (Magic.magicBox display True selected)
                |> Theme.column []
    in
    View.collapsible []
        display
        DisplayFactionalMagic
        ChoiceMagic
        "# Factional Magic"
        [ Theme.blocks [ centerX, width <| Element.maximum 800 fill ] FactionalMagic.intro
        , boxes
        ]
        [ boxes ]
