module View.FactionalMagic exposing (viewFactionalMagics)

import Data.FactionalMagic as FactionalMagic
import Element exposing (Element, centerX, fill, width)
import Theme
import Types exposing (Choice(..), Display, RankedMagic)
import View
import View.Magic as Magic


viewFactionalMagics : Display -> List RankedMagic -> Element Choice
viewFactionalMagics display selected =
    View.collapsible []
        display
        DisplayFactionalMagic
        (\( ranked, select ) -> ChoiceMagic ranked select)
        "# Factional Magic"
        [ Theme.blocks [ centerX, width <| Element.maximum 800 fill ] FactionalMagic.intro
        , FactionalMagic.all
            |> List.indexedMap (Magic.magicBox display True selected)
            |> Theme.column []
        ]
        [ FactionalMagic.all
            |> List.indexedMap (Magic.magicBox display True selected)
            |> Theme.column []
        ]
