module View.FactionalMagic exposing (viewFactionalMagics)

import Data.FactionalMagic as FactionalMagic
import Element exposing (Element, centerX, el, fill, spacing, width)
import Element.Font as Font
import Generated.Types exposing (Slot(..))
import Gradients
import Theme
import Types exposing (Choice(..), RankedMagic)
import View.Magic as Magic


viewFactionalMagics : List RankedMagic -> Element Choice
viewFactionalMagics selected =
    Theme.column
        [ width fill
        , spacing <| Theme.rythm * 2
        ]
        [ el
            [ Theme.morpheus
            , centerX
            , Font.size 48
            ]
            (Theme.gradientText 2 Gradients.blueGradient "Factional Magic")
        , Theme.blocks [ centerX, width <| Element.maximum 800 fill ] FactionalMagic.intro
        , FactionalMagic.all
            |> List.indexedMap (Magic.magicBox selected)
            |> Theme.column []
            |> Element.map (\( ranked, select ) -> ChoiceMagic ranked select)
        ]
