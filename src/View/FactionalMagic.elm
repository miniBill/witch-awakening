module View.FactionalMagic exposing (viewFactionalMagics)

import Element exposing (Element, centerX, fill, width)
import Generated.Magic
import Theme
import Types exposing (Choice(..), Display, RankedMagic)
import View
import View.Magic as Magic


viewFactionalMagics : Display -> List RankedMagic -> Element Choice
viewFactionalMagics display selected =
    let
        boxes : Element ( RankedMagic, Bool )
        boxes =
            Generated.Magic.all
                |> List.filter (\{ faction } -> faction /= Nothing)
                |> List.indexedMap (Magic.magicBox display True selected)
                |> Theme.column []
    in
    View.collapsible []
        display
        DisplayFactionalMagic
        ChoiceMagic
        "# Factional Magic"
        [ Theme.blocks [ centerX, width <| Element.maximum 800 fill ] intro
        , boxes
        ]
        [ boxes ]


intro : String
intro =
    """
    Factional Magic are the various magic specializations that are associated with a specific faction where that specialization originates or is otherwise more common with them while being less common beyond it. This can be different per specialization. For example, Wands is well known by the very nature of Hawthorne where Hawthorne students usually move on throughout Witchdom and most people are welcome to study at Hawthorne and learn. On the other hand, you have Occultism, the magic of Hespatia, that is far less common as people don’t usually live to betray Hespatia and they aren’t exactly welcoming to sit-ins.

    If a Factional magic has a Rank 0 effect, then it is universally available to all witches all the same, but the evolution on that rank 0 effect requires the specialized knowledge known to the given faction.

    The two faction “magics” of Gadgetry and Integration are NOT magic at all, but nonetheless require Power to invest in as a measure of your fate and opportunities.
    """
