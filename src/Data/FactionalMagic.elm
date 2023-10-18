module Data.FactionalMagic exposing (Details, all, intro)

import Data.Magic exposing (Affinities)
import Generated.Types exposing (Affinity(..), Class(..), Faction, Magic(..))


type alias Details =
    { name : Magic
    , star : Bool
    , class : Class
    , faction : Faction
    , affinities : Affinities
    , description : String
    , ranks : List String
    }


all : List Details
all =
    []


intro : String
intro =
    """
    Factional Magic are the various magic specializations that are associated with a specific faction where that specialization originates or is otherwise more common with them while being less common beyond it. This can be different per specialization. For example, Wands is well known by the very nature of Hawthorne where Hawthome students usually move on throughout Witchdom and most people are welcome to study at Hawthorne and learn. On the other hand, you have Occultism, the magic of Hespatia, that is far less common as people don't usually live to betray Hespatia and they aren't exactly welcoming to sit-ins.

    If a Factional magic has a Rank 0 effect, then it is universally available to all witches all the same, but the evolution on that rank 0 effect requires the specialized knowledge known to the given faction.

    The two faction “magics” of Gadgetry and Integration are NOT magic at all, but nonetheless require Power to invest in as a measure of your fate and opportunities.
    """
