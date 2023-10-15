module Data.Perk exposing (Content(..), Details, all, intro)

import Generated.Types exposing (Affinity(..), Class(..), Perk(..))


type alias Details =
    { name : Perk
    , class : Class
    , affinity : Affinity
    , content : Content
    }


type Content
    = Single Int String
    | WithChoices String (List ( String, Int )) String


intro : String
intro =
    """
    Perks are an assortment of bonus traits ranging from added benefits of your new witch true form, to special magical abilities you learn or manifest in your transformation that differ from anything already shown. {choice *Perks cost POWER as shown in the corner. Like Magic, Perks have half the cost if you have the Affinity shown in the other corner. [???] is universally halved (Not pre-factored).*}
    """


all : List Details
all =
    [ oracle ]


oracle : Details
oracle =
    { name = Oracle
    , class = Warlock
    , affinity = Mind
    , content = Single 4 """
        Your bad omens feel a little more specific, giving you clearer impressions on the type of events to unfold and a vague intuition on steps you could take to resist that omen, even as simple as “I should walk to the library today” without knowing why, that will help contribute in a cascade of events to undermine the bad omen.

        With Ministration, you can consult your celestial summons for cryptic insights to further help. Using a crystal ball or prayer, you can use foresight with more detail. Less distortion, and can focus on a sense other than sight and from a perspective of someone touched who will observe that future event
        """
    }
