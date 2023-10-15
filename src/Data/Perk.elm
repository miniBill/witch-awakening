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
    [ oracle, jackOfAll ]


oracle : Details
oracle =
    { name = Oracle
    , class = Warlock
    , affinity = Mind
    , content = Single 4 """
        _Requires Divination 4+._

        You have a greater grasp on your Augury and Foresight spells

        Your bad omens feel a little more specific, giving you clearer impressions on the type of events to unfold and a vague intuition on steps you could take to resist that omen, even as simple as “I should walk to the library today” without knowing why, that will help contribute in a cascade of events to undermine the bad omen.

        With Ministration, you can consult your celestial summons for cryptic insights to further help. Using a crystal ball or prayer, you can use foresight with more detail. Less distortion, and can focus on a sense other than sight and from a perspective of someone touched who will observe that future event
        """
    }


jackOfAll : Details
jackOfAll =
    { name = JackOfAll
    , class = Academic
    , affinity = Mind
    , content =
        WithChoices """
        "Oh. Looks like I was wrong about the rank 5 magic". You lose the ability to take any magic to rank 5. In return, you gain either
        """
            [ ( "12 Power,", -12 )
            , ( "or any 1 perk with a static cost for free", 2 )
            ]
            """You're also now competent in every mundane skill sufficient to compete on a regional level, though can be outclassed on a national or global level, but this doesn't change your physique, so athletic skills may still be more difficult.
        """
    }