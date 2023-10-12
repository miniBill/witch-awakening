module Data.Class exposing (Details, all)

import Generated.Types exposing (Class(..))


type alias Details =
    { class : Class
    , content : String
    }


all : List Details
all =
    [ academic, sorceress, warlock ]


academic : Details
academic =
    { class = Academic
    , content = """
        Academics are studious and focus easily on tasks, while training and studying to further their magic. Their thorough approach to magic tends to be slower if you want to have a life outside of studies, but the most rewarding as they comprehend in more depth and their growth is in their own hands, advancing as slow or fast as the time and effort they put in. Academics gain 1 {academic _*Focus*_} for every day in which they averaged 4 hours of study, 2 for 8 hours. You can use focus to buy a Power point for 10 Focus. This cost increases by 10 Focus per purchase. (10, 20, 30, etc)

        __Start with [30] power__. Player academics eventually cap out at [100] _power_ before other factors like complications. Any option marked with the {academic *blue*} icon has a flat cost reduction of -2 Power to its price, which can reduce it to 0, or become a gain of 1 point, or complications give 2 more power. This applies only once per option. Once per Magic specialization, once per perk, etc, and always applies first.

        *Advantage*: General use magic and classics. Academics favor mixed bags like Potions, Runes, and Portals. Any magic marked with blue used by an academic produces twice the yield or is half as time consuming, mana draining, or tiresome to use. Two potions for the price of one, two runes for the price of one, ect. Any duration of a blue marked magic effect applied by you, to you or to another, lasts twice as long For example, double the duration of a temporary potion.

        An Academic can study to master any two schools of magic for free, but takes time to learn equal to if you were saving the power to buy it, but no Power is spent.
        """
    }


sorceress : Details
sorceress =
    { class = Sorceress
    , content = """
        Sorceresses are inherently imbued with magic as natural extensions of their will so they tend to be more in tune with their bodies and grow through tactile training. They're naturals but tend to have less of a tangible understanding of how and why magic works or interesting implications of magical theory. Fireballs go boom, ain't gotta explain sheit. Sorceresses gain 1 {sorceress _*Might*_} for every day in which they averaged 1 hour of straining practice, 2 for 4 hours. You can use Might to buy a Power point for 10 Might. This cost increases by 10 Might per purchase. (10, 20, 30, etc)

        __Start with [30] power__. Player sorceresses eventually cap out at [100] _power_ before other factors like complications. Any option marked with the {sorceress *red*} icon has a flat cost reduction of -2 Power to its price, which can reduce it to 0, or become a gain of 1 point, or complications give 2 more power. This applies only once per option. Once per Magic specialization, once per perk, etc, and always applies first.

        *Advantage*: Inner power and direct combat usage. Sorceresses favor direct magic like Elementalist magic. They are not limited by affinity when buying elementalist magics of affinities they do not have, and one that matches their affinities can be taken for free.

        Sorceresses have stronger and more unique auras that are like beacons to anyone who can detect them. This aura can color any elemental magic the sorceress uses, such as white flames, gold stone, black water, or prismatic wind. This includes Naturalism. If they choose to use colored elementalism, then that magic is 50% more damaging, with 50% larger areas of effect and range.
        """
    }


warlock : Details
warlock =
    { class = Warlock
    , content = """
        Warlocks are endowed with power from some third party. Their power can't be taken back afterward anymore than such an entity might be capable of stealing power from any other witch. Instead of studying, or training, they spend time in service, partnership, employ, or worship to a patron. They grow by gaining with their patron(s), by doing quests, the Warlock gains {warlock _*Favor*_} equal to the Reward value of the quest, Warlocks can trade Favor 1-1 directly for Power due to the scarcity, being dependent on Quests. A Warlock can continue to do quests without a quest slot, but doing so offers no rewards except Favor.

        __Start with [30] power__. Player warlocks eventually cap out at [100] _power_ before other factors like complications. Any option marked with the icon has a flat cost reduction of -2 Power to its price, which can reduce it to 0, or become a gain of 1 point, or complications give 2 more power. This applies only once per option. Once per Magic specialization, once per perk, etc, and always applies first.

        *Advantage*: Darker and external magics, Relic usage. Warlocks favor indirect power like Hexes and Curses. They have a personalized brand they can mark on any relic they own or willing creature. They always know the location of one of their marks and when anyone else touches it, and a stronger sensation like an alarm if any harm comes to it. Branded creatures can be affected by the warlock's magic at any distance and the mark can be the target of things such as scrying even if the warlock doesn't know where it is.

        Warlocks can immediately start with 20 Reward Points to purchase relics that are infused in their own soul, summoned the same way as Mothergifts. (See Witchery)
        """
    }
