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
    [ oracle, jackOfAll, transformationSequence, poisoner, witchflame, energized, conjuration, elephantTrunk, prestidigitation, suggestion, fascinate, pantomime ]


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


transformationSequence : Details
transformationSequence =
    { name = TransformationSequence
    , class = Academic
    , affinity = All
    , content =
        WithChoices """
        You can keep your current human body or redesign it separately, and you have the ability to do a 1-10 second long transformation sequence that swaps between your human and witch body. As a human you can still see through the Veil like a witch but you can only use Perks with a base cost under 6, Relics, and Rank 1 magic until you transform into your witch form. Your human form _can_ be the opposite sex to your witch form. This can also equip Mothergifts.
        """
            [ ( "This is the basic version, and costs 6 power", 6 )
            , ( "For an additional 6 Power, you can instead completely change your Witch Type's race with your transformation, going between _two witch forms_.", 12 )
            , ( "For 12 more power, each can have separate magic and perk choices.", 18 )
            ]
            ""
    }


poisoner : Details
poisoner =
    { name = Poisoner
    , class = Warlock
    , affinity = Nature
    , content = Single 4 """
        Control over poison. You can command any highly venomous animal, easily nurture and grow poisonous plants and mushrooms, and telekinetically control poisonous materials slowly without significant force.

        More notably, is that this power grants complete immunity to diseases and toxins, including bioweapons like mustard gas. The control aspects are a bit tiring, but the immunity is passive. This doesn't allow you to cure or diminish the afflictions of others.
        """
    }


witchflame : Details
witchflame =
    { name = Witchflame
    , class = Sorceress
    , affinity = Fire
    , content = Single 2 """
        Witchfire is variably colored flame that has slightly different behavior from witch to witch, and when you place a witchfire somewhere, it stays there until dispelled even without any fuel to maintain it. Stick a mote of flame in the air and it will stay put. Great for home lighting, and you can adjust the temperature to 20 below freezing, up to 500 degrees, though the size is never larger than a campfire, and doesn't spread like normal fire, though it can chill, heat, or burn material it contacts. Wood would blacken but not ignite, for example. Great for cooking, potions, and rituals. Mild combat applications, it's mainly utility.
        """
    }


energized : Details
energized =
    { name = Energized
    , class = Sorceress
    , affinity = Wind
    , content = Single 4 """
        You passively emanate an electric aura that fills the air with a harmless sense of static and slight ozone smell. This passively charges any device within 10m of you, and you can focus to spike it to damage electronics within 30m or deliver a tazer-like zap to point you can see within 60m. Your touch slightly tingles, and with focus you can amplify this sensation. During a storm, you can focus your will on a target location to ‘cause a lightning strike with a rough accuracy of a 6ft sphere, it can be skewed by lightning rods.
        """
    }


conjuration : Details
conjuration =
    { name = Conjuration
    , class = Academic
    , affinity = All
    , content = Single 6 """
        _Requires Consortation 1+._

        You acquire the ability to summon the infernal shopkeeper, Mammon, whose genie-like visage appears as a proxy through which you can order any product that is available for purchase anywhere else on Earth or in Witchdom. He will set an asking price 7x what it would normally be worth (-1x per rank in Consortation). So long as you know that something is sold for human currency somewhere, you can buy it. He accepts cash or barter for objects of monetary value, or Kisses at K1 = $100.
        """
    }


elephantTrunk : Details
elephantTrunk =
    { name = ElephantTrunk
    , class = Warlock
    , affinity = Body
    , content = Single 4 """
        "Whoa, a male witch? Maybe we should check, just to be sure."

        You can get back to me on that one. This perk allows for a male witch. Male humans are just as likely to be a pre-awakened witch as any female, they just usually, well, don't stay that way.

        If you don't want to be male but just want the twig 'n berries for some fun, _Hexes_ can help with that so can _Alchemy_, or _Collection_ with a hex spell rune trinket.
        """
    }


prestidigitation : Details
prestidigitation =
    { name = Prestidigitation
    , class = Academic
    , affinity = All
    , content = Single 2 """
        A universalist bag of magic tricks, the magician's toolkit.

        You can heat or chill things generally recognized as food or drink to a reasonable temperature for that object, and flavor it or add a scent as you wish it.

        You can fabricate small objects that fit within your hand and weigh less than 1lb, with very rough craftsmanship unless you have an object nearby to copy, and it turns to dust after 1 minute or when dismissed.

        Create small 2D illusions on a surface while you maintain focus.

        Clean or soil roughly 1 cubic foot of area in a wave of the hand.
        """
    }


suggestion : Details
suggestion =
    { name = Suggestion
    , class = Warlock
    , affinity = Mind
    , content = Single 6 """
        There's something enchanting about your eyes. With 4 seconds of eye contact with another, you can issue a command with a shimmer in your eyes. People will resist commands that will do them any great harm unless they already wanted to do that thing, while commands that are merely risky will be a little difficult and maybe take repeated application as their confidence wavers. You can't command people to do unnatural things, like forget something, so if you make them do something too weird they can make the connection that you made them do it. It's a nagging suggestion and intrusive thought, but not mind control.
        """
    }


fascinate : Details
fascinate =
    { name = Fascinate
    , class = Warlock
    , affinity = Mind
    , content = Single 4 """
        Similar to suggestion but with a different outlet, you can ensnare the attention of others who can see or hear a performance of yours that has your main focus. Singing, dancing, playing an instrument, or acrobatic feats, fooling around. Whatever it is, that could be described as a bardic art. Those who hear you seek out a line of sight within reason, otherwise remain fixated looking in your direction. They have no desire to look away and give mild resistance to attempts to divert their attention elsewhere. Boosts skill in performance arts. You're able to weave Curses into your performance seamlessly.
        """
    }


pantomime : Details
pantomime =
    { name = Pantomime
    , class = Sorceress
    , affinity = Soul
    , content = Single 4 """
        A fun gift that inspired the folk performances of the mime. You have the ability to physically interact with spirit beings and constructs, while manifesting spirit constructs from your will Objects are mundane everyday items, if they weren't spirit objects. Nothing overly complex such as a watch or electronics, but you can manifest something like a chair, or ladder. The most complicated thing you can summon is a bicycle. If you need glasses, you can actually manifest spirit glasses. Spirit objects cannot interact with living things that don't have this ability. But a ghost chair would rest on the ground normally, without leaving a depression.
        """
    }
