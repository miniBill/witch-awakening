module Data.Perk exposing (Content(..), Details, all, intro)

import Generated.Types exposing (Affinity(..), Class(..), Perk(..))


type alias Details =
    { name : Perk
    , class : Class
    , affinity : Affinity
    , isMeta : Bool
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
    [ oracle, jackOfAll, transformationSequence, poisoner, witchflame, energized, conjuration, elephantTrunk, prestidigitation, suggestion, fascinate, pantomime, beautySleep, thirdEye, soulJellies, hatTrick, moodWeather, improvedFamiliar, hybridize, apex, chargeSwap, crystallize, memorize, maidHand, hotSwap, menagerie, bloodWitch, gunwitch, levitation, isekaid, heritage, magicFriendship, windsong, broomBeast, isekaiWorlds, isekaiHeritage ]


oracle : Details
oracle =
    { name = Oracle
    , class = Warlock
    , affinity = Mind
    , isMeta = False
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
    , isMeta = False
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
    , isMeta = False
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
    , isMeta = False
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
    , isMeta = False
    , content = Single 2 """
        Witchfire is variably colored flame that has slightly different behavior from witch to witch, and when you place a witchfire somewhere, it stays there until dispelled even without any fuel to maintain it. Stick a mote of flame in the air and it will stay put. Great for home lighting, and you can adjust the temperature to 20 below freezing, up to 500 degrees, though the size is never larger than a campfire, and doesn't spread like normal fire, though it can chill, heat, or burn material it contacts. Wood would blacken but not ignite, for example. Great for cooking, potions, and rituals. Mild combat applications, it's mainly utility.
        """
    }


energized : Details
energized =
    { name = Energized
    , class = Sorceress
    , affinity = Wind
    , isMeta = False
    , content = Single 4 """
        You passively emanate an electric aura that fills the air with a harmless sense of static and slight ozone smell. This passively charges any device within 10m of you, and you can focus to spike it to damage electronics within 30m or deliver a tazer-like zap to point you can see within 60m. Your touch slightly tingles, and with focus you can amplify this sensation. During a storm, you can focus your will on a target location to ‘cause a lightning strike with a rough accuracy of a 6ft sphere, it can be skewed by lightning rods.
        """
    }


conjuration : Details
conjuration =
    { name = Conjuration
    , class = Academic
    , affinity = All
    , isMeta = False
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
    , isMeta = False
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
    , isMeta = False
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
    , isMeta = False
    , content = Single 6 """
        There's something enchanting about your eyes. With 4 seconds of eye contact with another, you can issue a command with a shimmer in your eyes. People will resist commands that will do them any great harm unless they already wanted to do that thing, while commands that are merely risky will be a little difficult and maybe take repeated application as their confidence wavers. You can't command people to do unnatural things, like forget something, so if you make them do something too weird they can make the connection that you made them do it. It's a nagging suggestion and intrusive thought, but not mind control.
        """
    }


fascinate : Details
fascinate =
    { name = Fascinate
    , class = Warlock
    , affinity = Mind
    , isMeta = False
    , content = Single 4 """
        Similar to suggestion but with a different outlet, you can ensnare the attention of others who can see or hear a performance of yours that has your main focus. Singing, dancing, playing an instrument, or acrobatic feats, fooling around. Whatever it is, that could be described as a bardic art. Those who hear you seek out a line of sight within reason, otherwise remain fixated looking in your direction. They have no desire to look away and give mild resistance to attempts to divert their attention elsewhere. Boosts skill in performance arts. You're able to weave Curses into your performance seamlessly.
        """
    }


pantomime : Details
pantomime =
    { name = Pantomime
    , class = Sorceress
    , affinity = Soul
    , isMeta = False
    , content = Single 4 """
        A fun gift that inspired the folk performances of the mime. You have the ability to physically interact with spirit beings and constructs, while manifesting spirit constructs from your will Objects are mundane everyday items, if they weren't spirit objects. Nothing overly complex such as a watch or electronics, but you can manifest something like a chair, or ladder. The most complicated thing you can summon is a bicycle. If you need glasses, you can actually manifest spirit glasses. Spirit objects cannot interact with living things that don't have this ability. But a ghost chair would rest on the ground normally, without leaving a depression.
        """
    }


beautySleep : Details
beautySleep =
    { name = BeautySleep
    , class = Sorceress
    , affinity = Body
    , isMeta = False
    , content = Single 4 """
        Witches with this perk no longer need to sleep. They fatigue normally from exertion but normal rest alleviates it

        They can still choose to sleep if they wish, always resulting in lucid dreams unless they just want to skip through the night. It has the added benefit of letting them sleep on demand, waking up exactly when they wanted, with full consciousness.

        By this same token, they're immune to chemical or magical attempts to make them sleep.
        """
    }


thirdEye : Details
thirdEye =
    { name = ThirdEye
    , class = Academic
    , affinity = Soul
    , isMeta = False
    , content = Single 4 """
        Witches normally have an unseen third eye, it's how we see spirits. Some achieve a second awakening of this eye, allowing them to see auras possessed by all lifeforms and spirits. Every witch has a unique aura, but an aura will retain some constants regardless of shape or mood that can be used as a more secure fingerprint for ID left by magic use in an area, or on their person. Seeing an aura can also accurately read the emotional state of the creature.

        _Note: rare tricks can mask an aura._
        """
    }


soulJellies : Details
soulJellies =
    { name = SoulJellies
    , class = Warlock
    , affinity = Necro
    , isMeta = False
    , content = Single 4 """
        Some witches have this power to manifest these creatures made of jelly-like spirit matter. When they manifest seems random but associated with sudden emotion spikes. These curious helpful creatures are nonverbal but as intelligent as a familiar. They can exert 5lbs of force on the material world. A soul slime can be used as the spirit bound in Necromancy, and similarly a soul can be bound as a soul slime. You can maintain an average of 3, randomly replacing a loss within 3 days.
        """
    }


hatTrick : Details
hatTrick =
    { name = HatTrick
    , class = Academic
    , affinity = All
    , isMeta = False
    , content = Single 4 """
        Greatly enhances the mothergift of Witchery, allowing telekinetic control over the Garment, Hat, and Rod irrespective of difference or line of sight.

        You can create duplicates of your Hat you can jump into, to appear out of any other. Up to 6 dupes. If you have Portals and R5 Witchery, your garment or hat can connect to any portkey, gateway, or rift, and the hat can open up as a rift instantaneously.
        """
    }


moodWeather : Details
moodWeather =
    { name = MoodWeather
    , class = Sorceress
    , affinity = Nature
    , isMeta = False
    , content = Single 4 """
        This rare gift provides a level of subconscious control over the weather based on your mood, and based on your impressions. If you think rain is sad, it'd rain when you feel sad. If you think rain is pleasant, then it might rain when you're in a pleasant mood. Thunderstorms almost always occur with anger, either way. If you feel like going out, it’d either be sunny or overcast on preference. Humans aren't too happy with these kinds of witches and they have a close eye on weather events. A warden charm can suppress the effects.
        """
    }


improvedFamiliar : Details
improvedFamiliar =
    { name = ImprovedFamiliar
    , class = Warlock
    , affinity = Beast
    , isMeta = False
    , content = Single 6 """
        Whatever your familiar’s base form, your familiar gains the ability to shapechange into an enhanced form from the following:

        - *Dire X*: A Dire Animal. Larger than normal with bone growths providing armor plate or scales, and horns, spikes, & razor claws.

        - *Chimeric*: An animal that is a combination of multiple animals. Examples: Griffin, Hippogriff, Pegasus, Manticore, Chimera, ect. Whatever your expectations of a beast, it’s balanced to the other options named here. So no outright dragons or wishgranters."""
    }


hybridize : Details
hybridize =
    { name = Hybridize
    , class = Sorceress
    , affinity = Beast
    , isMeta = False
    , content = Single 6 """
        Choose a second racial type for your true form, and reasonably combine the physical look of both races, Take the higher Mana capacity, and maintain both methods of charge (each of which charge as fast as they normally would) and any features mentioned in their description, such as the breath of the Draviri or the honey of the Sprite. You can take both type perks of either race. You do not gain affinity of the second type. See _Cosmic Pearl_. You can take this twice for up to 3 aspects. With 2 you're a _hybrid_, with 3 you're a _chimera_.
        """
    }


apex : Details
apex =
    { name = Apex
    , class = Sorceress
    , affinity = Body
    , isMeta = False
    , content = Single 12 """
        You are a supreme example of your species of witch, a paragon among your peers and a prime specimen. Every notable quality about your witch's race is exaggerated above normal, and numerical aspects are tripled. Your height and mass may or may not be increased by 50% at your discretion. If your race didn't already increase your physique, you are more physically attractive and have peak human prowess. This also applies to type perks of your race, and applies to all parts of a _Hybridize_. but cost +3p/each.
        """
    }


chargeSwap : Details
chargeSwap =
    { name = ChargeSwap
    , class = Warlock
    , affinity = Soul
    , isMeta = False
    , content =
        WithChoices """
            Replace your Charge method with the method of another race, unless the chosen charge type would not be possible without an integral aspect of that race. ie; Can't take the Aurai charge type if you don't have the Aurai paradox voice, or a Gorgon's charge without their petrify. This changes your charge rate to match, as it's inherent to the method, not the type.
            """
            [ ( "This is the basic version and costs 4 power", 4 )
            , ( "_For an extra 6 power_, you instead _gain_ the desired charge method as an _additional_ charge method. They can both be providing mana gain at the same time, at their individual rates", 10 )
            ]
            ""
    }


crystallize : Details
crystallize =
    { name = Crystallize
    , class = Sorceress
    , affinity = Earth
    , isMeta = False
    , content = Single 6 """
        Internalize a spark of elemental Earth that allows you to change your form into living crystal like a diamond. Appearance is up to you, such as ruby or sapphire. You are invulnerable to physical harm while diamond, but this has heavy mana drain. With Low Mana stores you'd last 60 seconds before empty. With High, you might last 10 minutes. You can partially crystallize parts of your body like just a hand, for 1/10th the cost, or you can passively maintain a lesser surface crystallization that is as protective as half-inch thick steel plating, With a minor initial mana cost but Ino passive drain. You could also project crystal shards equivalent to a 9mm firearm
        """
    }


memorize : Details
memorize =
    { name = Memorize
    , class = Sorceress
    , affinity = Body
    , isMeta = False
    , content = Single 6 """
        We're witches. We aren't that physical, generally barring some races' physical advantages. Magic helps for sure, but other than some shenanigans with guns due to them bordering on alchemy, a martial witch will mostly be on their own in learning.

        .,,Unless you were to employ some trickery. With this gift you can memetically synchronize with any martial maneuver or technique you see performed, and repeat it yourself. Forevermore, forming an archive over time with your exposure.

        Think Taskmaster from those Marvel books a lot of humans love. Perfect execution every time, unless interrupted by something.
        """
    }


maidHand : Details
maidHand =
    { name = MaidHand
    , class = Academic
    , affinity = Mind
    , isMeta = False
    , content = Single 4 """
        Create psychological constructs, manifest as unseen telekinetic masses. Each construct acts as an _unseen servant_ capable of autonomously handling basic acts of service compared to a minimally competent maid or butler that's responsive to commands and orders that it will continue to act out until you stop it, tell it to do something else, or it meets a requirement such as a time or condition. You are always aware of where your constructs. are despite being invisible even to spirit sight, but you can provide them with clothes that they can fill out as though they had a human shape. Maximum number of 3 + ranks in Hexes and Psychotics.
        """
    }


hotSwap : Details
hotSwap =
    { name = HotSwap
    , class = Academic
    , affinity = Metal
    , isMeta = False
    , content = Single 6 """
        Your witchery is capable of adapting itself to any weapon (rod) and armor or helmet (garment and hat) that you have stored in your pocketspace. when you call your mothergifts, you can call any armor/clothing, or weapon you have within it. It is treated as being your gift, but you can't alter its appearance. In addition, whenever you unsummon your mothergift you can immediately resummon it in the same instance, dynamically replacing unsummoned parts with a new summon as you change from one outfit/armor to another. The equipment you possess can still be broken or damaged as normal and would require normal repairs unlike your actual mothergifts.
        """
    }


menagerie : Details
menagerie =
    { name = Menagerie
    , class = Warlock
    , affinity = Beast
    , isMeta = False
    , content = Single 6 """
        _Requires Witchery 5._

        In your pocketspace you gain 75 golden camels, 53 purple peacocks, 95 persian monkeys, 429 homunculi (synthetic humans) slaves, servants, and flunkies. 60 elephants, 231 llamas galore, 9 bears and lions. 40 holy sages (Real human spirits bound to your pocketspace that dedictated their lives to witches past) as scholars and philosophers though their information is a few millenia out of date. 1,000 birds of exotic varieties. You house them all in an elaborate palace complex the size of a small city that appears in your pocketspace, in a design of your choosing. Objects and wealth are false matter, but the creatures are real and can survive on false matter creations.
        """
    }


bloodWitch : Details
bloodWitch =
    { name = BloodWitch
    , class = Warlock
    , affinity = Blood
    , isMeta = False
    , content = Single 6 """
        Control blood to the extent that droplets can function like bullets, or form lances with ballista-like force, or to puppet the blood inside living or dead creatures to control them like marionettes, unless protected by some means like a warding rune. Control within 160m, not counting projectile range.

        You can also use blood to replace the mana cost of any magic at a rate of 1 drop for rank 1, 10 for rank 2, a vial for 3, a cup for 4, and 6 for 5, and use ritual sacrifice to replace any material requirements. The life of a noninnocent adult is can fuel up to rank 3 magic. Children down to the unborn and innocent adults can fuel rank 4 magic. Innocent virgin humans between 14 and 24 can fuel rank 5 magic.
        """
    }


gunwitch : Details
gunwitch =
    { name = Gunwitch
    , class = Sorceress
    , affinity = Metal
    , isMeta = False
    , content = Single 4 """
        Originated by a male witch, _John Moses Browning_, you can ritually bond with a single firearm to summon it as you would your Rod. If you have _Hot Swap_, then gunwitch applies to any firearm called with it. You can use large rifles as though it were your broomstick and you have a sixth sense over your bonded or swapped firearms which allows you to have a full detailed perception of the orientation of its barrel and the trajectory of their bullets, as well as the trajectory of any projectile that has been fired, or a moving changing trajectory of projectiles that will fire within 2 seconds with increasing intensity as it becomes more certain. So long as you have 1 bullet, you can duplicate it with a tiny mana cost.
        """
    }


levitation : Details
levitation =
    { name = Levitation
    , class = Academic
    , affinity = All
    , isMeta = False
    , content = Single 6 """
        Your relationship with space is pretty relative. You can move yourself omnidirectionaly through space as though moving reality relative to you rather than moving your body itself. You'd appear to levitate as you wish and never need to touch the ground again You can disconnect any object you touch to be severed from the effects of gravity and it would remain as you last left it unless acted upon by another force, though acting as though it had notable friction to slow itself or resist wind. You "fly" at up to 25mph, as casual to you as a light jog. If you have Witchery, you add your potential speed if you were to use your broomstick, though you need not actually use it to gain speed.
        """
    }


isekaid : Details
isekaid =
    { name = Isekaid
    , class = Warlock
    , affinity = Life
    , isMeta = False
    , content = Single 4 """
        When you awaken you'll be reborn in my world. Your Earth will exist in its own universe with its own cosmology as you believe it to exist and you can return if you learn Portals 5, though the only magic you can use there is making a portal to return to us. I'm just a visitor here and this is conversation is a dream. _(Unless you don't take this perk)_ When you return, you keep your last appearance taken tweaked as close to mundane as possible. You can choose how you're isekai'd. You hijack a false life retroactively created for this purpose with it's own history, even relations with companions. You can choose to keep or lose memories of your old life or gain them at some point.
        """
    }


heritage : Details
heritage =
    { name = Heritage
    , class = Sorceress
    , affinity = All
    , isMeta = False
    , content = Single 4 """
Choose a relative. They are a witch as well, either not yet awakened or awakened long ago and have been waiting to see your awakening. If you have Isekai'd, it can be a late relative who was isekai'd themselves, or isekai any relative now or later. Sharing your lineage, they have a strong magic tier. Choose up to 50 power for them to have and design a build for them. You can also invest your own Power on their behalf. You can take them as a Companion for 4p undiscounted unless you invested 4+ already. This is redundant if you cooperate with a relative (or friend) to make independent coexisting builds.

Alternatively, or additionally, If you have Isekai'd, you and another player can choose to be made related and both gain 4 Power.

"""
    }


magicFriendship : Details
magicFriendship =
    { name = MagicFriendship
    , class = Sorceress
    , affinity = Life
    , isMeta = False
    , content = Single 6 """
Choose one companion you have (or choose later). You'll form a special connection either super platonically with just a bit of undertones and fuzzies, or outright romantically. Either way, you'll both be able to feel the emotional state of the other and if they are in danger even if they don't know it themselves. You gain a new High charge rate by being intimate with one another, including just a warm hug. Stacks with other applicable charge methods. Both of you are incapable of thinking negative thoughts about the other. So fluffy it burns. Once per year you can work together to make a heart shape, and produce a beam of intense [Life] energy that will cure any ailment, or delete all non-deity enemies in a narrow line.
"""
    }


windsong : Details
windsong =
    { name = Windsong
    , class = Sorceress
    , affinity = Wind
    , isMeta = False
    , content = Single 15 """
        You gain the blessing of a wind spirit, or the wind spirit unlocks an aspect of your own wind affinity. Wind always seems to be in your favor with minor conveniences like campfire smoke not blowing in your face, and dramatically blowing your hair or cloaks. But more importantly, you can initiate the windsong to gain a swirling breeze around you and a sense for all motion within wind within 60m acting like a 360 tremorsense through the air. Whenever you dodge an attack within 1m of yourself you siphon off kinetic energy to enhance your own kinetic energy, adding the force of the attack to your next movement or attack within 6 seconds. You can also “Step” on the air up to 4 times in a row.
        """
    }


broomBeast : Details
broomBeast =
    { name = BroomBeast
    , class = Academic
    , affinity = All
    , isMeta = False
    , content =
        WithChoices """
            Your mastery over a broomstick is especially noteworthy. Your broom is like a proper extension of your will. For 1p, Whether or not you have ranks in Witchery, you can summon your Rod in the form of a Broomstick near instantaneously to your hand and you can telekinetically command its flight remotely. For another 2p, it has the speed of an arrow, up to 250 feet per second. This stacks with broom speed from other sources. You can maneuver it with similar agility to a sportsbike, so do watch out for sharp turns.

            For every additional Power spent, you gain an additional 50fps speed and improve the maneuverability by 50% until it's as agile as a hummingbird at 4 extra power spent.
            """
            (List.map (\i -> ( "-", i )) [ 1, 3, 5, 7, 9 ])
            ""
    }


isekaiWorlds : Details
isekaiWorlds =
    { name = IsekaiWorlds
    , class = Sorceress
    , affinity = Life
    , isMeta = False
    , content =
        WithChoices """
        Isekai yourself with all the benefits of this cyoa into any other setting of your choice. If that world has its own special abilities systems, you may learn from them if possible independently of this cyoa. You may require Isekai Heritage to benefit from or obtain special abilities only available to certain bloodlines, or simply random chance making it unlikely for most people to benefit from it. If it's uncommon, Nobility can let you benefit. If it's rare, then Royalty is required. Since you are being reborn, you don't count as having visited this earth for the purpose of Portals.
        """
            [ ( "This is the basic version, and costs 12 power", 12 )
            , ( "for an extra 2p, you can experience lucid aetheric dreams of this setting of _“Earth (Witch Awakening)”_, sufficient to allow Portal travel back here if you wish.", 14 )
            ]
            ""
    }


isekaiHeritage : Details
isekaiHeritage =
    { name = IsekaiHeritage
    , class = Sorceress
    , affinity = Life
    , isMeta = False
    , content =
        WithChoices """
            Be reborn as a the child of a particular lineage of your choice. You can also orchestrate events such that any number of relatives or friends from your life also get Isekai'd into comparable positions within your new life. They won't be witches without Heritage, but they can still be normal members of a nonhuman race if you wish. If you have the type perk, they can have the type perk as well without cost. Can be born off-Earth"""
            [ ( "4p: *Nobility / Merchant*: You'll be born to a fairly successful family of either noble class, or merchants. Comparable to upper middle class or low high class", 4 )
            , ( "8p: *Royalty / Merchant Lord*: Born to a highly successful royal bloodline or a rockefeller-like head of an international trade company.", 8 )
            ]
            ""
    }
