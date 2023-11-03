module Data.Relic exposing (Content(..), Details, all, intro)

import Generated.Types exposing (Class(..), Relic(..))


type alias Details =
    { name : Relic
    , class : Class
    , content : Content
    }


type Content
    = Single Int String
    | WithChoices String (List Int)


intro : String
intro =
    """
    # Relics

    {center} Relics are like perks, but are external boons in the form of magical artifacts, as a rest they aren't things inherent to yourself, but are things you can acquire over time.

    {center} "Let's see if we can detect any Relics in your future, they sometimes show up in these tests..."

    {center} {choice *Relics cost REWARD points obtained from Quests, as shown, however you can buy them with POWER instead, if you so choose.*}
    """


all : List Details
all =
    [ hexVPN, stormBrew, nightlight, stainedSliver, jadeBolt, goldenFish, necronomicon, alchemistStone, yagaRoot, nymphVessel, longingMirror, hellrider, archersBow, assassinsEdge, wardensMaul, devilsTrident, guardiansWall ]


hexVPN : Details
hexVPN =
    { name = HexVPN
    , class = Academic
    , content = Single 2 """
        The ultimate in secure internet access. HexVPN routes your internet traffic through a literal blackbox. The ominous hovering black cube with fractal engravings completely masks your connection in ways mortal VPNs can't; Your traffic won't even be seen or register as a request, and it allows you to access any public website that would otherwise have a paywall. No more sharing netflix accounts, and no region locks, rocking 666 Tbps up and down.
        """
    }


stormBrew : Details
stormBrew =
    { name = StormBrew
    , class = Warlock
    , content = Single 4 """
        This large glass jar can be used similar to a cauldron, for brewing storms rather than potions. Using similar rank ingredients.

        *R1*: Clear day. Light Drizzle.
        *R2*: Light mist, standard rain
        *R3*: Light snow or hail, decent rainshower.
        *R4*: Basic Thunderstorm. Heavy rain Thick snow. Basic hailstorm.
        *R5*: Heavy thunderstorm, torrential rain, golfball hailstorm, Blizzard. Heavy winds.

        Weather is influenced within a 30 mile radius. for up to 6 hours/use.
        """
    }


nightlight : Details
nightlight =
    { name = Nightlight
    , class = Warlock
    , content = Single 6 """
        This black candle actively produces darkness. It repels light, and physically produces an inky black haze. Those who close their eyes within the antilight of this candle will open them to find themselves in the Darkened World, the _World of Terror_ as shown in the Cosmology. If you lack portals, you may be trapped. Why? Well, the lit candle reduces the component cost of Alchemy and other magics by 3 ranks worth, even reducing to 0 cost.
        """
    }


stainedSliver : Details
stainedSliver =
    { name = StainedSliver
    , class = Warlock
    , content = Single 6 """
        This ruby red obsidian dagger with a thin hollow injector in its core is an ancient sacrificial dagger. It has two purposes. A) Itcan store a potion within it to apply its effects on a stab, getting 3 extra uses out of one potion. B) When empty and used in a ritual sacrifice, it syringes up blood, and with it the soul, refined and primed to be used later on demand to fuel an effect requiring it. While it has a soul, wounds it causes refuse to heal unless holding the dagger.
        """
    }


jadeBolt : Details
jadeBolt =
    { name = JadeBolt
    , class = Warlock
    , content = Single 6 """
        This heavy gold ‘handgun’ projects green quartz shards with equivalent force of a 45 Long Colt, growing its own ammo. You can direct a curse at the jade and emerald skull to imbue its shots with the curse effects until you replace the curse with another. Injuries caused by this weapon do not heal or benefit from healing effects for 1 week, during which time any means of cheating death are put on pause so long as their body is pierced with the quartz shards.
        """
    }


goldenFish : Details
goldenFish =
    { name = GoldenFish
    , class = Warlock
    , content = Single 4 """
        These rare creatures are to goldfish what witches are to humans. Hard to come by, these fish have peculiar spirits that pack a spiritual punch on the level of a human soul

        You can use one of these fishes in place of a human for the use of any blood rituals.

        You have 3 of these fishes, 2f, 1m. They require monthly water changes, swimming in a mix of an R4 Stimulant and Curative.

        They reproduce once a decade, and live indefinitely.
        """
    }


necronomicon : Details
necronomicon =
    { name = Necronomicon
    , class = Warlock
    , content = Single 4 """
        The book of the woeful dead. Using this, you can combine the disciplines of Necromancy and Consortation. Demons you summon can be bound like spirits into Undead bodies, or a Golem (Hexes). Their personality remains the same, but they're now permanent and don't count to limits. The body is itself a form of payment for their service but with a body demons like the foliot wouldn't be invisible, not being a spirit now. A Balor would be a a murder machine with thousands of years of combat training, but still limited to the capabilities of the given body. No effect on R5.
        """
    }


alchemistStone : Details
alchemistStone =
    { name = AlchemistStone
    , class = Academic
    , content = Single 6 """
        A ruby gem that can bond with your rod (Witchery), to augment it with a transmutation catalyst that acts as a reusable medium for using Hexes. Begins as a Rank 1 medium, you can upgrade it to let it work as a higher rank medium by letting it absorb 1 ton of materials of the higher rank. Itcan also store any amount of Kisses to use for magic, such as Alchemy, and Runes.

        You can feed the stone magic items to extract the Kiss value of the item, directly into kisses stored within the stone. Itcan also transmute Kisses into other Kisses of different values. Kisses can of course be extracted when desired.
        """
    }


yagaRoot : Details
yagaRoot =
    { name = YagaRoot
    , class = Warlock
    , content = Single 2 """
        A wooden carving of a gnarled spider. When placed under the floorboards, or buried next to the foundations, of a small home, it animates to grow hard wooden tendrils throughout the structure of the building and throughout its foundations. With meditation, you can deliver it commands that allow it to rise on eight spidery root-egs to travel where you wish it. It's surprisingly nimble and can use its legs to anchor itself in trees, and create basic wood furniture or stairs on demand. If you have R5 Witchery, it can burrow into the earth to disappear into your pocket space, and emerge anywhere within 10 miles or to property you own.
        """
    }


nymphVessel : Details
nymphVessel =
    { name = NymphVessel
    , class = Warlock
    , content = Single 4 """
        This ornate pitcher holds an endless reserve of water from the Faegarden and the mythic baths of the nymphs. Pouting from the left pours cold clear waters.

        You'd be hard pressed to find purer water, with the perfect mineral content for sustaining life. Pouring from the right pours hot milky white waters saturated with minerals... and some other stuff. This water is extremely revitalizing, relieving pain on contact and releasing tension and stresses while sanitizing and washing all impurities. Leaves you with pristine soft skin. Drinking it can cure infection and replace a meal.
        """
    }


longingMirror : Details
longingMirror =
    { name = LongingMirror
    , class = Warlock
    , content = Single 4 """
        This arcane mirror can alter its size from a pocket mirror, up to a full body standing mirror. You can use this as a scrying tool to operate a spirit eye as though it were a silent invisible camera drone without range or battery limit. Witches and mediums can see the eye if they spot it, being the size of a literal eye. Flies as fast as you can jog or the max speed of your broomstick if you have it improved with witchery.

        You can swipe your hand across the mirror to make a copy of the mirror manifest (also as a spirit construct) where the eye is, and allow two-way communication through it, if the other person can perceive it
        """
    }


hellrider : Details
hellrider =
    { name = Hellrider
    , class = Warlock
    , content = Single 6 """
        This infernal machine is a construct of hell. Its chitinous wrought iron frame is unnaturally durable and heavy yet its ability to travel is subjective, relative to its own point of reference. You could drive up the side of a building or on a ceiling, and on the surface of water. Travels at up to 260mph. You can summon or unsummon it with basic unranked witchery, or you can bond it with your Rod to act as an alternate form, doing so adds your broomstick's speed to the hellride's top speed, and it gains the ability to fly. And likewise, is like new again on each summon.

        It has a builtin flamethrower, and can leave a trail of fire between 10ft - 6 inches tall for 10 seconds.
        """
    }


archersBow : Details
archersBow =
    { name = ArcherSBow
    , class = Warlock
    , content = Single 12 """
        This bow is no more than a central grip that when triggered projects crystal arms and string. When drawn, it manifests its own crystal arrows. Each arrow strikes with the speed and force of a .50 caliber rifle. You can infuse any given shot with added mana equivalent to a rank 3 spell to cause it to explode within 3m on hit or proximity to a given target mentally established, or to cause the next projectile to seek with the agility of a hummingbird

        By infusing the projectile with mana equivalent to a rank 5 spell, you can cause the arrow to split into two. Which then continue to double for every 200m of distance traveled. Stacks with r3 infusion.
        """
    }


assassinsEdge : Details
assassinsEdge =
    { name = AssassinSEdge
    , class = Warlock
    , content = Single 12 """
        This ruby-like dagger holds a malicious edge that leaves aggravated wounds that actively draw blood out from the injury, bleeding 10x as much as it should. By infusing it with r3 tier mana, you can cause the next injury to violently expel all blood within a wounded target, leaving a desiccated husk. By infusing it with r5 tier mana, you can create 12 rapiers with the same ruby appearance and aggravated bloodloss. Each rapier a _dancing blade_, self-animated in synch with your own intentions as though wielded by a master swordsman. When moving in a straight line, they can move with the speed of an arrow up to 60m from you.
        """
    }


wardensMaul : Details
wardensMaul =
    { name = WardenSMaul
    , class = Warlock
    , content = Single 12 """
        This gilded hammer of comical proportions should be impossible to wield. Most humans can't even lift the head alone. But when activated in the hands of a bonded witch, it generates its own lift field with Levitation magic, and when swung it actively propels itself in short bursts. It weighs 950Ibs, but strikes as though it weighs 2 tons. By infusing it with r3 magic, the hammer can generate this lift field propulsion effect on contact, making a struck target weightless and then propelled up and away with great force. Infusing it with r5 magic, you can make yourself and the hammer impervious to harm for 3 minutes, though you're -20% slow.
        """
    }


devilsTrident : Details
devilsTrident =
    { name = DevilSTrident
    , class = Warlock
    , content = Single 12 """
        This black star-iron trident glows. with heat on demand. Any contact at all with the head of the trident causes 100 times the sensation of pain you would expect. Brushing against it would feel painful, even when not searing hot. The central point hovers in place as firm as though a part of the whole, but it can at your option detach to remain fixed inside an injury, with barbs shooting out of its sides. Infusing it with r3 magic lets you cause a struck target to subjectively experience time 5x slower. With r5, you can make them subjectively experience time 100x slower or make yourself perceive time 2x slower without injury.
        """
    }


guardiansWall : Details
guardiansWall =
    { name = GuardianSWall
    , class = Sorceress
    , content = Single 12 """
        This adamantine tower shield can adjust its size from a dinner plate to up to twice your own height and up to 3x your own width with a mere second of shifting. As adamantine, it's the only substance a riftblade can't cut, impervious to harm. This shield has the added benefit of special anchors that allow it to resist knockback and other forces, though its resistance wears out with overuse. With an infusion of mana = to the rank or otherwise as appropriate, the shield can absorb any magic effect, then replicate it itself using twice its normal mana cost. The shield can store 4 effects at once. Has 40 absorb charges per day.
        """
    }
