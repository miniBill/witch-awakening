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
    | CosmicPearlContent Int String


intro : String
intro =
    """
    {center} Relics are like perks, but are external boons in the form of magical artifacts, as a rest they aren’t things inherent to yourself, but are things you can acquire over time.

    {center} "Let’s see if we can detect any Relics in your future, they sometimes show up in these tests..."

    {center} {choice *Relics cost REWARD points obtained from Quests, as shown, however you can buy them with POWER instead, if you so choose.*}
    """


all : List Details
all =
    [ hexVPN, stormBrew, nightlight, stainedSliver, jadeBolt, goldenFish, necronomicon, alchemistStone, yagaRoot, nymphVessel, longingMirror, hellrider, archersBow, assassinsEdge, wardensMaul, devilsTrident, guardiansWall, alchemistStash, gemOfRenewal, prosthesis, violetLenses, manaCore, magicTalisman, treasurersMint, companionBrick, heirloom, riftblade, lifeRecord, servantDolls, dollmakersKit, thaumicSpikes, secretElixir, cosmicPearl, witchDeck, battleship, mythrilArmor, ritualInks, spellBullets, greatWarRifle, witchPistol, jesterOniMask, farTalisman, masterKey, pewterCrown, sunShard, hydron, collection, witchKisses ]


hexVPN : Details
hexVPN =
    { name = HexVPN
    , class = Academic
    , content = Single 2 """
        The ultimate in secure internet access. HexVPN routes your internet traffic through a literal blackbox. The ominous hovering black cube with fractal engravings completely masks your connection in ways mortal VPNs can’t; Your traffic won’t even be seen or register as a request, and it allows you to access any public website that would otherwise have a paywall. No more sharing netflix accounts, and no region locks, rocking 666 Tbps up and down.
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
        This heavy gold “handgun” projects green quartz shards with equivalent force of a 45 Long Colt, growing its own ammo. You can direct a curse at the jade and emerald skull to imbue its shots with the curse effects until you replace the curse with another. Injuries caused by this weapon do not heal or benefit from healing effects for 1 week, during which time any means of cheating death are put on pause so long as their body is pierced with the quartz shards.
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
        The book of the woeful dead. Using this, you can combine the disciplines of Necromancy and Consortation. Demons you summon can be bound like spirits into Undead bodies, or a Golem (Hexes). Their personality remains the same, but they’re now permanent and don’t count to limits. The body is itself a form of payment for their service but with a body demons like the foliot wouldn’t be invisible, not being a spirit now. A Balor would be a a murder machine with thousands of years of combat training, but still limited to the capabilities of the given body. No effect on R5.
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
        A wooden carving of a gnarled spider. When placed under the floorboards, or buried next to the foundations, of a small home, it animates to grow hard wooden tendrils throughout the structure of the building and throughout its foundations. With meditation, you can deliver it commands that allow it to rise on eight spidery root-egs to travel where you wish it. It’s surprisingly nimble and can use its legs to anchor itself in trees, and create basic wood furniture or stairs on demand. If you have R5 Witchery, it can burrow into the earth to disappear into your pocket space, and emerge anywhere within 10 miles or to property you own.
        """
    }


nymphVessel : Details
nymphVessel =
    { name = NymphVessel
    , class = Warlock
    , content = Single 4 """
        This ornate pitcher holds an endless reserve of water from the Faegarden and the mythic baths of the nymphs. Pouting from the left pours cold clear waters.

        You’d be hard pressed to find purer water, with the perfect mineral content for sustaining life. Pouring from the right pours hot milky white waters saturated with minerals... and some other stuff. This water is extremely revitalizing, relieving pain on contact and releasing tension and stresses while sanitizing and washing all impurities. Leaves you with pristine soft skin. Drinking it can cure infection and replace a meal.
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
        This infernal machine is a construct of hell. Its chitinous wrought iron frame is unnaturally durable and heavy yet its ability to travel is subjective, relative to its own point of reference. You could drive up the side of a building or on a ceiling, and on the surface of water. Travels at up to 260mph. You can summon or unsummon it with basic unranked witchery, or you can bond it with your Rod to act as an alternate form, doing so adds your broomstick’s speed to the hellride’s top speed, and it gains the ability to fly. And likewise, is like new again on each summon.

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
        This gilded hammer of comical proportions should be impossible to wield. Most humans can’t even lift the head alone. But when activated in the hands of a bonded witch, it generates its own lift field with Levitation magic, and when swung it actively propels itself in short bursts. It weighs 950Ibs, but strikes as though it weighs 2 tons. By infusing it with r3 magic, the hammer can generate this lift field propulsion effect on contact, making a struck target weightless and then propelled up and away with great force. Infusing it with r5 magic, you can make yourself and the hammer impervious to harm for 3 minutes, though you’re -20% slow.
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
        This adamantine tower shield can adjust its size from a dinner plate to up to twice your own height and up to 3x your own width with a mere second of shifting. As adamantine, it’s the only substance a riftblade can’t cut, impervious to harm. This shield has the added benefit of special anchors that allow it to resist knockback and other forces, though its resistance wears out with overuse. With an infusion of mana = to the rank or otherwise as appropriate, the shield can absorb any magic effect, then replicate it itself using twice its normal mana cost. The shield can store 4 effects at once. Has 40 absorb charges per day.
        """
    }


alchemistStash : Details
alchemistStash =
    { name = AlchemistStash
    , class = Academic
    , content = Single 6 """
        This set of belts, garters, and threads allows you to store potions and similar sized items beneath skirts, dresses, or simply in cloaks. Any item stored in the Alchemist’s Stash can instantly be called to your hand with no delay, appearing in the blink of an eye When not visible, the items leave no distortion on the clothing, neither do they clank, weigh you down, or are they felt, as though they were not there at all until you lift the hem or otherwise reveal them. They can’t be damaged when not visible. If you have Witchery, these can integrate with your mothergifts and appear when your garment is called. Stored potions are 20% more effective.
        """
    }


gemOfRenewal : Details
gemOfRenewal =
    { name = GemOfRenewal
    , class = Sorceress
    , content = Single 6 """
        This unique gem has the sheen of a diamond, the many colors of an opal, and the smooth roundness of a perfect pearl. It catches any present light to glimmer perfectly to any viewer. While worn, no damage leaves a visible mark on the body with no apparent damage. The damage is transferred to minuscule replica of yourself stored in the gem in suspended animation. You will not take damage until this replica is killed first. If you receive healing from any source, it likewise heals the replica. Area healing can affect the replica separately, doubling the benefits. Likewise, it double dips on any forms of regeneration you have.

        It takes 1 hour to rebind the gem to a new host.
        """
    }


prosthesis : Details
prosthesis =
    { name = Prosthesis
    , class = Sorceress
    , content = Single 2 """
        For each purchase, you have a gnomish magitech prosthetic replacing a normal part of the body with a rough mechanical replacement that’s clearly inorganic, but is finely in tune with your intentions and can be used to work magic as though it were natural flesh. It doesn’t have feeling, but you do have an awareness of its position and an yes/no sense of if it’s touched. Alternatively, you can have an elegant Doll limb replacing a part of you that either looks really nice or looks natural with a visible stitch line and some skin color variation, which does feel as normal but can sometimes behave oddly as though it belonged to someone else now & then, maybe even copping a feel.
        """
    }


violetLenses : Details
violetLenses =
    { name = VioletLenses
    , class = Warlock
    , content = Single 2 """
        These pink sunglasses let you see into the hearts of intelligent beings, seeing a sort of “flame” or ball of turbulent energy within them. The color and reactivity of the flame can allow you to judge their character and feelings towards others. If they deeply love someone on an intimate level, a small duplicate of their love’s own flame can be seen buming above their own. By swiping a finger along the left arm of the glasses, you can switch to X-Ray vision, selectively seeing through chosen objects or substances. Along the right arm, and you can switch to a thermal vision that has accurate detail Any given modes can stack with each other. Disable heartsight with a tap to the bridge.
        """
    }


manaCore : Details
manaCore =
    { name = ManaCore
    , class = Warlock
    , content = Single 12 """
        An amulet containing a finely tuned crystal that acts like a processor for channeling mana throughout. It operates on its own to run calculations on probabilities and magical theory to generate counterspell effects on incoming hazards provided it has at least a tenth of a second to. react. It has a backup defensive barrier visible when a hazard is within a few inches of the wearer’s body, producing a crystalline weave that can shatter if overwhelmed, but is comparable to a 4 inch thick steel plate, resetting when the core resets every other second.

        It can be used as a magic focus for performing magic as though using your Rod. Also boosts the damage of Spell Bullets by 20%.
        """
    }


magicTalisman : Details
magicTalisman =
    { name = MagicTalisman
    , class = Warlock
    , content = Single 20 """
        Legendary relics of considerable power, these talismans contain Rank 5 magic within them. While it is on your person, you can use that magic as though it were your own. They’re decently sized and not the easiest things to conceal, producing a glow when used, and actively require exposure to open air to function, so they’re wom like accessories in various fashions. A person can only be linked to one at a time, requiring a full minute to rebind to a new talisman.

        The magic it possess can be any magic specialization, with no penalty or restriction applying to it. The magic chosen and its affinity influence the appearance.
        """
    }


treasurersMint : Details
treasurersMint =
    { name = TreasurerSMint
    , class = Academic
    , content = Single 6 """
        Your very own proper Kiss Mint, a small cube which expands into a room sized chamber within which a large treasure chest can be the focus of the ritual to mint a new Kiss. Instead of minting a single kiss, a 10 minute ritual will mint the entire chest full of Kisses, provided you have a chest full of small copper coins. 5 chests of small coppers can be transmuted into a chest of large coppers. 5 chests of large coppers produce a chest of small silvers. 4 chests of small silvers produce a chest of large silvers, 10 chests of large silvers produce a chest of small golds, of which 10 produce a chest of large golds. You can skip steps by performing an equivalent ritual time using an equivalent quantity of initial materials.
        """
    }


companionBrick : Details
companionBrick =
    { name = CompanionBrick
    , class = Sorceress
    , content = Single 2 """
        Contains an object relevant to a any one potential Companion. This object allows you to bypass normal dynamics to take that companion regardless of restrictions or personality, your object is your “In” with them to form a bond in one manner or another. This item can be any manner of plot contrivance that smooths or outright enables a relationship with that companion, either platonically or even romantically, at least opening the door to the possibility

        This is your golden ticket to obtain a legitimate reason to befriend any companion you may otherwise struggle to rationalize.

        Alternatively, you can keep the cube itself as a companion. It’s a good listener.
        """
    }


heirloom : Details
heirloom =
    { name = Heirloom
    , class = Sorceress
    , content = Single 4 """
        It’s pretty rare, but it seems your mothergifts aren’t new. You’ve inherited heirloom gifts from a witch ancestor in your family line. Choose one magic specialization have been their focus that they are known for, and halve the cost of buying that magic for yourself. You can use this on a magic you don’t have the affinity for, or halve again the cost of a magic you do, but you cannot use that magic specialization when not wearing your mothergifts, your garment and hat almost all witches get from RO Witchery, and using your rod. Downside is your mothergifts have a preferred shape, the shape most identified with that ancestor, and any alterations will only last about 1 hour before they start to revert unless you reapply the change. Doesn’t affect rod forms: i.e. broom.
        """
    }


riftblade : Details
riftblade =
    { name = Riftblade
    , class = Warlock
    , content = Single 4 """
        Normally portals are stationary, but the some witches managed to figure out how to convert one-way proxima rifts into a sliver acting as a blade that freely moves through space unanchored, bound only to its hilt. Rifts as mentioned, are the sharpest known objects in Witchdom, a riftblade will cut through anything without resistance except other rifts, but will also share the effects of any proxima the witch has access to, if any, contained to only apply an effect on contact. This also alters the appearance of the riftblade as appropriate, such as red heat rifts, yellow storm rifts, white cold rifts, blue water rifts, black void rifts.

        Basic riftblades look like mirrors The blade can still be shattered as described by Portals, starting from R1. If broken, just retoggle it.
        """
    }


lifeRecord : Details
lifeRecord =
    { name = LifeRecord
    , class = Academic
    , content = Single 6 """
        These somewhat dangerous keys are often destroyed on sight by Hawthorne and the ORC, while the Watchers, Hespatians, and Alphazons would kill to acquire them. This key lets you tap any manner of depiction of a target individual to enter the Index of Everything, specifically the All Archive wing in a research center, though you can’t leave it to go anywhere else in the archive.

        Here you’ll find every single scrap of information about a person that has ever been written, drawn, photographed, or otherwise recorded. Even if nobody ever saw it. Every video, every clip, every blurb or diary entry, so long as it’s about the individual, it’s here somewhere. Only works on Humans and Witches. One use per month. This place can’t be visited by any other means.
        """
    }


servantDolls : Details
servantDolls =
    { name = ServantDolls
    , class = Warlock
    , content = WithChoices """
        One of the first attempts to create Dolls resulted in these beings. They’re 2ft animated porcelain dolls that live to serve and can emulate the intelligence of a human with low IQ and social difficulties but can faithfully carry out tasks as directed. They bond to a single master, able to bond to a new one if their last died for good. They’ll do all you ask, and might take initiative to do things to serve you that you didn’t ask them to do, based on how you shape your relationship with them and how they get a feel for the things you want or not. If broken, they become whole again at midnight. They’re surprisingly as strong as a full sized human despite their size. You have n × n dolls, where n = purchases. Take this relic 3 times to have 9 dolls.
        """ (List.map ((*) 2) <| List.range 1 4)
    }


dollmakersKit : Details
dollmakersKit =
    { name = DollmakerSKit
    , class = Warlock
    , content = Single 6 """
        A sewing kit with pitch black threads and a small box that always has a new button when opened, and a red stained needle This sewing kit can be used over ja 1 hour ritual to sew a living human or witch, to convert them into a Doll, replacing their previous witch type if applicable, land if they weren’t a witch then they become a witch with the Doll type. This can also be used to replace parts of the person with parts of another, including missing or non-functional parts. They’ll have 2 Power to spend per individual body used in the new doll. The brain determines the factual person that becomes the doll (including soul). You might use hexes to resize parts or organs to fit the proportions better. The resulting being cannot disobey your commands.
        """
    }


thaumicSpikes : Details
thaumicSpikes =
    { name = ThaumicSpikes
    , class = Warlock
    , content = Single 4 """
        Long crude iron spikes that are infused with the immortality curse. A being pierced with one of these spikes cannot die by any means while the spike remains within them. Once inside, they have a strong magnetic-like pull to remain inside, requiring around 800Ibs of force to pull free. They feel pain as normal, including the pain of the spike itself. If an effect would slay an immortal being, one of the spikes will shatter into dust and save the life of the bearer, taking the hit for them. Such as a Jade Bolt, or a Golden Weapon from VoL. While spiked, they’ll also regenerate from a broken limb in a minute, limb loss in an hour, bullet wounds in seconds. You have 3, and can craft more using Curses 5 to curse iron spikes.
        """
    }


secretElixir : Details
secretElixir =
    { name = SecretElixir
    , class = Academic
    , content = Single 12 """
        A very illusive potion, the creation process known by Alphazon executives and Hespatian Crowns. You will come across one. Drinking the full bottle, tiny as it is, imbues you with the ability to craft the potion yourself if you have Potions 5. A single drop is an undetectable, untraceable poison that causes death in a completely random manner in a random time within 1 month. 2 drops will make a person tum inside out. 3 drops will sever a witch’s ability to use magic, or undo the effects... Or make a human a witch with a random witch type and 3d20 Power they’ll grow into. 12 drops per bottle.

        Requires distilling 5 rank 5 potions in one, & activating the potion with your activated blood.
        """
    }


cosmicPearl : Details
cosmicPearl =
    { name = CosmicPearl
    , class = Sorceress
    , content = CosmicPearlContent 10 """
        This pearl of great power is less of a material object than it is a tangible fold of crystalized probabilities. You can soak it in any water and that water will become equivalent to a T4 ingredient for Alchemy or other magics, and when on your person it buffs Elemental magic by 25%. Or, you can melt the pearl into a solution of molten gold, to disperse it throughout and transmute it into an “Elixir of Many Colors”. In drinking it, you can permanently replace one affinity you possess with any other, or add a new one entirely. A creature can only hold a limit of 4 affinities at any one time. A Warlock with a Soulbound Pearl has 1 use for changing affinities, and keeps the passive benefit.
        """
    }


witchDeck : Details
witchDeck =
    { name = WitchDeck
    , class = Academic
    , content = Single 6 """
        You have a deck of 60 blank cards that can be used by having a witch or other supernatural entity sign the card and provide a drop of blood or rough equivalent. In doing so, you can now summon a complete duplicate of that entity using the card, once per 24hr until dismissed or destroyed/slain, only one instance of the same entity at a time. The entity in turn if ever slain in a way that would have been permanent (or first regardless of other methods if they choose), can hijack the next instance you use that card (which will have a golden glow on it) in order to return to return to life but under your control for 24hrs. You can sign a card yourself to spawn a duplicate you. Personalities remain intact but memories aren’t.
        """
    }


battleship : Details
battleship =
    { name = Battleship
    , class = Warlock
    , content = Single 20 """
        Holy _basil_, you found an entire heckin’ arcanotech battleship. It has zeppelin-like bulbous frame with a top like a military destroyer. It flies at up to 120mph at full steam which drains its stored up power over an hour but cruises at 80 without gaining or losing power. Has ammo bays that fabricate crystal cannonballs while keeping them stable, storing up to 9 each for a total of 90 shots, generating 1 ball per minute. The balls wink out of existence after 10 minutes of exposure. When lsed in one of the cannons, they travel in a straight line until impact, causing a 40ft diameter explosion as a swirling meat grinder of armor piercing crystal slivers that shreds tanks. Hull is foot thick mythril plating.
        """
    }


mythrilArmor : Details
mythrilArmor =
    { name = MythrilArmor
    , class = Warlock
    , content = Single 4 """
        This silvery white armor is made from mythril, legendary for being so lightweight it falls like a feather, while being nigh impenetrable. A dragon’s tooth would chip biting into solid plates made of this, though it’d deal a good dent to the armor. Dragons don’t play around. It’s supernaturally cool to the touch, moderating the wearer’s temperature to stay slightly below room temp no matter the conditions, giving it resistance to hot or cold basted attacks and providing comfort in harsh deserts or arctic tundras. It’s light enough it’d be like wearing a thin silk nightgown, while in full plate, though actual mobility largely comes down to design. A chain shirt of mythril could stop bullets but the blunt force could still break a rib.
        """
    }


ritualInks : Details
ritualInks =
    { name = RitualInks
    , class = Warlock
    , content = Single 2 """
        A set of relic needles and a tome of ritual preparations necessary to create inks, paints, or blades that allow the direct application of Runes to the living body itself. Flesh isn’t as durable as metal trinkets, but it can come in handy to always have a rune on your person on your very skin.

        The rune can be disrupted if you suffer injury to the site of the mark, whether it’s a tattoo or bodypaint. Healing will restore a damaged tattoo or scaring, though not bodypaint.

        Fortunately, the inking / painting process is more cost efficient than classical runescribing, saving you 50% the standard costs when using Kisses and can utilize cheaper equivalent ingredients if not utilizing Kisses, for roughly the same general savings.
        """
    }


spellBullets : Details
spellBullets =
    { name = SpellBullets
    , class = Academic
    , content = Single 4 """
        A special arcane reloading bench allows you to use 1 Kiss, to produce magic bullets patterned after any other bullet.

        Your magic bullets are either a flat bonus of double effectiveness, or you can imbue them with any affinity you possess to cause a 1m elemental burst of that element that either adds the same damage potential as the bullet itself again, or converts all the potential damage of the bullet into the elemental effect.

        For every additional kiss you use, you can increase the damage potential by 1% and add 1m of either AoE or effective range.

        You can engrave the bullets with Runes or other magic effects you possess allowing the effects to be carried to the projectile itself, even if the projectile is just energetic.
        """
    }


greatWarRifle : Details
greatWarRifle =
    { name = GreatWarRifle
    , class = Warlock
    , content = Single 6 """
        Relics of witch involvement during the World Wars, in a parallel conflict referred to as the Lockheart Shism, where the witch community was divided in opinions roughly correlated to the human’s own dividing lines. These rifles look like mundane rifles at a glance, but they hide complex arcanotech engineering. They can generate a runic scope as effective as a full telescope in sync with your intentions to show trajectory calculations and distances. Spell Bullets fired from it are five times as potent with high velocity and the base aoe burst is 4m. Each rifle has a short “chant” using r4 mana drain to imbue the rifle itself with power to add 1 mile of range to the projectile, and adding the force of a howitzer shell to the attack.
        """
    }


witchPistol : Details
witchPistol =
    { name = WitchPistol
    , class = Academic
    , content = Single 4 """
        You have a particular form of wand in the shape -- roughly – of a pistol. This can be a Master Wand if you possess that perk. It can be separate from or it can be a form of your Rod. You can buy this again for spare wand-pistols, (though they all count as the same master wand). They can be loaded with Spell Bullets, disappearing into the wand when pressed against it. By default, the wands can shoot energetic projectiles comparable to a 9mm With infinite ammo. Or you can forgo a second pistol to upgrade the caliber of the base projectile. 9mm - .45 - 225 - .50. Using the wand you can extend the range of your magic by 300m by changing the origin of the spell to on-hit, likely with increased projectile velocity vs the normal spell’s own.
        """
    }


jesterOniMask : Details
jesterOniMask =
    { name = JesterOniMask
    , class = Warlock
    , content = Single 6 """
        This mask has a mind of its own, a sliver of The Shadow. When on your head its eye can be seen moving with a glimmer of a keen intellect. It has 360 perception within 120m, while its vision is telescopic. It can speak to you in a mirror of your own voice, within your head. Ithas great insights, but has its own goals and agendas, which usually include helping you out in most things When worn over your face, the rest of your body becomes enveloped in shadows and you gain a doppelganger of yourself that shares the telepathic link but is its own entity. By infusing the mask with R5 mana when you remove the mask from your face, the doppelganger can remain unshadowed like a twin of yourself. Only one at a time.
        """
    }


farTalisman : Details
farTalisman =
    { name = FarTalisman
    , class = Sorceress
    , content = Single 6 """
        A small shard from a high level Outsider entity. This relic is more like a parasite, it perpetuates its existence by bonding with your mana reserves. In exchange, it protects its host. While it is worn, you cannot die, any injury becomes superficial, a bullet through the head no more lethal than a bullet through the hand. The entity can perceive anything you perceive, and likewise you can see through its eye. You can grow additional eyes anywhere on your body, hard as stones and not sensitive. These eyes can also be placed on any surface you touch, including unprotected creatures. When closed, they leave no indication but a thin line. With an expense of R2 mana, you can grow copies of any part of your body anywhere you can see.
        """
    }


masterKey : Details
masterKey =
    { name = MasterKey
    , class = Warlock
    , content = Single 6 """
        These enchanted keys are highly prized. They don’t merely unlock something, but they bypass obstruction entirely. By placing a master key against any surface, you open up an entirely new gateway through that surface. This doesn’t transmute that surface itself, but shunts through it to completely bypass it, the surface unaffected in any way. The key turns into the doorknob. Yanking on the knob from either side returns the key and closes the gate leaving no trace.

        This CAN work through the Longing Mirror or similar effects.
        """
    }


pewterCrown : Details
pewterCrown =
    { name = PewterCrown
    , class = Warlock
    , content = Single 4 """
        Empowers curses while worn, you can deliver any curse you know instantaneously without pointing to a creature you can see. It also grants the curse of Submission, which can deliver orders that the creature is forced to comply with that can be summed up in one word and enacted within a time frame. At rank 4 curses, you can add a second word to have the victim influence a 2nd creature/object.

        *R1*: 2 seconds. Freeze, Clap.
        *R2*: 4s. Bow, Kneel.
        *R3*: 10s. Flee, Cower.
        *R4*: 20s. Kill It, Break That.
        *R5*: 60s. Go Die. Forget This. (Something within the past hour).
        """
    }


sunShard : Details
sunShard =
    { name = SunShard
    , class = Sorceress
    , content = Single 4 """
        This jagged prism of light appears as a 2 dimensional reverse silhouette of light from any given angle. In the hands of a witch it can adopt the appearance of any non projectile weapon it makes contact with, storing forms for later use. It weighs nothing for the witch, but strikes as though it weighed 3x as much as it should. It produces light like a torch, illuminating roughly a 30ft/ 10m area around you when active. It burns enemies as though it were red hot. It can be bonded with your Witchery Rod, it taking on the sun shard’s traits, in any given form.
        """
    }


hydron : Details
hydron =
    { name = Hydron
    , class = Academic
    , content = Single 2 """
        This masterwork cauldron is prized among potionmakers. These cauldrons are able to brew six different potions within the same pot all at once by pouring or inserting separate ingredients in through the six different “heads” adorning the cauldron, Shared ingredients like water can go in through the main opening. The six heads can be lowered down and opened/closed to pour that batch of potion out like a nozzle. By starting with an empty hydron, you can pour functional potions into the heads to mix the potions into a singular potion with the effects of all at once distilled into a single vial.
        """
    }


collection : Details
collection =
    { name = Collection
    , class = Academic
    , content = WithChoices """
        Rather than a relic, I’ll let you have a go at my collection of potions and rune-charmed trinkets, up to Rank 4. -1 per potion, -3 per rune charmed trinket. Limit 3 potions and/or 2 tunes. Cost is totaled as one purchase.
        """ (List.range 1 9)
    }


witchKisses : Details
witchKisses =
    { name = WitchKisses
    , class = Warlock
    , content = WithChoices """
        One of the closest things to a standardized currency in witchdom. Any witch can tap into Kisses as a source of mana or as a ritual reagent, spending them for practical purpose giving them intrinsic value. There are Gold, Silver, and Copper kisses. with two sizes each, large and small. Small Coppers are K1 large coppers are K5, small silvers are K25, large silvers are K100. Small golds are K1,000 Large golds are K10,000 “Burning” K25 would max out a novice witch’s Low mana capacity. For each purchase of this “Relic”, you acquire K1,000,000. Repeat purchases are totaled, not separate.
        """ (List.map ((*) 2) <| List.range 1 20)
    }
