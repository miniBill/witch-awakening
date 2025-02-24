module Data.TypePerk exposing (Details, all, title)

import Generated.Types exposing (Affinity(..), Race(..))


type alias Details =
    { race : Race
    , cost : Int
    , content : String
    }


all : List Details
all =
    [ neutral, daeva, ifrit, siren, naiad, dryad, oread, lamia, aurai, nymph, gorgon, luxal, kekubi, sylph, undine, sprite, empusa, lilin, erinyes, hannya, taura, wulong, dravir, doll, vanir, changeling, elf, orc, pharon, jotun, hollow, dwarf, wither, mimi, sword, xeno, cyborg, spider, gnome, pixie, fairy, genie, gemini, phlegeton, moorwalker, dictum ]


neutral : Details
neutral =
    { race = Neutral
    , cost = 4
    , content = """
        You have a “Second Spark” of untapped potential that you can work with and expand with time and effort. At the upfront investment into this perk, you have this spark, which you can feed with effort.

        You can choose a second class type to gain that class type’s growth method, you need only find balance in managing the two separate means of growth.

        In the face of a world of magical threats, you also resist magic damage and magical hazard durations by 20%, giving an edge.
        """
    }


daeva : Details
daeva =
    { race = Daeva
    , cost = 1
    , content = """
        Learn to manifest fine tuned control over the functions of body. You can raise or lower your temperature to tolerate limited temperature extremes such as laying comfortably bare in snow, or hot desert sands. You can reduce your heart rate to just one beat per 10 minutes in a trance state where you can survive without air and general delay death for up to weeks. You have perfect fertility control, and you can choose to pass on Witchiness to your offspring to guarantee or prevent witch children, as well as choose sex and gene expression.
        """
    }


ifrit : Details
ifrit =
    { race = Ifrit
    , cost = 2
    , content = """
        Gain the ability to dismantle your body plasma to become a flying skull, and propel yourself on jets of flame up to 200mph, or 10x the speed of your broomstick from Witchery, flying through the air like a missile or rocket. You can explosively start a skull flight while your whole body is still present, it breaking apart to kick start the flight, but regaining your body takes about 30 seconds of reformation.

        In skull form, you can only use [Fire] magic or magic that could reasonably be performed without hands.
        """
    }


siren : Details
siren =
    { race = Siren
    , cost = 3
    , content = """
        Learn to amplify the allure of your voice to project a truly mesmerizing melody that strongly tugs at the psyche of listeners who hear it, carried by the wind an additional 50% further than a voice would normally be carried. Those captivated become oblivious to danger and seek out the voice. You also gain echolocation, able to visualize a 3D map of what your voice touches.

        For 2p less, you don’t have to sing; Your voice is permanently hypnotic without your control over it, other than not speaking.
        """
    }


naiad : Details
naiad =
    { race = Naiad
    , cost = 2
    , content = """
        You have a strong connection to aquatic life with a spark of divine right to rule the sea. You can understand and be understood by any salt water creature greater in size than baby shrimp. Creatures at least as intelligent as a 4 year old human are also subject to passive Suggestion to obey directives.

        There is no size or time limit, just an intelligence limit. If you actually have the Suggestion perk as well, your suggestions to less intelligent aquatic life is a strong compulsion even at risk of certain death.
        """
    }


dryad : Details
dryad =
    { race = Dryad
    , cost = 2
    , content = """
        Some dryads are known for exceptional adaptability. Touch any plant or fungi and you adapt your body to become related to that plant and adopt its traits and abilities, and your tree form becomes a tree-like variant of that plant. Touch a cactus to turn into a giant cactus and in either form, resist desert heat, and you can grow cactus fruit. You grow any fruits or vegetables at normal rates (Unless you have Cornucopia) though other traits take hold immediately. You store one adaptation at a time, touching a new plant to override the previous. Your dryad’s seed becomes more comparable to a fungus, releasing spores on death that scatter, first to mature will awaken while the others wilt.
        """
    }


oread : Details
oread =
    { race = Oread
    , cost = 3
    , content = """
        Your burrowing leaves no trace of disturbance at all after a moment of ripples that return to their original state, and the earth actively aids your movement within it, boosting your speed to your intended destination by 200mph (Plus the speed of your Rod if you have Witchery, including if you have Hellrider. You do not have to be riding it, but you could if you want. An Oread could sculpt earth easily by molding it with their hands, but you can do so with greater ease, simply touching an earthen surface to shape it within 5m (x Earthmoving ranks) to your desired shape within your artistic limits. Earth shaped this way doesn’t count as worked earth.
        """
    }


lamia : Details
lamia =
    { race = Lamia
    , cost = 4
    , content = """
        Your consumption is more refined than most, able to actually utilize the magic energies within and surrounding a witch that you may have consumed. While you have a witch inside you, you can temporarily use one witch specialization up to one rank less than their own and/or use a perk they have. If you fully consume the witch unto death and digest the body, you gain 10 Focus, Might, or Favor for your class type, 1 for humanoids in general.

        You’re also able to swallow whole humans with more ease, in seconds instead of a minute or two.
        """
    }


aurai : Details
aurai =
    { race = Aurai
    , cost = 4
    , content = """
        With significant effort, some aurai are able to figure out a way to harmonize with a wavelength that has resonance with their own timeline. Once every few seconds they can consciously project their intentions to move somewhere they can clearly see, then accelerate their moment in time to skip the travel, or backwards to someplace they were within the past 6 seconds. Moving forward takes a small mana cost per 60 meters, while moving backwards takes just a little more mana per 6 seconds (going back has exponential cost increase) By sacrificing their current memory, they can revert to their physical condition at that time, though they know they jumped back from the future.
        """
    }


nymph : Details
nymph =
    { race = Nymph
    , cost = 2
    , content = """
        Nymphs figured out a way to bypass their mana capacity limitations by acquiring the ability to reabsorb water they’ve imbued, becoming like an external battery stored for later. A nymph in her pool/bath can store mana and access it later, more like a second charge method, but while in their bath they have direct access, effectively having a massive mana capacity, or using it like a mana potion if separated, a vials worth replenishing 15% their capacity. Nymphs often maintain such baths the way a dragon might protect and enjoy their hoard. They now no longer age if at least vials worth of imbued water is within 30m of them, and gain the [Mind] affinity
        """
    }


gorgon : Details
gorgon =
    { race = Gorgon
    , cost = 5
    , content = """
        Some gorgons have a significantly more dangerous gaze than others. Whether or not gorgon’s eyes are seen, their gaze petrifies everything in their line of sight, slower than if there was eye contact. Simply being in this gorgon’s line of sight will reduce speed by 10% per second, reducing by 10% per second not in line of sight. At 100%, they’re fully petrified as normal, and the gorgon is able to manipulate petrified victims as though they were still living and malleable, allowing poses, and the gorgon can now un-petrify by thinking about it with a touch, selectively, such as their face to let them speak, or for torture.
        """
    }


luxal : Details
luxal =
    { race = Luxal
    , cost = 3
    , content = """
        Learn to trigger explosive growths of your associated gemstone or metal, growing from your body in the way you see fit. You can grow no more metal or stones than you have consumed in the past week, which then doesn’t count to subsequent growth after it is used. Simple shapes like blades or spikes are easy, but precise craftsmanship requires actual artistic skills, though with the skill or a reference on hand, you could create jewelry or mint coins.

        Your growths can be disconnected to simply fall off, or can be projected with the force of a 50 cal bullet. How that force translates to the actual projectile depends on the projectile, a spear of metal being slower than a small bolt or bullet shape.
        """
    }


kekubi : Details
kekubi =
    { race = Kekubi
    , cost = 5
    , content = """
        Advanced control over the ash, soot, or smoke produced from their body, both what they produce deliberately, and what composes their own body. They can use this to manipulate it (Aesthetically a lot like some form of shadow manipulation) within 30 meters of themselves, in addition to effectively turning any other magic into this manner of soot manipulation. The primary unique usage of this would be the control of their own body, freely breaking apart their body to control it remotely and in pieces as small as a grain of soot, or a full limb, or to levitate themselves in full. They can still receive damage when broken apart, though naturally they’re harder to physically damage (Blunt/Pierce/Slash).
        """
    }


sylph : Details
sylph =
    { race = Sylph
    , cost = 3
    , content = """
        Some sylphs inherit the ability to take their near-etherealization to the next level; With this perk you can fully phase yourself to ignore solid matter, allowing you to walk through walls. This is a pseudo-biological trait, so you can’t bring other objects along with you that you want, it’s all or nothing. This includes your mothergifts, so you might want to avoid witches and mediums while ethereal, and you’ll need a different escape method if you want to bail out of an area with an object you intended to swipe and I’d recommend learning to summon your mothergifts quicker to regain modesty when you end the ethereal phase. You can’t breathe while phased, but you’re invulnerable to anything that can’t harm a spirit.
        """
    }


undine : Details
undine =
    { race = Undine
    , cost = 2
    , content = """
         Undine can be mildly difficult to notice underwater, but some are able to fine tune their body to become perfectly transparent within a body of water. Effectively, an undine with this perk is outright invisible within water and camouflaged within rain, and is able to maintain control and movement in non-solid forms, such as moving around as a puddle, where normal undine would be immobile without becoming more solid. This allows the undine to pass through any surface that water could pass through, such as cracks under a door or seams of improperly sealed wood. You could invade the body of a creature but no, becoming solid within won’t do harm, being limited to available space.
        """
    }


sprite : Details
sprite =
    { race = Sprite
    , cost = 2
    , content = """
        You can grow in size up to a height of 4'10\\" / 147.32 cm. Still short, but now you can properly interact with normal civilization Your production of silk and honey is upscaled appropriately. and while your venom and honey has more volume, proportional quantity is still required, being diluted, though the honey is just as flavorful and with the same texture. Proportionally, a full sized Sprite could produce about two bathtubs worth of honey per day. Honey production is on demand though an individual could have an overproduction problem. For 2p less, you could have a persistent honey leak.
        """
    }


empusa : Details
empusa =
    { race = Empusa
    , cost = 5
    , content = """
        Magnify the corruption sustaining you in order to acquire the [Wind] tag in addition to the Empusa’s other elements. You can adopt a mist form in which you’re hard to recognize and can’t interact physically, while being effectively immune to physical harm, though energetic harm can still get you. Your speed is unaffected but you can fly. Transitioning between mist and physical only takes a second. Mist or not, you can see in pitch blackness. Additionally, you’re 4x as strong, fast, and reactive.

        For -15p (epic), you can also or instead take SYWTB A Vampire CYOA: [https://imgur.com/a/wDLuzus]
        """
    }


lilin : Details
lilin =
    { race = Lilin
    , cost = 3
    , content = """
        The quintessential gift of the desire demon, almost always present with high level succubi: Lilins with this perk can invade the dreams of any sleeping creature capable of understanding language. The lilin then establishes a full lucid dream that is under their control, though witches with Beauty Sleep or Digicasting can resist and fight for control. Within these dreams the lilin can negotiate with the creature to establish a pact where they consent to maintaining this dream, and they won’t wake up unless freed by a third party or the lilin. Any taboo within the dream still counts for charge.
        """
    }


erinyes : Details
erinyes =
    { race = Erinyes
    , cost = 3
    , content = """
        Some Erinyes are able to channel their curse to harm themselves with a blade or piercing object if there is currently an active curse mirroring harm onto another. That other can then be directly harmed by the weapon that has the Erinyes’s own blood on it. The next attack that harms the linked creature can be healed as normal, but the pain will forever remain fresh and will have an ink black scar. These marks of pain last until removed by a Cheraphim and can affect creatures protected by runes of warding, as it’s considered indirect, riding on the physical attack. (Likewise, the Erinyes’s curse itself works)
        """
    }


hannya : Details
hannya =
    { race = Hannya
    , cost = 4
    , content = """
        If you think an angry Hannya is intimidating, wait until you see one with this perk: These Hannya have up to a further 100% boost to strength and durability based on their degree of anger, hate, or simple desire for violence. ... Per cup (measure) of alcohol they’ve consumed in the past 24hrs. Up to 2,000%, or x20. The higher the bonus, the less immune to intoxication they are. This bonus coincides with a physical increase in size by 25% per 100. This can be direct upscaling with proportions intact, or just the arms, or can correlate with a full restructuring of their skeletal structure and musculature into a more monstrous appearance.
        """
    }


taura : Details
taura =
    { race = Taura
    , cost = 2
    , content = """
        You don’t have powerful animal legs for nothing. Taura with this perk are capable of moving on their own power up to 260mph. If you have ranks in witchery, you can add the max speed of your broomstick. With focus and an expenditure of mana, you can enter the Longstride, where your perception shifts and you partially elevate to a higher dimension. It feels like the planet shrinks to a small planetoid where you can clearly see the curvature on the horizon and see ghostly models and representations of the actual landscape of the world. In nonspherical worldspaces, it shrinks all the same but with no visible curvature. In effect, you can move a mile per 3ft of relative movement.
        """
    }


wulong : Details
wulong =
    { race = Wulong
    , cost = 2
    , content = """
        Wulong are strongly associated with the arts, but some even more so, having an ability that digicasting is in part based on: Some wulong are able to physically enter any painting or written depiction that they or another wulong has created, to enter the space in the manner that it was envisioned. Within one of these spaces, they can see a tangible fourth wall somewhere within that acts like a window through the painting or page into the real world, but this can be shifted to show through any other painting or page with the same image or description, and then step through this fourth wall to return. They all share the same space and something can be left within it, while something taken tums to ink or paint outside.
        """
    }


dravir : Details
dravir =
    { race = Dravir All
    , cost = 5
    , content = """
        Unlike most type perks, it’s rare to see a Dravir without this: The ability to assume the form of a dragonbeast, a form of humanoid dragon-type monster, being a bipedal dragonoid with 5 digit hands with full functionality of normal human arms, tipped with retractile razor claws, and they gain wings that can fly. May or may not have drastic sexual dimorphism depending on your lineage. Which is to say, female dragonbeasts may still look far more human with changes centered mostly around the back and limbs. This form empowers the Draviri’s physical attributes by 150%, and empowers their breath weapon by 300%. Dravir with this perk do not age past prime. Can partially transform, such as just growing wings.
        """
    }


doll : Details
doll =
    { race = Doll
    , cost = 4
    , content = """
        Some dolls are known for the ability to occupy multiple frames at once. With this perk, you gain the [Mind] affinity, and the ability to remotely operate a second doll form that looks similar or identical to your original body. For every additional power point invested in this perk, you can control an additional body + 1 per point, ie; [1] - 1, [2] - 3, [3] - 6, [4] - 10. Any body can be repaired as normal to resume function, but even when a body would be incapable of moving on its own, it can be remotely puppeted by force from a still functioning body. Every doll body can then also have a secondary form between 6 inches and 2ft tall that is a reduced quality depiction of the full appearance, like a literal child’s doll of themselves.
        """
    }


vanir : Details
vanir =
    { race = Vanir
    , cost = 5
    , content = """
        The ability to shape ice form of any mundane object up to the total volume of a solid 3ft cube. Which could be a 15ft ladder for example, the total volume of the ice used fitting within that cube. You could then create a second ladder joining the first to extend it

        The ice constructs of a Vanir are hard as solid steel. These ice constructs melt at 1/10th the normal rate of ice. Effectively permanent in cold regions where ice wouldn’t melt, until broken. Creation is near instant, but can’t have moving parts, and is created within 60m, but trail of ice must travel to where you intend to create the object (if the object wouldn’t be touching your hand already), moving at 10m per second.
        """
    }


changeling : Details
changeling =
    { race = Changeling
    , cost = 4
    , content = """
        While they can freely assume the appearance of any young humanoid, some changelings have an enhanced shapeshifting to assume the form of any object with volume like that of a ping pong ball, up to that of a fridge.

        These changelings can now also assume the form of mature humanoids. In either case, they have to have a visible reference to work off of and it only includes details the changeling knows about. Tum into specific person and you might not know about their mole by their nethers, for example, or the inner workings behind the fridge.
        """
    }


elf : Details
elf =
    { race = Elf
    , cost = 2
    , content = """
        Graceful by default, some elves are supernaturally dexterous and capable of impossible feats. These elves can balance in any position on any surface, allowing them to stand on a knife edge provided the sole of their shoe can withstand it. They can walk on walls or on ceilings. They could jump and touch a ceiling with one finger to lift themselves up to stand upright, upside down on the ceiling. Again, so long as the surface would be able to support them. Note elves weigh half as much as you would expect them to, and they’re often slender.
        """
    }


orc : Details
orc =
    { race = Orc
    , cost = 5
    , content = """
        Orcs with this trait are paragons of will and can endure any treatment or guile without faltering. Their sense of pain is dulled, and their force of will can allow them to survive total organ failure for hours while walking on shattered shins over a field of glass. So long as one muscle fiber is intact, their limbs will not give up. Even severed, an orc limb remembers its last goal. A beheaded orcs body continuing to fight, the head surviving for a few hours. Any orc parent gains this perk for free when their children are in danger, ending when they are out of harm’s way.
        """
    }


pharon : Details
pharon =
    { race = Pharon
    , cost = 4
    , content = """
        Pharon with this perk have a faint golden aura surrounding their head they can suppress autonomously just by thinking that they want to be sneaky, otherwise if they think they want to be ostentatious, it can illuminate a room.

        Their head is invulnerable to damage anywhere there is skin, and their bite force is capable of biting through steel plates an inch thick, and chewing it. Though they probably don’t want to swallow it. If they have a beak or something similar like a horn, their neck has enough force to puncture said steel plates.
        """
    }


jotun : Details
jotun =
    { race = Jotun
    , cost = 4
    , content = """
        A Corrupted jotun’s giant form loses their skin leaving exposed muscle and fat tissues. Their flesh is as resistant to harm as steel with twice the strength of a normal jotun. The jotun is capable of rapid regeneration, passively regenerating the equivalent of a bullet wound per second. They can focus to enhance their regeneration with 10% of their mana capacity to instantly regenerate a lost limb or equivalent. Additionally, their size change is more intimidating. Jotun that are lost to addiction to human meat get this perk free, but they’re mindless predators.
        """
    }


hollow : Details
hollow =
    { race = Hollow
    , cost = 4
    , content = """
        Hollows by default are able to incorporate ores they burned into the material composition of their armor, but some are more adaptive than others. Hollows with this perk can actively break down and replace their armor with ores, ingots, or any source of metal, and can incorporate special metals such as Mythril, or Adamantine, if they can find it. The process causes their armor to tile away in flakes as new flakes settle into place, within a few feet of distance from the materials used. This can also be used to actively heal them, siphoning materials on the fly for patch jobs repairing their armor at the rate of a gauntlet per second, with enough materials sufficient to replace the injury.
        """
    }


dwarf : Details
dwarf =
    { race = Dwarf
    , cost = 3
    , content = """
        Dwarves tend to have secretive & protective societies, but the dwarves you see outside of their underground stronghold cities are out on a mission with unrelenting focus giving two notable reputations: You can take this twice, once for each.

        *A) Giantslayers*, certain dwarves are invigorated by ancestral spirits when facing giants (6.6ft+), empowering their speed and damage dealt by 50%.

        *B) Manabanes*, certain dwarves have particular bloodlines that react to the presence of hostile magics. The duration of any detrimental magic effect is reduced by 75%. Even if no duration is shown, they can auto dispel in 3 days, and have 25% magic damage resistance.
"""
    }


wither : Details
wither =
    { race = Wither
    , cost = 5
    , content = """
        Some withers have a fearsome ability to generate a sand composed of heavily rusted metals that flows out from their body. Enough to fill a small backyard pool. These sands act as a strong acid that rapidly decays organic matter, plant or animal, dead or living. It doesn’t affect bones and has reduced effect on skin, leaving desiccated husks behind in a few seconds. The wither can control this sand as an extension of their will within 60m. The sand returns to their body when they wish it, but they slowly replace it over time at a buckets worth a day. The sand itself can have physical force comparable to a chainsaw, heavy blade, or maul, leaving rusty wounds. Tetanus hazard.
        """
    }


mimi : Details
mimi =
    { race = Mimi
    , cost = 4
    , content = """
        Mimis have some animal traits in addition to a couple physical characteristics, but with this perk they have an additional 2 general traits that you would associate with their animal type, and these two traits can be considerably stronger and supernatural in nature, with folklore and legends as valid inspiration for the associated trait, such as raccoons using tanuki folklore as a basis for a trait or two.

        Non-folklore traits can also be taken for one of these traits, and they’ll be augmented further. A normal trait granting strength might make you as strong as an adult male strongman, but an augmented trait like this could instead make you as strong as four such strongmen, for example.
        """
    }


sword : Details
sword =
    { race = Sword
    , cost = 5
    , content = """
        Swords with this perk are able to form especially strong bonds to a Wielder, or sometimes called a Master, depending on the school of thought involved. It takes a week to synchronize to a new wielder but once synchronized once it only takes a few minutes to re-adapt to a prior wielder. The Sword and Wielder merge their Mana capacity and Charge rates, and each are capable of using all Magic Specializations and Perks that the other possesses, when within a 1km radius of each other, and at any distance they can send telepathic messages to one another. They can cooperate to perform Harmony magic with extreme ease, their harmony magics being 100% more potent.
        """
    }


xeno : Details
xeno =
    { race = Xeno
    , cost = 10
    , content = """
        Some Xenos seem to have a natural affinity for working biomatter. Gain Necromancy 3 and Hexes 3.

        Their body is also enhanced, their outer plating is just as sleek as it was, but is twice as hard and they resist energetic damage by 75% (Cold, Heat, Electricity, Radiation, ect). Xenos by default already can exist comfortably in vacuum as well as high pressures.

        Using Necromancy to manipulate their own flesh and bone consumes no mana, neither does using Hexes, to work with their own biomatter, including eggs, allowing them to easily create networks of biomatter like veins to connect eggs in order to keep them supplied with blood.
        """
    }


cyborg : Details
cyborg =
    { race = Cyborg
    , cost = 6
    , content = """
        While all cyborgs have some degree of passive regeneration capable of providing healing / maintenance to inorganic parts of their body, it isn’t particularly notable or versatile beyond that. Cyborgs with this type perk however, have built in internal nanofabricators, little factories that produce femtomachines. This allows them to mend their body at a rapid rate, repairing damage comparable to a bullet wound every few seconds, and can fully repair their body, both synthetic and organic parts. Additionally, they can take Gadgetry and/or Integration at half price as though they had affinity for it, and when they do they are not linked to Alphazon, and their Gadgetry can be produce from their body on demand. Utilizing a silver swarm, if the witch has Metallurgy it’s usage costs half the mana it would normally.
        """
    }


spider : Details
spider =
    { race = Spider
    , cost = 8
    , content = """
        There are those among the spiders that can adopt the shape of a much smaller spider for convenience, between the size of a dime or a half dollar, or they can freely assume the form of a much larger spider that in its normal stance would be eye level with an adult human male’s chest. In this form their exoskeleton is plated like thick steel armor, and their speed matches that of a sports car.

        In this form they can produce webbing as thick and sturdy as a metal cable on demand capable of reaching roughly 100 meters when launched, easily latching on to nearly any surface, or they can create 10ft increments of nanowebbing very difficult to see that when properly anchored will slice through bone with just the force of a victim walking into it.

        The Spider can now learn Arachnescence, at a faction discount, their web is 3x durable.

        Spiders that take Metamorphosis can take the Zooarch form of a great spider. With r5 Arachnescence, they can instead choose the form of the Basilisk, replacing the snake aesthetics with the arachne aesthetics to your description, spider eyes function the same as the snake heads. For size, their spider body matches the zooarch, plus the human upper body adding height Each eye can think separately as mirrors of the same consciousness.
        """
    }


gnome : Details
gnome =
    { race = Gnome
    , cost = 15
    , content = """
        The mastery of a Gnome genius, an inventor’s spirit comparable to fictional mad scientists.

        You use engineering to replicate in a limited capacity, the effects of any magic effect of any Magic Specialization. Rank 1 takes 1 hour of work, 8 for R2, 24 for R3, 1 week for R4, and 1 month for R5, involving components of comparable rarity to a Potion of its rank. Instantaneous / non durational effects have a 25% failure chance, and each failure has a 50% chance to destroy the device. You can repair a destroyed device in 1/10th the time. Duration / continuous based magics have this fail chance at every 24 hours of total activity. Gadgetry can be replicated without fail chance. Can have a tiny cockpit to operate Power Armor.

        (Or a Golem made with Hexes, or Franken body made with Necromancy.)
        """
    }


pixie : Details
pixie =
    { race = Pixie
    , cost = 3
    , content = """
        Notable pixies are capable of generating pixie dust with their wings, a flap of their wings with intention to do so can release a pinch of pixie dust, well a pinch compared to a full size humanoid.

        - On Animals of sub-human intelligence, this lets the pixie issue commands that it will follow for up to 1 week.
        - On Plants, this causes an instantaneous bloom so long as. the plant is mature enough to do so, or invigorates the plant allowing it to grow 200% above normal, with 200% larger fruits or vegetables, etc.
        - On Humans or animals of equal or greater intelligence, this induces sleep unless they match the pixie’s own rank (Measured in highest rank effect they can manage). Considered magic.
        """
    }


fairy : Details
fairy =
    { race = Fairy
    , cost = 10
    , content = """
        Especially potent fairies that tend to manage local groups of fairies, or rise to become fairy princesses or fairy queens. These fairies are able to grow Fairy Rings, rings of mushrooms or flowers, that establish a domain-like space that those outside cannot see into, which may also be a gate through time to a unique timespace similar to time what a pocket dimension is to space. In this timespace, the fairy can dictate how much time passes relative to outside but only to slow it, so 1 day inside was as much as a year outside of it, or speed it so 1 hour inside was half an hour outside. The fairy can reverse time within the timespace, but it’s only physical, and time is still flowing forward normally, minds are unchanged. The timespace is only the size of the fairy ring created, leaving the fairy ring leaves the timespace. If the fairy has Portals, their fairy rings can act as gateways to any other fairy ring known to the one who enters.
        """
    }


genie : Details
genie =
    { race = Genie All
    , cost = -5
    , content = """
        All genies require this type perk. Genies have a Vessel, an invulnerable artifact between the size of a baseball, up to the size of an urn, it can change its appearance. It must appear opulent and valuable. If the Genie has Witchery, the pocketspace exists within this Vessel and with or without Witchery, the pocketspace is where she appears when banished to or simply returning to her lamp with a visible effect. As the vessel IS the pocket space, it cannot be stored within itself or in any other form of extradimensional space.

        A Genie cannot be more than 300m away from their vessel The last non-witch to touch the vessel becomes the Genie’s Master, who cannot be disobeyed (But can be argued with for a few seconds), the genie cannot cause the master harm. The master can choose the genie’s appearance and race type, when outside her vessel.
        """
    }


gemini : Details
gemini =
    { race = Gemini All
    , cost = 4
    , content = """
        Geminai with this type perk have even stronger bonds, capable of trading their consciousness and their half of their soul to trade places between their Egos (however minimal or great they may be divergent), or they can swap entirely as though teleporting. Gemini share a mental bond to feel the other’s emotional state, senses, & intention such that they can act as a whole, or they can fully communicate telepathically, each hearing the mental dialogue of the other if they “raise their voice”, or both parties can tighten the connection to hear their passive internal dialogue entirely as though speaking out loud to oneself near the other.

        Nothing can sever this connection, short of death.
        """
    }


phlegeton : Details
phlegeton =
    { race = Phlegeton
    , cost = 4
    , content = """
        Some Phlegethons learn to deliberately release a layer of their blood and keep it under control, acting as a sheath of boiling-hot mist which explodes into flames whenever it is struck. This doesn't impede their senses or injure them.
        """
    }


moorwalker : Details
moorwalker =
    { race = Moorwalker
    , cost = 2
    , content = """
        A few Moorwalkers who have ventured into space or unusual realms have found that they slowly convert desolation into moor, bare stone and vacuum changing to moss and thin air, then soil and a full atmosphere. This takes years of their presence to complete, but can affect a very large area.
        """
    }


dictum : Details
dictum =
    { race = Dictum
    , cost = 5
    , content = """
        Some Dicta are Oathbrokers, able to make voluntary oaths between two other parties self-enforcing, with a curse befalling any party who breaks the oath. They choose the rank of Curse, maximum equal to the strongest specialization they possess, but if it exceeds their ranks in Curse they can't control its details.
        """
    }


title : String
title =
    "# Type Perks"
