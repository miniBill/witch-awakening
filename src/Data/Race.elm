module Data.Race exposing (Details, all, intro, title)

import Generated.Types exposing (Affinity(..), Race(..), Size(..))
import List.Extra


type alias Details =
    { name : Race
    , tank : Size
    , affinities : List Affinity
    , charge : Size
    , content : String
    }


all : List Race -> List Details
all races =
    [ neutral, daeva, ifrit, siren, naiad, dryad, oread, lamia, aurai, nymph, gorgon, luxal, kekubi, sylph, undine, sprite, empusa, lilin, erinyes, hannya, taura, wulong, dravir races, doll, vanir, changeling, elf, orc, pharon, jotun, hollow, dwarf, wither, mimi, sword, xeno, cyborg, spider, gnome, pixie, fairy, genie races, gemini races, phlegethon, moorwalker, phantasm, golem, muspel, dictum, qareen, rusalka, lares, firebird, fresco, silverstream, revenant, petrichor ]


neutral : Details
neutral =
    { name = Neutral
    , tank = Med
    , affinities = [ Soul, Body ]
    , charge = Med
    , content = """
        The overwhelming majority of witches are Neutrals. There is nothing visually abnormal about them (by default). Even some witches who have the option to awaken as another race may want to choose to be Neutral for its baseline casual existence, no new dependencies or anything abnormal to consider. They’re effectively the “Human” witch, but witches refer to them as neutral rather than human to distinguish them from the mundane. They age slower than humans, at half the rate, and do so very gracefully, and they tend to be more attractive than the average human, and are harder to kill, with more efficient biology to reduce inconveniences with less biological requirements than normal by 25%, and prevent dying from a papercut, or from a tiny air bubble in the wrong place.

        __Neutrals draw Mana__ from Sleep, recovering around 1% of their Mana per hour of sleep, doubling per hour. 2 hours is more impactful than two 1 hour naps for example, regaining 16% for their 5th hour for a total of 31%, then 63% total for 6hrs.
        """
    }


daeva : Details
daeva =
    { name = Daeva
    , tank = Med
    , affinities = [ Body, Life ]
    , charge = High
    , content = """
        Daeva are to humans, what humans are to monkeys. They’re peak evolution and represent the perfection of body and overflowing spark of life. Unlike the other witch types, they’re unnatural for the excess humanity, not the addition of anything new. Where the others may have scales or feathers, you take normal human traits and take them further, with a body like a comic book heroine, a goddess in the flesh, free of any imperfections and basic inconveniences of mortality. They flatly don’t age, forever maintaining a mature but youthful physique. The average daeva height sits around 6ft 6inches up to 8, but short daeva can happen. They’re physically around twice as strong as a human male bodybuilder, without an obvious change in their body tone, and seem to have endless stamina. They feel emotions more strongly, while being in better control of them.

        __Daeva draw Mana__ from Emotions, either the presence of high emotions in others, or singular high emotions directed at them. ie; An excited crowd vs a lover are both high.
        """
    }


ifrit : Details
ifrit =
    { name = Ifrit
    , tank = High
    , affinities = [ Fire, Necro ]
    , charge = Low
    , content = """
        Ifriti are beings native to the elemental plane of fire. Their connection manifests in the most obvious manner of the hitches, as their bodies are burned away in their awakening, their body becoming a living conflagration of flame made flesh anchored and governed by a core skull. They bleed plasma and smoke while limbs severed from their core flicker away like dying flames. The fire of their bodies is slightly above room temp and does not burn, and is tangible enough to touch, feeling like normal flesh, though their hair is more gaseous and warmer. Their body is only a little transparent, enough to see the skull but not all the way through them. They inherently do not age, but can “eat” flammable fuel sources along with traditionally edible materials. Damage to the body is superficial and mended by absorbing flames, though damage to the skull must be healed by traditional means.

        __Ifriti draw Mana__ from Burning, the release of energy released by matter through burning caused by the witch.
        """
    }


siren : Details
siren =
    { name = Siren
    , tank = High
    , affinities = [ Wind, Beast ]
    , charge = High
    , content = """
        Sirens are beings of wind and music, melodies carried by the wind, and have a connection to birds. Their bodies grow leathers in a similar pattern as Nymphs grow scales, though some go further with leather plumage sufficient to be relatively modest without clothing, and are able to transfigure themselves into the form of a specific bird they’re associated with and hybrid states between the two. Whatever the bird type, Sirens are all well known for melodic voices that are deeply pleasant to outright hypnotically attractive. Sirens age at half human rates, and reverse their age through nesting in cocoons of feathers that reverses a year per day. A dead siren who is still intact enough can be nursed back to life by breathing into them a few times a day for 1-3 days depending on severity.

        __Sirens draw Mana__ from Breath, whenever they take someone’s breath away, when someone forgets to take a breath, when otherwise unable to breathe, or when the Siren overtly draws the breath from their lungs, within a few inches of their lips.
        """
    }


naiad : Details
naiad =
    { name = Naiad
    , tank = High
    , affinities = [ Water, Beast ]
    , charge = High
    , content = """
        Naiads are essentially mermaids. Their bodies have very fine scales that range from barely perceptible or providing a scant shimmer when hit by the light, to larger scales, typically concentrated around the back and surfaces facing away, leaving soft exposed skin facing forward fading with a gradient into scales at their shoulders and back, that glitter like precious metal or crystal coins. Naiads age normally outside of water, but when submerged in water they age backwards three times as fast, to their prime, equivalent to human at 20. A dead Naiad with a mostly intact body can be submerged in water to slowly bring them pack over a few days to a few weeks depending on severity. All naiads can focus to transfigure their legs into a long scaled tail with a fan-like fin for rapid swimming, exceeding 60mph, or 52.1 knots, on average. They absorb oxygen from water through their skin.

        __Naiads draw Mana__ from Submersion, recharging while immersed in water or rain, based on coverage and quantity.
        """
    }


dryad : Details
dryad =
    { name = Dryad
    , tank = High
    , affinities = [ Nature, Earth ]
    , charge = Low
    , content = """
        Dryads have plantlike bodies given human form, from which leaves or bark can grow. Many have branches or twigs forming horns or crowns, tiaras, and diadems, that can include berries or fruits. Dryads age normally but are able to assume the form of a tree, in which they can gain any nutrients from light and soil, and de-age at 10x the rate they’d age. The presence of a dryad prevents the growth of infectious diseases and enhances the growth of plants and creatures by 200%, growing with added Vitality. Fruits are sweeter and larger, animals are larger and healthier, etc. All dryads additionally have an internal seed the size of avocado pit. If they die, this pit will grow into a large underground pod with a radiant flower, that will regrow the dryad’s body over 3 months to roughly the equivalent of an 8 year old human. They’ll regain their memories over a week.

        __Dryads draw Mana__ from Nurture, from the growth of other living things directly attributable to the Dryad’s effort, whether it’s tending to plants, or feeding a person.
        """
    }


oread : Details
oread =
    { name = Oread
    , tank = High
    , affinities = [ Earth, Beast ]
    , charge = High
    , content = """
        Oreads have hair that is more fur-like, and they can have a fine layer of fur over their skin in a similar pattern to the Naiad’s scales or Sirens fur. Like the Siren, they have an associated animal, a land mammal, from foxes or cats to deer or cow. They can morph between this and their true form on a sliding scale. Earth easily gives way for them, allowing them to burrow with ease to the extent they can “swim” through sand, dirt- or bedrock choosing to leave a tunnel or not. Worked earth does not respond to this effect, such as concrete, or shaped stone. They age at half rate and regress to 20ish when they sleep buried in the ground. A dead oread sufficiently intact will return to life over a month if buried, based on severity. Both satyrs and foxgirls are examples.

        __Oreads draw Mana__ from Bonds, physical closeness to individuals with a close emotional bond with the witch, whether it’s family, close friends, lovers, even a pet. relaxing together in contact isolated from urgency or stress, only the warmth of cuddles. A long warm hug could fill 20%.
        """
    }


lamia : Details
lamia =
    { name = Lamia
    , tank = High
    , affinities = [ Fire, Beast ]
    , charge = Med
    , content = """
        Lamiæ are like twin counterparts to the Naiad with a bit of an inversion; Lamiæ by default have the lower body of a snake comparable to the Naiad’s tail but around a third+ longer, being 5-7 times the length of their human upper body. Their body itself may or may not have scales, but they usually have slitted irises and retractile fangs with a venom that induces sleep. Lamiæ can lay an egg that grows a young new body, catching their soul to hatch if later slain.

        __Lamiæ draw Mana__ from Consumption, the swallowing of live creatures up to the size of an adult man, which depending on the potency of a Lamia’s distortion, leaves no visible stretching past the throat. Creatures inside passively charge the Lamia’s mana based on the strength of its soul, making most animals non-viable and mortal humans the baseline. A human will charge the Lamia to full over 8 hours, at which point they’d die after growing weaker. They can be released before then. A witch would charge in 1 hour while surviving up to 24hr.
        """
    }


aurai : Details
aurai =
    { name = Aurai
    , tank = Med
    , affinities = [ Wind, Necro ]
    , charge = High
    , content = """
        Aurae are closely associated with the faewilds. Their irises are like kaleidoscopic gemstones, and have slender claw-like nails. Their voice innately carries with it a withering drain that steals a year of life per half second of exposure, which restores the Aurai’s own age to around age 20, banking excess drain. Their screams can induce a horrific vision where the listener is displaced to a random point in time at least 100 years in the past, for anywhere from 1 week to the rest of their life, remaining until they find a glowing butterfly never more than 1 mile away that serves as an anchor, only to return to the moment after they heard the scream, then drop dead if slain in the past at any point. This never affects the existing timeline. When slain, they experience this vision themselves and finding the butterfly resurrects them.

        __Aurae draw Mana__ from Displacement, gaining power through the corrective measures involved in covering up paradoxes in the timeline as it mends itself from disruptions caused by sending a being backward in time.
        """
    }


nymph : Details
nymph =
    { name = Nymph
    , tank = Low
    , affinities = [ Water, Life ]
    , charge = High
    , content = """
        Nymphs have very fine scales similar to the Naiad, but without the larger scales, only the fine scales that cause a slight glitter in the light, and counter-intuitively make their skin feel a little more silky in the spots where they’re present. The obvious difference would be the presence of slender fin-like horns that give them an intuition towards what a person desires. Nymphs age 10x faster than humans, but water washes away age on contact to roughly the equivalent of a human’s 18, more youthful than most witch types get. They do not require any nutrition to sustain themselves, instead relying only on an inherent ability to imbue water with mana to instill it with arcane nutrients, giving it a milky look. A dead nymph can be restored to life through physical intimacy based on the desire, like True Love’s Kiss for example, among others.

        __Nymphs draw Mana__ from Desire, whenever desire is directed at them, or instigated by them, whether it’s flirting or a good sales pitch for a new car, proportional to intensity and distance.
        """
    }


gorgon : Details
gorgon =
    { name = Gorgon
    , tank = Med
    , affinities = [ Beast, Necro ]
    , charge = Low
    , content = """
        Gorgons are known for the snakes growing in or replacing their hair, and some may or may not have bodily scales ranging from fine to large scales at their backs or around joints. Gorgons don’t choose a tertiary affinity, but from Earth, Nature, Water, or Metal, they determine the nature of the gorgon’s petrifying gaze. Any type of Stone, Wood, Ice, or Metal up to the rarity of gold. Unless protected by some means such as a Warding rune, any creature that directly sees the Gorgon’s eyes becomes petrified to the chosen element until dispelled by some means Petrified creatures are in suspended animation with no needs, but are aware and can feel. Gorgons do not age so long as they have a humanoid in stasis. A slain gorgon self-petrifies and slowly self-repairs until whole at a rate of a papercut per minute. Once whole, they resurrect, cracking to reveal a new body equivalent to an 18 year old, unless they were under.

        __Gorgons draw Mana__ from Petrification, while a creature is petrified their lost time passively provides the Gorgon with energy.
        """
    }


luxal : Details
luxal =
    { name = Luxal
    , tank = Med
    , affinities = [ Metal, Life ]
    , charge = Med
    , content = """
        Luxin have an associated metal the way an Oread or Siren have an associated animal. Their bones are formed of this metal, and they can have random metal growths visible on their skin or growing from their body such as a visible metal plate over their sternum, or dotted along their spine, or growths like horns or crowns. Their irises are literal gemstones that match the large gemstone that forms their heart equivalent. They don’t bleed or burn. Metals melt like butter in their mouth, which they eat in addition to normal food. So long as they eat metal, they don’t age for the day. Age can be regressed with overconsumption of metals. A dead Luxal can regrow from their gem heart if submerged in a molten pool of their associated metal, emerging whole as though aging from 0, gaining 1 year per hour spent submerged up to 24.

        __Luxin draw Mana__ from Opulence, passively charging from the presence of “wealth” though it has nothing to do with monetary value, but precious metals and stones amplified by skill in working them.
        """
    }


kekubi : Details
kekubi =
    { name = Kekubi
    , tank = Med
    , affinities = [ Fire, Body ]
    , charge = High
    , content = """
        Kekubi look like living shadows having elemental bodies composed entirely of concentrated pitch black ash, soot, and smoke, which can emanate from them in response to negative thoughts or emotion. Any magic they use can reflect this, being reskinned into black particulates. Such blackened magic is 10% more effective. This soot emanation can cause a bother with blackening things they touch if they aren’t under control. Damage to their bodies is superficial (No critical damage) but comparable overall damage can disperse their body causing death, they can remotely operate severed limbs within a 20m radius. They age like elves, and if slain they crumble into ash to reform over 24 hours if the ashes aren’t scattered, or within 24 hours of at least 50% of their ashes being recombined

        For some reason, if they are close to a Doll, they can form Magic Friendship free.

        __Kekubi draw Mana__ from Immolation, burning their own body as though they were coal, harmlessly resting on flames with their body absorbing smoke
        """
    }


sylph : Details
sylph =
    { name = Sylph
    , tank = High
    , affinities = [ Wind, Soul ]
    , charge = Low
    , content = """
        Sylphs might look normal at a glance, the more slender frame can be excused, and the pointed ears are no big deal, but then their hair is perpetually floating ethereally, being weightless, as is their body overall. Slight gusts become significant issues, which Sylphs combat by focusing to partially etherealize, straddling the line between the material and spirit world, unaffected by the wind and invisible to those that can’t perceive spirits. They can will themselves to move omnidirectionally, but can’t phase through matter. They don’t age, remaining around 18 equivalent. A dead sylph’s body disappears leaving the spirit in place, which crystallizes into a cocoon of spirit matter. They age a year per day spent in the crystal, from 0. It will shatter on its own when they reach 18.

        __Sylphs draw Mana__ from Spirits, being near high concentrations of naturally occurring spirits or spirit matter, places with history and tragedy. The site of mass graves or terrible battles or old ruins of a building with enough importance or meaning that it left spirit matter echo in the spirit world.
        """
    }


undine : Details
undine =
    { name = Undine
    , tank = Low
    , affinities = [ Water, Body ]
    , charge = High
    , content = """
         Undine are are comparable to the Ifrit, with elemental bodies entirely composed of water, concentrated into a gelatinous form that can feel like a normal body to the touch, although slick as though oiled lor wet, but if they lose focus their form becomes more and more liquid on a sliding scale. They have a preferred true shape for their appearance, but they can freely focus to adopt any shape with the same volume, though they can separate to reduce their volume or absorb water to add volume up to the volume of a whale. Their water body has all normal senses. They don’t age and have a clear marble as a core. Damage to their body is superficial, but the core can be shattered, without an inherent means of revival if that happens.

        __Undine draw Mana__ from Purification, where water based substances that come in contact with an Undine are purified of other elements and contaminants, leaving pure water with trace minerals. Can’t help with anything larger than a pea, but does eliminate sand or dirt as well.
        """
    }


sprite : Details
sprite =
    { name = Sprite
    , tank = Med
    , affinities = [ Nature, Life ]
    , charge = Med
    , content = """
        Sprites typically denizens of the faewild, Sprites are inherently tiny, about an inch tall, and have an associated winged insect that skews their appearance. All Sprites have wings while a wasp sprite might grow a smooth chitin plating covering their back and limbs to one extent or another. Whatever the insect, all Sprites can produce both silk webbing and honey, and both can be combined to produce hive materials. They have retractile stingers in their wrists that can inject a paralytic venom, which when combined with their honey can form a thick wax that firms up when worked like dough. Bathing in their honey reverses and prevents aging around prime. A dead sprite can be buried in a honey to revive in 3-7 days based on severity.

        __Sprites draw Mana__ from Fermentation, of their honey. The traditional method being to store cells made of their own wax and silk full of honey for 7 days, becoming like a mana potion restoring 15% of their mana capacity per drop (About 2 cups equivalent). Sprites produce 1 Tbsp a day.
        """
    }


empusa : Details
empusa =
    { name = Empusa
    , tank = High
    , affinities = [ Blood, Necro ]
    , charge = High
    , content = """
        Empusas are a form of vampire born in intense negative emotion and torment. Unlike other witch types, a witch Empusa is frozen in their prior mortal forms, though the witch power still results in male mortals becoming female counterparts if they don’t have Elephant Trunk, as well as being more physically fit. They don’t age, frozen at the age they Awoke, their bodies cold and lifeless with no biological function other than optional lung capacity for scent and speech. Their eyes have an unnatural glow, the color based on the witch’s individual aura, and they have slender hollow fangs with which they suck blood They store up to 5x their body weight in blood without any visual bloat, and are 100% as fast per 1x bodyweight of blood stored. A dead Empusa can be soaked in blood to reanimate them within a minute.

        __Empusas draw Mana__ from Blood, draining the equivalent of one whole human to death fully charges their mana. Animals are a tenth as effective. An Empusa relies on their mana and will die if fully depleted but their charge also mends their body
        """
    }


lilin : Details
lilin =
    { name = Lilin
    , tank = High
    , affinities = [ Fire, Mind ]
    , charge = Low
    , content = """
        Lilin witches can trace their way back to infernal lineages- to a family line that escaped or earned their way out of hell. They have different types of horns and leathery wings, and tail(s) in different styles, and skin tinted some unnatural color. Markings on their body brand them as free demons. They have a second set of transparent eyelids that give them thermal vision, and they can’t be burned, entirely comfortable lounging in or on lava flows. They do not age, and if slain they do find themselves back in hell to serve time for any misdeeds, but anyone who knows their name and the marking on their body can summon them with Consortation 3. If summoned, they don’t have to return when dismissed, but summoners can include a pact they have to accept if they want to be summoned.

        __Lilin draw Mana__ from Taboo, when they draw pleasure from scenarios that they themselves believe is wrong when observed by another who also believes it is wrong. Such as seducing someone into cheating on another, or public indecency.
        """
    }


erinyes : Details
erinyes =
    { name = Erinyes
    , tank = Med
    , affinities = [ Wind, Blood ]
    , charge = High
    , content = """
        Erinyes witches can trace their Way back to celestial lineages- to a fallen angel, a past incarnation of themselves. They have boney dark feathered wings and their skin is marked with inky black stains showing where their tears fell in the process of branding them with celestial runes identifying their sin and punishment. The celestial faction may or may not be related to the celestials behind Abrahamic religions. Erinyes cease aging between 18-26 and if slain, can re-awaken from a new mortal host somewhere unless ritually sacrificed, which stops their reincarnation for a century. Erinyes have a peculiar effect wherein the last person to physically harm them receives a mirror of any further harm to the Erinyes for 24hrs.

        __Erinyes draw Mana__ from Pain, when they cause another being physical or emotional torment, amplified by vocal expressions of that suffering; ie screams or crying. Note; This could be consensual in the case of certain tastes, or a something like a scream house attraction where some are there for it Or they might even be a physical therapist.
        """
    }


hannya : Details
hannya =
    { name = Hannya
    , tank = High
    , affinities = [ Water, Mind ]
    , charge = Med
    , content = """
        Hannya are known as raging drunkards heavily addicted to alcohol, almost always blushed with at least a light buzz. They have long slender horns that also “blush” like their cheeks. This blush effect is amplified into overdrive when they experience strong passions, turning bright red when enflamed by anger, which easily applies to most combat scenarios among other things as they have a habit of working themselves up. The redder they get, the more physical prowess they have, both in strength, and dexterity, and general stamina. They actually age in reverse, but whenever they are completely drunken their age will change to represent how they imagine themselves or want to be on a whim until they become sober. A dead Hannya can be brought back to life by first mending the body, then dousing with ice cold water.

        __Hannya draw Mana__ from Alcoholism, drinking alcohol isn’t just a quirk but their primary energy source. They aren’t immune to getting drunk, but don’t experience hangovers.
        """
    }


taura : Details
taura =
    { name = Taura
    , tank = Low
    , affinities = [ Nature, Beast ]
    , charge = Med
    , content = """
        Taurai are very obvious at a glance for their large animal lower “halves” where the upper torso of a human from the hips up replace an animal head and neck, the whole body shoulders-down of another animal replacing what would be their human legs. They may or may not have animal features on their head or back as well up to the extent of being Sphinx-like. This can be any legged animal from lizard or spider to rabbit or horse. This is usually a hoofed animal but pawed animals aren’t that rare, and spider taura are fairly common in places. Their lower bodies are generally powerful and have a high sprint speed and endurance.

        __Taura draw Mana__ from Conservation, when acting in harmony with a natural state of being, which generally revolves around sustainability and continuity of life, land protecting nature or historical artifacts or buildings 60+ years old. Non-invasive improvements or protections will cause a passive mana gain for 100x the time it took to build/fix/set up. High charge while actively guarding within a 100m area.
        """
    }


wulong : Details
wulong =
    { name = Wulong
    , tank = High
    , affinities = [ Beast, Mind ]
    , charge = Med
    , content = """
        Wulong are exclusively a type of witch, associated with the asian lung dragons Wulong have jagged but usually smooth tipped horns like deer, or coral, and stained or painted-like arms up past their elbows that can look like stained glass or tie-dye, and they have long tails with a strip of soft fur ending in a tuft. Wulong are able to fly by force of will with somewhat strenuous effort equivalent to a full sprint Their blood is an ink the same color as the dominant color of their horns and arms. Wulong age like Elves, slowing from age 3 until reaching 18 at 100. A slain wulong can be reborn from a painting or statue that the wulong had formed a bond with using a drop of blood. Post-death, If someone with enough passion and knowledge of the wulong’s body creates an accurate masterwork, they can be reborn without a prior bonded work of art, from no remains.

        __Wulong draw Mana__ from Artistry, charging in proximity to works of art. Any work of art, made with creative and/or meaningful intent imbued with subjective value of the creator. Paintings, Storybooks, Statues, etc.
        """
    }


dravir : List Race -> Details
dravir races =
    let
        affinity : Affinity
        affinity =
            List.Extra.findMap
                (\r ->
                    case r of
                        Dravir aff ->
                            Just aff

                        _ ->
                            Nothing
                )
                races
                |> Maybe.withDefault All
    in
    { name = Dravir affinity
    , tank = Low
    , affinities = [ Beast, affinity ]
    , charge = Med
    , content = """
        Draviri are what happens when those princesses end up sacrificed to the dragon with no hero(ine) to save the day. Leaving the details aside, Draviri have the horns, tail, and scales of a dragon, typically having full claws on their hands and feet. Draviri pick their secondary elemental affinity, which is manifest in a breath weapon that affects a 15ft cone or 30ft line of flame [Fire], lightning [Wind], cold and ice [Water], stone/metal shrapnel (Earth) [Metal], or poison gas / thorns [Nature] For every 10 years of life, this area increases by 5ft. Their dense draconic muscle gives them strength and stamina like a Daeva Draviri age like Elves, and if slain an egg can be found within their body that will hatch the reborn dravir within a year in the right conditions, until hatched or destroyed.

        __Dravir draw Mana__ from Destruction, when they undo the work and labor that went into producing something of value based on its value and purpose to someone else. This includes the taking of life, particularly human or witch life, which briefly provides a High charge rate.
        """
    }


doll : Details
doll =
    { name = Doll
    , tank = Low
    , affinities = [ Soul, Necro ]
    , charge = High
    , content = """
        Dolls are artificial beings. If you’ve awakened into a Doll form, your soul is likely a reincarnation of a soul used in the creation of a Doll in the past but it could be exposure to experimental energies that corrupted your witch type. Dolls get their name as most Dolls are literally ball jointed dolls of wood or porcelain but dolls can also be fully organic made of sewn flesh of different body parts, others are clockwork, or recently Alphazon has been making fullsynth dolls. Black market deals in Asia have been making dolls with limited self-governance and selling them.

        Dolls don’t age or have biological functions by default, though some can have living organic parts. A doll can’t die, only cease function. If repaired then they regain function. They have all normal senses, even touch- Pain or pleasure both, but a maker can adjust these along with personality elements or even memory,

        __Dolls draw Mana__ from Service, obeying a request or command from a sapient being endows them with charge and gives them a dopamine hit comparable to sugar.
        """
    }


vanir : Details
vanir =
    { name = Vanir
    , tank = High
    , affinities = [ Water, Nature ]
    , charge = High
    , content = """
        Vanir are beings associated with winter, and hidden groves of life amid the frost. The presence of a vanir will allow plant life to thrive in the cold and while creatures will still feel cold, they won’t suffer actual harm, including themselves, though they don’t feel discomfort at all with the cold. Their bodies are blue, white, or gray emitting a cold chill, from which ice crystals form like scales, spikes, or horns.

        Vanir age normally until their apparent age freezes randomly between 18-25. A slain vanir flash freezes and shatters into a mist of ice crystals. Somewhere within the nearest tundra (such as a snowcapped mountain peak), a new body will form out of ice over 3 months, then crack to free them with new life.

        __Vanir draw Mana__ from Resistance, when acting against a natural state of being both in nature and in social dynamics, such as going against popular opinion, or resisting baser natural desires, not wearing many layers in the cold, or turning down that slice of cheesecake.
        """
    }


changeling : Details
changeling =
    { name = Changeling
    , tank = Med
    , affinities = [ Body, Mind ]
    , charge = Med
    , content = """
        Some universes only produce Witches as changelings, albeit usually under different mechanics. Changelings look like human children anywhere from 0 to 14, and are notably able to freely shapeshift in the blink of an eye to assume any other appearance that matches that description, including perfectly copying the appearance of another at a glance. A changeling cannot physically age past 14 or even use transformation magic to push past it and are biologically immortal. A slain changeling will re-awaken from a mortal human child randomly within the same plane/realm. (ie; Earth), replacing them. The replaced soul and mind will find itself in The Nursery (A realm).

        __Changelings draw Mana__ from Identity, any interactions had wherein another believes the changeling to be someone that they knew. An impatient changeling is able to sow doubt and uncertainty in order to briefly spike mana gain to High. Worth noting that changelings very often have the *Charge Swap* perk, or seek out a way to gain it. It costs 3p less for them.
        """
    }


elf : Details
elf =
    { name = Elf
    , tank = High
    , affinities = [ Body, Nature ]
    , charge = Med
    , content = """
        Elves once ruled Earth, as plentiful as modern humans currently are, with great empires of megalithic works and a global atmospheric energy grid... anyway, surviving elves come in Sun, Moon, and Sky varieties. Suns are golden-bronze skinned and favor forests. Moons are silvery-plum skinned and favor subterranean living, typically short. Sky elves are fair skinned and taller than most. Elves start out aging like humans but rapidly slow down. 100 is equivalent to a human’s 18, while they might look 30 at 1,000 and never exceed looking like 40. A slain elf will resurrect at the last Elven Lifeshrine they interacted with (Or their parent if they haven’t yet). Lifeshrines are rare on Earth, but most factions will have at least one. Also, Elves are *not* lewd, they’re a proud & noble people darn it.

        __Elves draw Mana__ from Meditation, entering a trance-like state where they look conscious but immobile with eyes glazed over. They charge 25% Mana per hour, and an hour of trance is comparable to 2 hours of deep restful sleep.
        """
    }


orc : Details
orc =
    { name = Orc
    , tank = High
    , affinities = [ Body, Earth ]
    , charge = Low
    , content = """
        Orcs were widely present along with the elves with a hostile relationship back then. Orcs come in three colors, green with tusks, red with horns, and blue with neither. Exceptions exist, and half-breeds often have very muted colors like a tint to a more typical skin tone. They typically have elf-like ears, but are known for being strongly built and large in stature with an average height of 6'8\\". They have low light vision capable of seeing in dark with the faintest light source, and can go without any air for hours, or days on thin air. Orcs have a sense for ore and fault lines. Orcs age normally, but when an orc dies they are reborn from the closest member of their family tree to rapidly age back to 20 equivalent within 3 months.

        __Orcs draw Mana__ from Birth, fueled by the growth of new life created by the orc More effective for female orcs growing it themselves, male orcs have about a tenth the same energy gain while a currently pregnant orc gains charge very rapidly then it levels out into background charge as the offspring grow.
        """
    }


pharon : Details
pharon =
    { name = Pharon
    , tank = High
    , affinities = [ Beast, Soul ]
    , charge = Low
    , content = """
        Pharon are animal-headed people that have been treated as gods in some human cultures. They have human bodies but the head and neck of an animal creature, somewhat opposite to the taura. This can be any animal including insects, if it’s an animal, it’s valid. They’ll have any abnormal features and functions the animal head would normally have, from a beetle’s mandibles to the eyes and neck rotation of an owl. They age as normal but are capable of creating ambrosia by distilling light between their hands. A Pharon consuming ambrosia will cease aging for 3 days during which time for each additional “Serving” they’ll age in reverse at an increasing rate. If slain, ambrosia can return their body to life if placed inside the body (or in the ash pile).

        __Pharon draw Mana__ from Renown, fueled by their notoriety one way or another but increases proportional to intensity of feelings. The greater the respect, fear, reverence, or even hate and so on, the higher the charge in that moment of thought. They can feel their name spoken.
        """
    }


jotun : Details
jotun =
    { name = Jotun
    , tank = Med
    , affinities = [ Body, Blood ]
    , charge = Low
    , content = """
        Jotun are a truly ancient race that reigned prior to the antediluvian elven kingdoms where they were slaves. They can grow to 15 meters in height on demand with a around a 15% mana drain, negligible drain to return to normal. Proportional strength and speed. They can remain in either size indefinitely. Jotun cease aging} at 31 for males, 21 for females, though they all share the same max height. If slain, a jotun has a second body 1/30th off their size located where a heart would be encased in a crystal-like calcium shell impervious to most things short of a rift blade. This shell cracks to free and wake the body reborn in 3 days if slain at full size, 3 weeks if slain in human size.

        __Jotun draw Mana__ from Flesh. Any meat will provide Charge but, naturally, creatures with greater souls give greater charge... humans, or more still, witches Animal meats low, human is medium, witch is high. Charge is passive while digesting. Humanoid flesh gives them a spine-tingling sense of bliss that can be addictive. leading to some unpleasantries.
        """
    }


hollow : Details
hollow =
    { name = Hollow
    , tank = High
    , affinities = [ Soul, Metal ]
    , charge = Low
    , content = """
        Hollows are a race of people that are a merger of spirit and metal. They have bodies that behave like spirits, and spirits that behave like bodies. Their physical body is invisible and intangible, but bound to a suit of armor, and casts a colored glow based on their aura. Their spirit form on the other hand, has biological processes, which is how Hollows reproduce. In their spirit form with any other spirit. Their physical form carrying the offspring will have a visible orb of metal with a will-o-wisp-like glow around it, internally within their own suit of armor which will then grow over time as it collects metals and minerals. They eat spirit matter to survive, much like a Spirit Beast, but can consume spirit objects, not just “living” spirits. The armor heals the way a body would, or with healing magics. Their spirit can remove the armor, but can only move within 30ft of it. A Hollow “Dies” if their armor is destroyed to the point of unusability, but can be brought back to life by reforging their armor if any part of it remains to incorporate in it.

        __Hollows draw Mana__ from Ore. They place raw ore within their armor where it suspends and breaks down over time, and is used for the armor’s integrity.
        """
    }


dwarf : Details
dwarf =
    { name = Dwarf
    , tank = Med
    , affinities = [ Earth, Metal ]
    , charge = High
    , content = """
        Dwarves are a short race that stands between 3-4f tall. They have larger eyes proportionally to most humanoids. They’re very long lived like elves but they stay more youthful for longer, never growing more from their youthful state until they begin wrinkling with old age after 300+ years. The main way to tell a Dwarf’s age is the length of their hair (or beard for males, & muscles, more pronounced in males). Every dwarf has an associated metal up to the rarity of gold, which grows in fine threads like hair, including the beard of male dwarves, and their bones are all high carbon steel, their muscle sinews dense with incorporated metals, making them extremely hardy and durable as though armored with bones that very rarely break, though heavier than an Orc They seem to have boundless stamina and laser focus their pursuits.

        They have low-light vision & don’t need air. A dead Dwarf given a dwarven burial (even if no body remains) can be reborn to a relative, to regain memories at age 10. Someone can volunteer at the funeral.

        __Dwarves draw Mana__ from Community, the presence of other dwarves or creatures with this charge method. Low with 1 other, Med with 6, High with 20+.
"""
    }


wither : Details
wither =
    { name = Wither
    , tank = High
    , affinities = [ Necro, Metal ]
    , charge = Med
    , content = """
        Withers are cursed people as a result of the horrific actions of their ancestors or past incarnation of themselves. They’ll be born thinking they’re Neutral but will be plagued with bad fortunes and health issues. At around age 18, they’ll die of a mysterious condition only to rise again in 3 days to a cold body with blackened limbs rotting on the bone. They’re no longer unlucky, but their undead body continually tries to rot from the extremities inward, they’d have skeletal fingers by the time their shoulders begin to blacken, over a week duration, They can delay and reverse decay through cannibalizing humanoid flesh leaving behind a skeleton, to regrow their own and remain whole for 1 week. An arm for an arm, body for body, ect. They can do this at a distance of 30ft. They have an aura of decay that makes people feel ill, and rapidly rusts metals. A nail would be dustin a minute They cannot die, they continue to reanimate every full moon unless impaled on a gold or silver spike with coins over the eyes / eye sockets.

        __Withers draw Mana__ from Rot, empowered by the presence of rotting organic matter, particularly meats, proportional to the quantity, & the sapience of the body in life.
        """
    }


mimi : Details
mimi =
    { name = Mimi
    , tank = Low
    , affinities = [ Beast, Life ]
    , charge = High
    , content = """
        Mimis are animal people much like Oreads, but lack the Earth connection, and come in two flavors of Amazons and Halflings, some subspecies being over 6ft tall while the others rarely exceed 4"8" Like Oreads, they have an associated animal that they attribute, usually mammals but it could be any natural animal. They’re almost always lighthearted people full of life and vibrance, where Oreads can often be a melancholic sort, introverts to the Mimi’s extroversion (On average). They can be impulsive and seem to naturally have improved luck hard to quantify, otherwise they benefit from typical tropes associated with their animal reference, having about 2 “Traits” (Stacks with Familiarity) other than physical characteristics which don’t count against this. Mimi’s stop aging at roughly 18 for halflings, or between 25-35 for amazons. A dead mimi can be brought back to life by erecting a shrine to them, and a tear drop.

        __Mimis draw Mana__ from Headpats, Belly Rubs, and Back Scratches, or similar forms of contact, recovering 5% of their mana capacity per second of contact. A Mimi can headpat themselves for 0.5%, but this makes them feel depressed and teary for some reason.
        """
    }


sword : Details
sword =
    { name = Sword
    , tank = Low
    , affinities = [ Metal, Blood ]
    , charge = Low
    , content = """
        Wait what? Oh I’ve heard of these before. Huh, you may be a Sword Yeah, a sentient sword and a living relic. “Swords” have a humanoid body that can visually look like any other race shown here, but they adopt a sword form that is their true form. If unconscious or slain, they’d adopt their sword form. They have no biological needs, but do age up until age 20 equivalent, their sword form starting out as a knife and will grow with their body over time into some type of sword based on their personality, Can include polearms, just swords with long handles, and axes, weird swords with long handles, or hammers... swords with very weird flat blades. Don’t ask questions, just stab. Their sword form heals over time, damage reflecting in their humanoid body. A dead sword can be brought to life by using the inert sword to take a life. In sword form, they have all their magic abilities without requiring hands, and all magic manifests in some sword related way. They can synchronize with the thoughts and intentions a wielder.

        __Swords draw Mana__ from Battle, gaining MP during conflict. Increases to Med after 2 minutes, & High after 10 minutes.
        """
    }


xeno : Details
xeno =
    { name = Xeno
    , tank = Med
    , affinities = [ Beast, Blood ]
    , charge = High
    , content = """
        Xenos are a new species only discovered in the last 10 years by an Alphazon research team on Titan. A crashed ship from outside the solar system. Normal Xenos are monstrous predators, but after killing some witches on the team, they started hatching hybrids that are more humanoid and less feral. These hybrids have established a colony. They run at 120mph, can throw cars, and have hard carapaces equivalent to mithril armor with claws and bladed tails. Remarkably quiet in movement and can see infrared and UV.

        __Xenos draw Mana__ from _Eggs_, laid by the xeno if female. Male xenos benefit from the eggs laid by their parent or eggs laid by their own mate. Each egg individually provides about 1% total mana per minute af fa range of 500 meters, .5 for an additional 500, .1 in another 500. Stacks. Eggs can’t be fertilized after being laid, remaining as mana batteries, but are fertilized during formation like the sexual reproduction most witches are familiar with. An egg lasts 1 month before running dry, unless supplied blood. 1 drop = 1% charge."""
    }


cyborg : Details
cyborg =
    { name = Cyborg
    , tank = Med
    , affinities = [ Body, Metal ]
    , charge = High
    , content = """
        Cyborgs can be simulated a number of ways but this Cyborg type is a particular instance and a common type of witch back on Terra Prima, combining flesh and synthetic components. Cyborgs don’t age past their appearance, and a dead cyborg will have a small backup chip that can be inserted into a new cyborg body. Cyborgs can have up to 5 mundane items integrated into their body for use in some manner.

        __Cyborgs draw Mana__ from Electricity, using internal reactors, typically in the form of fuel pellets, little beads any alchemist can learn to make using stardust and a potion of stamina of any rank. R1: Restores 1% mana capacity per minute for up to 24 hrs. R2: 2%. R3:5%. R4: 10% per 30 seconds for 1 hour. R5: 25% per 10 seconds for half an hour. A cyborg with no fuel pellet will feel a hunger-like sensation

        They can of course tap into any power source to drain electricity at the rate at which the system is rated for, such as 1,50 watts for an outlet, which can charge 5% per minute. Cyborgs are not immune to electrical damage from unregulated attacks.
        """
    }


spider : Details
spider =
    { name = Spider
    , tank = Med
    , affinities = [ Beast, Metal ]
    , charge = High
    , content = """
        A species of *Manaweaver* spiders that can produce witches, but usually it’s humans who awaken and tum into one. The size of a human hand, they can produce a foot of semi-metallic webbing per minute as thick as yam, their bites can paralyze any creature not magically protected, they climb walls as you’d expect. They age comparable to a human but can cocoon themselves for 48 hours to emerge youthful again, between 12-24 depending on how long they were cooking in there. A dead Spider witch can assume direct control over the body of any unintelligent children they’ve created by laying eggs in the hundreds, only creating intelligent spiders if they reproduce sexually if you must know.

        __Spiders draw Mana__ from Bondage. Look, I know how that sounds and that’s not wrong either, but so long as a spider has a human or supernatural creature bound in their webbing, they produce charge similar to a gorgon’s stash of statues. Bound targets each provide about 1% capacity per min. A willing participant provides 5%. If a bound target is killed, it provides an instant +50%.
        """
    }


gnome : Details
gnome =
    { name = Gnome
    , tank = Med
    , affinities = [ Nature, Metal ]
    , charge = High
    , content = """
        Gnomes are tiny inventive people that even in the wilds in history they were a productive and innovative people relying heavily on cunning works of primitive engineering to construct safe communities and traps to deal with a world of far larger threats, working with trees, roots, and earth. Now they’re natural at working metals into complex contraptions. They stand only 4 inches tall but have the strength of a typical humanoid child, impressive for their size, and the speed of a cat with strong limbs and durable bodies that are near immune to blunt damage

        __Gnomes draw Mana__ from Tinkering, any time they create anything, they gain mana from doing so proportional to the material value, skill necessary, and time required. Jury rigging a quick contraption in a minute with a practical function and purpose could net you as much as 15% of your mana capacity. A long project that takes 4 hours could be a full restore, or 5 minutes with high skill, value, and risk of failure. Major projects taking a week to build can double their capacity for a week."""
    }


pixie : Details
pixie =
    { name = Pixie
    , tank = Low
    , affinities = [ Nature, Wind ]
    , charge = High
    , content = """
        Pixies are fae related to the Sprites, associated with plants where Sprites are associated with a manner of animal life. They can have wings like a sprite, or they can have leafy wings in any shape or color a leaf can have, though it may be thin to the point of transparency. Otherwise, pixies look like humans, elves, or dwarves but on a tiny scale, standing around an inch tall. They have a dryad-like ability to take on the form of any flower they’ve planted that has bloomed, and the ability to take pollens, seeds, or related traces of any two different flowers to breed a hybrid flower. Pixies flatly do not age, they merely grow from a baby size, to their adult size of around an inch. A slain pixie can be reborn from any flower they have planted.

        __Pixies draw Mana__ from Blooming, whenever a flower created by the pixie blooms or a flower that the pixie has slept in the petals of, blooms then the pixie receives mana. As such, pixies tend to manage gardens of many different types of flowers, or in the wild range far to claim or plant them in fields or forests.
        """
    }


fairy : Details
fairy =
    { name = Fairy
    , tank = High
    , affinities = [ Soul, Mind ]
    , charge = Low
    , content = """
        Fairies are fae related to the Sprites, but without either the flora or fauna association. They can have wings like a sprite, or they can have wafer wings of light transparent or opaque, that can emit a faint glow or light like a torch obscuring the fairy’s body. Otherwise, fairies look like humans, elves, or dwarves but on a tiny scale, standing around an inch tall. Fairies have the ability to on touch induce euphoria and bliss in creatures, filling them up with either a happiness that compels them to dance and sing, or a carnal impulse that compels. them to indulge vices of the body. Creatures so affected are temporally locked with no sense of time, and don’t age. Fairies flatly do not age, they merely grow from a baby size, to their adult size of around an inch. A slain fairy can reincarnate at any still ongoing revelry they started.

        __Fairies draw Mana__ from revelry, whenever a creature of human intelligence or greater is locked in a fairy’s revelry, they gain charge over time. 1 human in revelry for 8 hours will charge a fairy’s mana capacity by around 25%. They stack.
        """
    }


genie : List Race -> Details
genie races =
    let
        affinity : Affinity
        affinity =
            List.Extra.findMap
                (\r ->
                    case r of
                        Genie aff ->
                            Just aff

                        _ ->
                            Nothing
                )
                races
                |> Maybe.withDefault All
    in
    { name = Genie affinity
    , tank = High
    , affinities = [ All, affinity ]
    , charge = Low
    , content = """
        Genies are avatars of raw magic. They have the [???] type, meaning [???] types are double discounted and rounded down. All Genies then can pick any one affinity as their secondary type which heavily influences their appearance in spirit form. Their spirit form looking like a humanoid elemental of their chosen type from 1 inch to 30f tall. All genies then have a physical form based on any other witch race, as though using Hybridize to acquire it, but they can’t gain the type perks or their form of cheating death. Genies all have rank 2 in every core & faction magic, and Prestidigitation & Conjuration free, used personally, not via Mammon, & costs nothing if used to satisfy a Master’s wish Genies do not age. A slain genie returns to her Vessel, see type perk.

        __Genies draw Mana__ from Wishes, whenever any person says "I wish", and the genie is capable of satisfying that wish with her available abilities, the genie gains a low mana charge for the next hour. If her master is the one to wish, she gains her full mana capacity for meeting the wish.
        """
    }


gemini : List Race -> Details
gemini races =
    let
        affinity : Affinity
        affinity =
            List.Extra.findMap
                (\r ->
                    case r of
                        Gemini aff ->
                            Just aff

                        _ ->
                            Nothing
                )
                races
                |> Maybe.withDefault All
    in
    { name = Gemini affinity
    , tank = High
    , affinities = [ Earth, affinity ]
    , charge = Low
    , content = """
        Gemini are split soul beings that inhabit two bodies as one synchronous whole. The bodies are always nearly identical. Geminai have bodies composed of an associated gem associated with an Affinity, ruby fire affinity for example. Each of the pair have a different gem. This gem influences the coloration of parts of their body besides their skin, though they can have gem protrusions through their skin, which is still soft like flesh on the surface. Each half of the Gemini spends Power separately on its own effects, evenly splitting their power total between them. Gemini are born looking around 8 and age 1 year per 10 for around 80-120 years before they stop aging entirely.

        A dead Gemini half will fade into stardust, but will reappear when the surviving half sleeps, waking in the other half’s arms.

        __Geminai draw Mana__ from Pairing, the more in synch the two halves are in mind, intent, and appearance, the more mana they generate proportional to distance to each other. They feel this charge rate and are uncomfortable when it is weakened.
        """
    }


phlegethon : Details
phlegethon =
    { name = Phlegethon
    , tank = Med
    , affinities = [ Fire, Blood ]
    , charge = Med
    , content = """
        In a desert, Phlegethontes look entirely human. In humid places, though, their skin steams softly, and in the cold, their flesh gradually goes clear, showing their veins, which seem to flow with liquid fire, and their eye sockets, which have roaring candle flames behind the eyeballs. Cutting them releases the burning blood. They have a sixth sense for other people around them, the heat of their breath and beating of their blood standing out to a Phlegethon like a spotlight, though they can't distinguish individuals well.

        On death, the flames from their blood spread explosively, and cannot be quenched without magic. If the flames take a life, or an intact heart is fed into them, the Phlegethon will reconstitute themself.

        __Phlegethontes draw Mana__ from Crowds. Phlegethontes charge when in the presence of many people. They can gain a trickle from as few as two others, but only reach their full rate when a half-dozen people are nearby. If there are two dozen mortals or a hundred witches or other magical people within a hundred foot radius, they charge at a High rate.
        """
    }


moorwalker : Details
moorwalker =
    { name = Moorwalker
    , tank = Med
    , affinities = [ Earth, Wind ]
    , charge = Low
    , content = """
        Moorwalkers look like rough-hewn statues with edges worn soft by centuries of wind. When looking closely at them while they're sitting still in calm air, a careful observer can notice that their hair, eyes, and fingernails are not all there - they're translucent air. This is also noticeable with skin contact, where it will always feel like they 'just missed you' and a faint breeze will touch your skin. (For this reason they avoid handshakes with mortals, where the 'missed' feeling stands out.) Gusts of wind never seem to affect moorwalkers as much as things around them, though this is subtle until the winds reach gale force.

        If there is ever no sapient person within ten miles of the spot a moorwalker died, or within ten miles of the largest piece of their bone remaining, they resurrect instantly on the spot.

        __Moorwalkers draw Mana__ from Solitude. Moorwalkers gain mana only when at least one hundred feet from any other person. This increases if there are no animals either (bugs and smaller excepted), and with the distance - if a mile from any other people, production increases to Medium. However, this increase is negated if they're not close to solid land and their rate gets even slower if not in atmosphere.
        """
    }


phantasm : Details
phantasm =
    { name = Phantasm
    , tank = High
    , affinities = [ Wind, Mind ]
    , charge = Med
    , content = """
        Phantasms are, in their natural state, completely invisible and intangible; a free-floating mind, not tethered to any physical form whatsoever. Even the air around them is completely permeable. For those who were born human, this tends to make them habitually touch-starved, especially before they get used to their aerokinesis and the tactile sense it can give them when they have mastered it enough to be automatic. Somewhat more difficult is mastering the art of creating a visual body with the shimmering of the air. This always leaves them looking ghostly, usually like a transparent image of (a slightly idealized version of) their mortal form, but it is largely an illusion.

        Not even the vacuum of space is dangerous to a Phantasm; they have no physical form. At worst, direct attacks on their mind can render them comatose, at which point they become nigh-impossible to locate and therefore recover freely.

        __Phantasms draw Mana__ from Voyeurism. Phantasms gain mana by observing events without the target's knowledge. This charges faster if they are learning secrets that the one under observation would wish to protect.
        """
    }


golem : Details
golem =
    { name = Golem
    , tank = High
    , affinities = [ Earth, Mind ]
    , charge = Low
    , content = """
        A golem is a rock tricked into thinking. Unlike other thinking rocks such as computers, this is done by inscribing particular words on fine paper with magical ink, and forming a clay brain around them. Golems are not initially well defined, looking approximately like an artist's posable figurine made of clay, but they can slowly shift their appearance, and most adult golems look like swarthy humans. They are extremely durable and resilient, and can shut off pain easily. The only damage which is truly serious is things which manage to damage the animating inscription. This normally requires destroying most of the golem's head, which does not otherwise impair them.

        If the animating scroll is destroyed, a golem ceases to function. Rewriting the scroll precisely, with a particular golem in mind, and placing it in a new clay body, will resuscitate the dead golem. If not quite correct, this may create a new person instead, or the golem may not animate.

        __Golems draw Mana__ from Writing. Golems charge through the act of writing, cataloging facts and observations about the world. This must be hand-written, in ink on paper (including parchment, papyrus, etc.). The importance and relevance of the information is not important, but it must be true or believed to be true; fiction is not acceptable.
        """
    }


muspel : Details
muspel =
    { name = Muspel
    , tank = High
    , affinities = [ Earth, Fire ]
    , charge = High
    , content = """
        Muspel are lava wrapped in stone wrapped in flesh, and must be careful not to move too quickly lest one of the layers rupture, which cause damage that can be healed quickly, but not instantly. Almost no external force can cause similar damage, even to the outermost layer; Muspel are monstrously tough, and only damage themselves because they are also monstrously strong. They have an affinity for the sun, always knowing precisely where it is relative to them, even at night; they find realms without a true sun, such as much of Alfheimr, very disconcerting.

        Muspel whose core is destroyed slowly reform at the edges of the nearest active lava flow. It takes three days to form the stone layer, and three weeks for the flesh.

        __Muspels draw Mana__ from Annihilation. Muspel gain mana when destroying all record of some knowledge. This can be trivial knowledge, as long as it is not new and is having its last record destroyed. Old Muspel make a habit of recording trivial facts such as series of die rolls, so that they may annihilate it a decade later to charge.
        """
    }


dictum : Details
dictum =
    { name = Dictum
    , tank = Med
    , affinities = [ Mind, Metal ]
    , charge = Med
    , content = """
        When they Awake, Dicta look like metallic-ceramic composite androids of their previous form. They are not actually robotic or technological, but purely magical: their form is a symbolic reflection of immutable Order. As they age, their appearance changes at 10% the rate of humans, but they gradually grow larger, adding 1% of their current height per year. Their natural strength is merely a very fit human's, but it grows proportionately to their body weight, so an old Dictum will be quite strong.

        The changes of appearance with age are cosmetic, not impairing. If killed, after ten million seconds they will wake in a staging area on the border of the World of Order and Earth, at the age and size they were when they Awoke.

        __Dicta draw Mana__ from Promises. The more often it comes up and the greater the impact, the more it charges passively. Breaking a promise drains a substantial fraction of its total production instantly, and decreases production for a long time.
        """
    }


qareen : Details
qareen =
    { name = Qareen
    , tank = Med
    , affinities = [ Fire, Water ]
    , charge = High
    , content = """
        A Qareen is a very mutable being, usually imitating flesh and blood but fundamentally made of heat and mist, not solid. They are heavily influenced by primal Chaos, and harming them is difficult because the reaction of their body is inconsistent and unpredictable even to themselves. They have a natural affinity for illusion and shape-changing; since their natural form is inconsistent, they may as well direct the change to resemble someone else. They can never keep this up for long without other magic, though, as Qurana are naturally unstable.

        If destroyed, a Qareen will awake in the borderlands of the World of Chaos and Earth some time later. 80% of the time this will be a year or less, but the duration is random.

        __Qurana draw Mana__ from Risk. Whenever a Qareen puts something important to them at risk from random chance or natural forces (not from intentional harm from another person) they gain mana. The greater the importance the greater the charge. Whether or not the stakes are won or lost makes no difference; a lucky Qareen may gain a lot of mana without losing anything important.
        """
    }


rusalka : Details
rusalka =
    { name = Rusalka
    , tank = Low
    , affinities = [ Necro, Water ]
    , charge = High
    , content = """
        Rusalki form in bodies of water, usually rivers, where much death has occurred. Opinions vary on whether they are forgetful ghosts in watery bodies or new souls anchored to death. Their hair is long, algae-covered, and perpetually growing, and cutting or styling it is physically painful to the rusalka. Rusalki have an intuitive sense for the greatest regrets and grievances of spirits around them, and can learn to sense these from the living as well. They cannot breed true without drowning the father, but their spontaneously generation is often near others.

        A dead rusalka's spirit migrates to the nearest fresh water, where it will regain physical form the next time someone drowns, consuming the soul of the drowned.

        __Rusalki draw Mana__ from Hair. The longer and messier a rusalka's hair, the more quickly she will generate mana. A ponytail (the neatest that is painless) will generate a trickle if at least four feet long, as will a messy tangle cut to one foot; six feet of tangle is the 'standard' High level.
        """
    }


lares : Details
lares =
    { name = Lares
    , affinities = [ Blood, Life ]
    , content = """
        Also derogatorily called Brownies, Lares are the protector spirits of old families. Most Laral lineages originate with a very old family of Neutral witches, and in these cases at least some of the family remain associated with their originators and their homes. A Lar bonded to a family and residence passively repairs their surroundings by their presence, and has an extra sense that tells them when the building or family is damaged or in danger. Most have some talent for moving quickly that they use to come to the defense of their charges, and these are more effective than usual when they do.

        A Lar who dies bonded will be roused back toward life every time a fire is lit in the home or family's hearth. A dozen or two fires will see them awakened, and they will appear the next night while the hearth is unobserved.

        __Lares draw Mana__ from Maintenance. A Lar charges by taking care of chores and maintenance of a residence, particularly while unobserved. They can also charge from performing first aid and nonmagical medicine on their bonded family, but this is hard to do unobserved. While observed, their charge rate drops to Medium.
        """
    , tank = Med
    , charge = High
    }


firebird : Details
firebird =
    { name = Firebird
    , tank = Med
    , affinities = [ Fire, Beast ]
    , charge = High
    , content = """
        Firebirds, or Phoenixes, are feathered all down their arms, back, and legs in shades of red and gold, and their arms have the structure and strength of real wings, though with careful clothing choice they can appear human to a casual inspection. They are not constantly aflame, but the faster they move, the more the feathers become flames. This generates natural thermals, so with their light bones, they can fly under their own power.

        Of all natural methods of immortality, theirs is probably the hardest to foil. On death, they ignite and leave a torso-sized egg that will in a matter of days hatch into themself again, but if this egg is destroyed, it will reappear instantly elsewhere, appearing further and further away, and hatching faster and faster, if it is destroyed repeatedly. Even trapping them in stagnant time or petrification will not stick, causing them to temporarily shatter the effect in a burst of flame.

        __Firebirds draw Mana__ from Conviction. Whether their own strong confidence in what they are doing, or the feeling of certain rightness in others, a phoenix charges when those near them or closely tied to them experience strong confidence in the moral rightness of their cause or actions. Whether those beliefs are correct is entirely irrelevant.
        """
    }


fresco : Details
fresco =
    { name = Fresco
    , tank = High
    , affinities = [ Life, Soul ]
    , charge = Med
    , content = """
        Frescoes are one of the oddest races behind the Veil, because they are, for the most part, two-dimensional. They have limited telekinesis that provides a tactile sense and fills space, but even Fresco witches walking in public must wear cloaks made of a single large piece of cloth so that they can keep their body on the cloak and project out with telekinesis to fill the space under it and keep it supported in the correct shape for a human. Frescoes usually form from memories; reborn souls who either were present for widely-remembered events or who were remembered themselves. Positive memories are more likely to cause Frescoes, and far more likely to awaken as witches, with the weight of the collective good regard granting them their elemental affinities.

        A Fresco is not dead while its origin is still remembered. To rebind it to the world, inscribe or illustrate the memory that formed it onto a wall; it will reshape itself into the form of the Fresco in a matter of hours.

        __Frescoes draw Mana__ from Teaching. Whether leading a classroom, tutoring, or demonstrating a skill actively, Frescoes charge mana by imparting knowledge to others. Accuracy not required, but honesty is, and inattentive students charge slower.
        """
    }


silverstream : Details
silverstream =
    { name = Silverstream
    , tank = High
    , affinities = [ Water, Metal ]
    , charge = Low
    , content = """
        Looking like living mercury sculptures, silverstreams can be as hard and rigid as metal or as flexible and elusive as water. They have a consistent total mass and have difficulty adopting hollow shapes or non-humanoid body plans, but they're extremely flexible. Losing parts of their body is annoying but not crucial; they will reattach if they recover the stream-silver or slowly regrow it back up to their full normal mass. Silverstreams have little trouble maintaining a consistent shape, and can slowly change their default, but making it move like a human is trickier.

        A drop of dead stream-silver in still water or on tarnishable metal will slowly convert it to stream-silver over the course of a week for a full body mass. Smaller amounts will need help transferring to new seed matter.

        __Silverstreams draw Mana__ from Avoidance. Evading consequences for their actions or refusing to deal with something they need but do not want cause silverstreams to gain mana. This stacks. Dodging pain or injury give brief spikes of high gain.
        """
    }


revenant : Details
revenant =
    { name = Revenant
    , tank = Med
    , affinities = [ Body, Necro ]
    , charge = High
    , content = """
        Revenants are not alive, though they look very much like humans with slightly pallid coloring; they have no internal organs, their forms never change, and they repair damage rapidly if it does not destroy them, restoring their full form. They reproduce only by guarding intimate partners (or others with a strong physical connection) through their own deaths.

        Revenants are very difficult to kill. If they are and their body is laid in the grave with someone who died cursing death, they will revive after about a tenth of the time since the deceased died.

        __Revenants draw Mana__ from Endurance. Revenants gain main by subjecting themselves to things they dislike and remaining in their presence without acting to stop them. Taking up annoying fiddly hobbies or exercising to the point of pain are popular methods.
        """
    }


petrichor : Details
petrichor =
    { name = Petrichor
    , tank = Med
    , affinities = [ Earth, Blood ]
    , charge = High
    , content = """
        Petrichor lineages are born from places where gods bled. Their forms resemble clay statues of the wounded god, and missing limbs are particularly common. They all have weak connections to their divine 'ancestor' and senses for things pertaining to their domains. Treating ordinary clay with sapient's blood allows them to repair the damage to their forms, but this breaks their divine connection, immortality, and charge if taken too far.

        A destroyed Petrichor reforms slowly from their ancestral creation point, over the course of a year. Clay gardens tended by the lineage can serve as a replacement location, at most one per world.

        __Petrichors draw Mana__ from Maiming. Impairment from missing limbs or similar durable injuries gives a Petrichor charge. Any prothesis (like a peg leg) reduces it, and a full-replacement one like the artifact drops it to a trickle.
        """
    }


title : String
title =
    "# True Form - Race"


intro : String
intro =
    """
    "This is my favorite part, so don’t zone out on me: Even if most people prefer to be a neutral it’s fun to see what other option or two a person might have. Let’s see... oh, oh. ooh" The witch shudders with quite the enthusiasm, and continues "_This is ten times_ the most options I’ve seen in another witch. _*You have so many possibilities*_! Let’s explore them... I haven’t even seen some of these before, in person _or_ in an awakening ritual. I’m a fan of #7 in particular, they’re so cute." This is the physical expression of your True Form, what you would revert to if any effect were to reveal the true nature of a shapechanger for example. Your old body was just a temporary vessel for this true form. In your change, {choice you can totally remake your body within human limits}. However, witches are female by default, but you can take the perk _Elephant Trunk_ to be male if you wish, or the _Transformation Sequence_ perk to transform into your past human vessel and back. Note with the options presented you can become another non-human species as a Witch of that species, ie; A witch can be a naiad but not all naiads are witches. Every different race of witch has a different source of Mana as well as some unique benefits and base abilities. All witches except Neutral and partially Daeva share some degree of _inherent_ life extension, though there are many other methods any witch can learn or obtain later on regardless of their race. {choice *The blue potion icon represents Mana, the yellow bolt icon represents Charge rate. _This uses a Relative system and not absolute value, don’t overthink it_*}, it’s a representation of what you can expect in comparing your witch race to an average of other witches _of your same general power level_. High mana represents high storage, & charge represents the time it takes your method to fill your mana stores though it can often scale with overexposure, and stack.

    This is very simply just to give you a suggestion for perspective on how much magic you could expect to use. High Mana means a bigger tank, but charge is how fast it is filled. So high tank might be good for going all in while a low tank but high charge might have the endurance to go for longer. This is roughly balanced by how easy the charge source is to come by and how well it can stack up on itself.
    """
