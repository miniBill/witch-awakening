module View.Race exposing (viewRace)

import Element exposing (Attribute, Element, alignTop, centerX, el, fill, height, inFront, moveDown, moveRight, moveUp, px, rgb, spacing, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Generated.Types as Types exposing (Affinity(..), Race(..), Size(..))
import Gradients
import Images exposing (Image)
import String.Multiline
import Theme exposing (gradientText, viewAffinity)
import Types exposing (Choice(..))


viewRace : Maybe Race -> Element Choice
viewRace race =
    Theme.column
        [ width fill
        , spacing <| Theme.rythm * 2
        ]
        [ Theme.blocks [] <| String.Multiline.here """
            # True Form - Race

            "This is my favorite part, so don't zone out on me: Even if most people prefer to be a neutral it's fun to see what other option or two a person might have. Let's see... oh, oh. ooh" The witch shudders with quite the enthusiasm, and continues "_This is ten times_ the most options I've seen in another witch. _*You have so many possibilities*_! Let's explore them... I haven't even seen some of these before, in person _or_ in an awakening ritual. I'm a fan of #7 in particular, they're so cute." This is the physical expression of your True Form, what you would revert to if any effect were to reveal the true nature of a shapechanger for example. Your old body was just a temporary vessel for this true form. In your change, {choice you can totally remake your body within human limits}. However, witches are female by default, but you can take the perk _Elephant Trunk_ to be male if you wish, or the _Transformation Sequence_ perk to transform into your past human vessel and back. Note with the options presented you can become another non-human species as a Witch of that species, ie; A witch can be a naiad but not all naiads are witches. Every different race of witch has a different source of Mana as well as some unique benefits and base abilities. All witches except Neutral and partially Daeva share some degree of _inherent_ life extension, though there are many other methods any witch can learn or obtain later on regardless of their race. {choice *The blue potion icon represents Mana, the yellow bolt icon represents Charge rate. _This uses a Relative system and not absolute value, don't overthink it_*}, it's a representation of what you can expect in comparing your witch race to an average of other witches _of your same general power level_. High mana represents high storage, & charge represents the time it takes your method to fill your mana stores though it can often scale with overexposure, and stack.

            This is very simply just to give you a suggestion for perspective on how much magic you could expect to use. High Mana means a bigger tank, but charge is how fast it is filled. So high tank might be good for going all in while a low tank but high charge might have the endurance to go for longer. This is roughly balanced by how easy the charge source is to come by and how well it can stack up on itself.
            
            """
        , [ [ neutral, daeva, ifrit, siren, naiad ]
          , [ dryad, oread, lamia, aurai, nymph ]
          , [ gorgon, luxal, kekubi, sylph, undine ]
          , [ sprite, empusa, lilin, erinyes, hannya ]
          , [ taura, wulong, dravir, doll, vanir ]
          , [ changeling, elf, orc, pharon, jotun ]
          , [ hollow, dwarf, wither, mimi, sword ]
          ]
            |> List.concat
            |> List.map (raceBox race)
            |> Theme.wrappedRow
                [ width fill
                , spacing <| Theme.rythm * 3
                ]
        ]


neutral : RaceDetails
neutral =
    { race = Neutral
    , tank = Med
    , affinities = [ Soul, Body ]
    , charge = Med
    , content = """
        The overwhelming majority of witches are Neutrals. There is nothing visually abnormal about them (by default). Even some witches who have the option to awaken as another race may want to choose to be Neutral for its baseline casual existence, no new dependencies or anything abnormal to consider. They're effectively the “Human” witch, but witches refer to them as neutral rather than human to distinguish them from the mundane. They age slower than humans, at half the rate, and do so very gracefully, and they tend to be more attractive than the average human, and are harder to kill, with more efficient biology to reduce inconveniences with less biological requirements than normal by 25%, and prevent dying from a papercut, or from a tiny air bubble in the wrong place.

        __Neutrals draw Mana__ from Sleep, recovering around 1% of their Mana per hour of sleep, doubling per hour. 2 hours is more impactful than two 1 hour naps for example, regaining 16% for their 5th hour for a total of 31%, then 63% total for 6hrs.
        """
    }


daeva : RaceDetails
daeva =
    { race = Daeva
    , tank = Med
    , affinities = [ Body, Life ]
    , charge = High
    , content = """
        Daeva are to humans, what huntans are to monkeys. They're peak evolution and represent the perfection of body and overflowing spark of life. Unlike the other witch types, they're unnatural for the excess humanity, not the addition of anything new. Where the others may have scales or feathers, you take normal human traits and take them further, with a body like a comic book heroine, a goddess in the flesh, free of any imperfections and basic inconveniences of mortality. They flatly don't age, forever maintaining a mature but youthful physique. The average daeva height sits around 6ft 6inches up to 8, but short daeva can happen. They're physically around twice as strong as a human male bodybuilder, without an obvious change in their body tone, and seem to have endless stamina. They feel emotions more strongly, while being in better control of them.

        __Daeva draw Mana__ from Emotions, either the presence of high emotions in others, or singular high emotions directed at them. ie; An excited crowd vs a lover are both high.
        """
    }


ifrit : RaceDetails
ifrit =
    { race = Ifrit
    , tank = High
    , affinities = [ Fire, Necro ]
    , charge = Low
    , content = """
        Ifriti are beings native to the elemental plane of fire. Their connection manifests in the most obvious manner of the hitches, as their bodies are burned away in their awakening, their body becoming a living conflagration of flame made flesh anchored and governed by a core skull. They bleed plasma and smoke while limbs severed from their core flicker away like dying flames. The fire of their bodies is slightly above room temp and does not burn, and is tangible enough to touch, feeling like normal flesh, though their hair is more gaseous and warmer. Their body is only a little transparent, enough to see the skull but not all the way through them. They inherently do not age, but can “eat” flammable fuel sources along with traditionally edible materials. Damage to the body is superficial and mended by absorbing flames, though damage to the skull must be healed by traditional means.

        __Ifriti draw Mana__ from Burning, the release of energy released by matter through buming caused by the witch.
        """
    }


siren : RaceDetails
siren =
    { race = Siren
    , tank = High
    , affinities = [ Wind, Beast ]
    , charge = High
    , content = """
        Sirens are beings of wind and music, melodies carried by the wind, and have a connection to birds. Their bodies grow leathers in a similar pattern as Nymphs grow scales, though some go further with leather plumage sufficient to be relatively modest without clothing, and are able to transfigure themselves into the form of a specific bird they're associated with and hybrid states between the two. Whatever the bird type, Sirens are all well known for melodic voices that are deeply pleasant to outright hypnotically attractive. Sirens age at half human rates, and reverse their age through nesting in cocoons of feathers that reverses a year per day. A dead siren who is still intact enough can be nursed back to life by breathing into them a few times a day for 1-3 days depending on severity.

        __Sirens draw Mana__ from Breath, whenever they take someone's breath away, when someone forgets to take a breath, when otherwise unable to breathe, or when the Siren overtly draws the breath from their lungs, within a few inches of their lips.
        """
    }


naiad : RaceDetails
naiad =
    { race = Naiad
    , tank = High
    , affinities = [ Water, Beast ]
    , charge = High
    , content = """
        Naiads are essentially mermaids. Their bodies have very fine scales that range from barely perceptible or providing a scant shimmer when hit by the light, to larger scales, typically concentrated around the back and surfaces facing away, leaving soft exposed skin facing forward fading with a gradient into scales at their shoulders and back, that glitter like precious metal or crystal coins. Naiads age normally outside of water, but when submerged in water they age backwards three times as fast, to their prime, equivalent to human at 20. A dead Naiad with a mostly intact body can be submerged in water to slowly bring them pack over a few days to a few weeks depending on severity. All naiads can focus to transfigure their legs into a long scaled tail with a fan-like fin for rapid swimming, exceeding 60mph, or 52.1 knots, on average. They absorb oxygen from water through their skin.

        __Naiads draw Mana__ from Submersion, recharging while immersed in water or rain, based on coverage and quantity.
        """
    }


dryad : RaceDetails
dryad =
    { race = Dryad
    , tank = High
    , affinities = [ Nature, Earth ]
    , charge = Low
    , content = """
        Dryads have plantlike bodies given human form, from which leaves or bark can grow. Many have branches or twigs forming horns or crowns, tiaras, and diadems, that can include berries or fruits. Dryads age normally but are able to assume the form of a tree, in which they can gain any nutrients from light and soil, and de-age at 10x the rate they'd age. The presence of a dryad prevents the growth of infectious diseases and enhances the growth of plants and creatures by 200%, growing with added Vitality. Fruits are sweeter and larger, animals are larger and healthier, etc. All dryads additionally have an internal seed the size of avocado pit. If they die, this pit will grow into a large underground pod with a radiant flower, that will regrow the dryad's body over 3 months to roughly the equivalent of an 8 year old human. They'll regain their memories over a week.

        __Dryads draw Mana__ from Nurture, from the growth of other living things directly contributable to the Dryad's effort, whether it's tending to plants, or feeding a person.
        """
    }


oread : RaceDetails
oread =
    { race = Oread
    , tank = High
    , affinities = [ Earth, Beast ]
    , charge = High
    , content = """
        Oreads have hair that is more fur-like, and they can have a fine layer of fur over their skin in a similar pattern to the Naiad's scales or Sirens fur. Like the Siren, they have an associated animal, a land mammal, from foxes or cats to deer or cow. They can morph between this and their true form on a sliding scale. Earth easily gives way for them, allowing them to burrow with ease to the extent they can 'swim' through sand, dirt- or bedrock choosing to leave a tunnel or not. Worked earth does not respond to this effect, such as concrete, or shaped stone. They age at half rate and regress to 20ish when they sleep buried in the ground. A dead oread sufficiently intact will return to life over a month if buried, based on severity. Both satyrs and foxgirls are examples.

        __Oreads draw Mana__ from Bonds, physical closeness to individuals with a close emotional bond with the witch, whether it's family, close friends, lovers, even a pet. relaxing together in contact isolated from urgency or stress, only the warmth of cuddles. A long warm hug could fill 20%.
        """
    }


lamia : RaceDetails
lamia =
    { race = Lamia
    , tank = High
    , affinities = [ Fire, Beast ]
    , charge = Med
    , content = """
        Lamiæ are like twin counterparts to the Naiad with a bit of an inversion; Lamiæ by default have the lower body of a snake comparable to the Naiad's tail but around a third+ longer, being 5-7 times the length of their human upper body. Their body itself may or may not have scales, but they usually have slitted irises and retractile fangs with a venom that induces sleep. Lamiæ can lay an egg that grows a young new body, catching their soul to hatch if later slain.

        __Lamiæ draw Mana__ from Consumption, the swallowing of live creatures up to the size of an adult man, which depending on the potency of a Lamia's distortion, leaves no visible stretching past the throat. Creatures inside passively charge the Lamia's mana based on the strength of its soul, making most animals non-viable and mortal humans the baseline. A human will charge the Lamia to full over 8 hours, at which point they'd die after growing weaker. They can be released before then. A witch would charge in 1 hour while surviving up to 24hr.
        """
    }


aurai : RaceDetails
aurai =
    { race = Aurai
    , tank = Med
    , affinities = [ Wind, Necro ]
    , charge = High
    , content = """
        Aurae are closely associated with the faewilds. Their irises are like kaleidoscopic gemstones, and have slender claw-like nails. Their voice innately carries with it a withering drain that steals a year of life per half second of exposure, which restores the Aurai's own age to around age 20, banking excess drain. Their screams can induce a horrific vision where the listener is displaced to a random point in time at least 100 years in the past, for anywhere from 1 week to the rest of their life, remaining until they find a glowing butterfly never more than 1 mile away that serves as an anchor, only to return to the moment after they heard the scream, then drop dead if slain in the past at any point. This never affects the existing timeline. When slain, they experience this vision themself and finding the butterfly resurrects them.

        __Aurae draw Mana__ from Displacement, gaining power through the corrective measures involved in covering up paradoxes in the timeline as it mends itself from disruptions caused by sending a being backward in time.
        """
    }


nymph : RaceDetails
nymph =
    { race = Nymph
    , tank = Low
    , affinities = [ Water, Life ]
    , charge = High
    , content = """
        Nymphs have very fine scales similar to the Naiad, but without the larger scales, only the fine scales that cause a slight glitter in the light, and counter-intuitively make their skin feel a little more silky in the spots where they're present. The obvious difference would be the presence of slender fin-like horns that give them an intuition towards what a person desires. Nymphs age 10x faster than humans, but water washes away age on contact to roughly the equivalent of a human's 18, more youthful than most witch types get. They do not require any nutrition to sustain themselves, instead relying only on an inherent ability to imbue water with mana to instill it with arcane nutrients, giving it a milky look. A dead nymph can be restored to life through physical intimacy based on the desire, like True Love's Kiss for example, among others.

        __Nymphs draw Mana__ from Desire, whenever desire is directed at them, or instigated by them, whether it's flirting or a good sales pitch for a new car, proportional to intensity and distance.
        """
    }


gorgon : RaceDetails
gorgon =
    { race = Gorgon
    , tank = Med
    , affinities = [ Beast, Necro ]
    , charge = Low
    , content = """
        Gorgons are known for the snakes growing in or replacing their hair, and some may or may not have bodily scales ranging from fine to large scales at their backs or around joints. Gorgons don't choose a tertiary affinity, but from Earth, Nature, Water, or Metal, they determine the nature of the gorgon's petrifying gaze. Any type of Stone, Wood, Ice, or Metal up to the rarity of gold. Unless protected by some means such as a Warding rune, any creature that directly sees the Gorgon's eyes becomes petrified to the chosen element until dispelled by some means Petrified creatures are in suspended animation with no needs, but are aware and can feel. Gorgons do not age so long as they have a humanoid in stasis. A slain gorgon self-petrifies and slowly self-repairs until whole at a rate of a papercut per minute. Once whole, they resurrect, cracking to reveal a new body equivalent to an 18 year old, unless they were under.

        __Gorgons draw Mana__ from Petrification, while a creature is petrified their lost time passively provides the Gorgon with energy.
        """
    }


luxal : RaceDetails
luxal =
    { race = Luxal
    , tank = Med
    , affinities = [ Metal, Life ]
    , charge = Med
    , content = """
        Luxin have an associated metal the way an Oread or Siren have an associated animal. Their bones are formed of this metal, and they can have random metal growths visible on their skin or growing from their body such as a visible metal plate over their sternum, or dotted along their spine, or growths like horns or crowns. Their irises are literal gemstones that match the large gemstone that forms their heart equivalent. They don't bleed or burn. Metals melt like butter in their mouth, which they eat in addition to normal food. So long as they eat metal, they don't age for the day. Age can be regressed with overconsumption of metals. A dead Luxal can regrow from their gem heart if submerged in a molten pool of their associated metal, emerging whole as though aging from 0, gaining 1 year per hour spent submerged up to 24.

        __Luxin draw Mana__ from Opulence, passively charging from the presence of “wealth” though it has nothing to do with monetary value, but precious metals and stones amplified by skill in working them.
        """
    }


kekubi : RaceDetails
kekubi =
    { race = Kekubi
    , tank = Med
    , affinities = [ Fire, Body ]
    , charge = High
    , content = """
        Kekubi look like living shadows having elemental bodies composed entirely of concentrated pitch black ash, soot, and smoke, which can emanate from them in response to negative thoughts or emotion. Any magic they use can reflect this, being reskinned into black particulates. Such blackened magic is 10% more effective. This soot emanation can cause a bother with blackening things they touch if they aren't under control. Damage to their bodies is superficial (No critical damage) but comparable overall damage can disperse their body causing death, they can remotely operate severed limbs within a 20m radius. They age like elves, and if slain they crumble into ash to reform over 24 hours if the ashes aren't scattered, or within 24 hours of at least 50% of their ashes being recombined

        For some reason, if they are close to a Doll, they can form Magic Friendship free.

        __Kekubi draw Mana__ from Immolation, burning their own body as though they were coal, harmlessly resting on flames with their body absorbing smoke
        """
    }


sylph : RaceDetails
sylph =
    { race = Sylph
    , tank = High
    , affinities = [ Wind, Soul ]
    , charge = Low
    , content = """
        Sylphs might look normal at a glance, the more slender frame can be excused, and the pointed ears are no big deal, but then their hair is perpetually floating ethereally, being weightless, as is their body overall. Slight gusts become significant issues, which Sylphs combat by focusing to partially etherealize, straddling the line between the material and spirit world, unaffected by the wind and invisible to those that can't perceive spirits. They can will themselves to move omnidirectionally, but can't phase through matter. They don't age, remaining around 18 equivalent. A dead sylph's body disappears leaving the spirit in place, which crystallizes into a cocoon of spirit matter. They age a year per day spent in the crystal, from 0. It will shatter on its own when they reach 18.

        __Sylphs draw Mana__ from Spirits, being near high concentrations of naturally occurring spirits or spirit matter, places with history and tragedy. The site of mass graves or terrible battles or old ruins of a building with enough importance or meaning that it left spirit matter echo in the spirit world.
        """
    }


undine : RaceDetails
undine =
    { race = Undine
    , tank = Low
    , affinities = [ Water, Body ]
    , charge = High
    , content = """
         Undine are are comparable to the Ifrit, with elemental bodies entirely composed of water, concentrated into a gelatinous form that can feel like a normal body to the touch, although slick as though oiled lor wet, but if they lose focus their form becomes more and more liquid on a sliding scale. They have a preferred true shape for their appearance, but they can freely focus to adopt any shape with the same volume, though they can separate to reduce their volume or absorb water to add volume up to the volume of a whale. Their water body has all normal senses. They don't age and have a clear marble as a core. Damage to their body is superficial, but the core can be shattered, without an inherent means of revival if that happens.

        __Undine draw Mana__ from Purification, where water based substances that come in contact with an Undine are purified of other elements and contaminants, leaving pure water with trace minerals. Can't help with anything larger than a pea, but does eliminate sand or dirt as well.
        """
    }


sprite : RaceDetails
sprite =
    { race = Sprite
    , tank = Med
    , affinities = [ Nature, Life ]
    , charge = Med
    , content = """
        Sprites typically denizens of the faewild, Sprites are inherently tiny, about an inch tall, and have an associated winged insect that skews their appearance. All Sprites have wings while a wasp sprite might grow a smooth chitin plating covering their back and limbs to one extent or another. Whatever the insect, all Sprites can produce both silk webbing and honey, and both can be combined to produce hive materials. They have retractile stingers in their wrists that can inject a paralytic venom, which when combined with their honey can form a thick wax that firms up when worked like dough. Bathing in their honey reverses and prevents aging around prime. A dead sprite can be buried in a honey to revive in 3-7 days based on severity.

        __Sprites draw Mana__ from Fermentation, of their honey. The traditional method being to store cells made of their own wax and silk full of honey for 7 days, becoming like a mana potion restoring 15% of their mana capacity per drop (About 2 cups equivalent). Sprites produce 1 Tbsp a day.
        """
    }


empusa : RaceDetails
empusa =
    { race = Empusa
    , tank = High
    , affinities = [ Blood, Necro ]
    , charge = High
    , content = """
        Empusas are a form of vampire born in intense negative emotion and torment. Unlike other witch types, a witch Empusa is frozen in their prior mortal forms, though the witch power still results in male mortals becoming female counterparts if they don't have Elephant Trunk, as well as being more physically fit. They don't age, frozen at the age they Awoke, their bodies cold and lifeless with no biological function other than optional lung capacity for scent and speech. Their eyes have an unnatural glow, the color based on the witch's individual aura, and they have slender hollow fangs with which they suck blood They store up to 5x their body weight in blood without any visual bloat, and are 100% as fast per 1x bodyweight of blood stored. A dead Empusa can be soaked in blood to reanimate them within a minute.

        __Empusas draw Mana__ from Blood, draining the equivalent of one whole human to death fully charges their mana. Animals are a tenth as effective. An Empusa relies on their mana and will die if fully depleted but their charge also mends their body
        """
    }


lilin : RaceDetails
lilin =
    { race = Lilin
    , tank = High
    , affinities = [ Fire, Mind ]
    , charge = Low
    , content = """
        Lilin witches can trace their way back to infernal lineages- to a family line that escaped or earned their way out of hell. They have different types of horns and leathery wings, and tail(s) in different styles, and skin tinted some unnatural color. Markings on their body brand them as free demons. They have a second set of transparent eyelids that give them thermal vision, and they can't be burned, entirely comfortable lounging in or on lava flows. They do not age, and if slain they do find themselves back in hell to serve time for any misdeeds, but anyone who knows their name and the marking on their body can summon them with Consortation 3. If summoned, they don't have to return when dismissed, but summoners can include a pact they have to accept if they want to be summoned.

        __Lilin draw Mana__ from Taboo, when they draw pleasure from scenarios that they themselves believe is wrong when observed by another who also believes it is wrong. Such as seducing someone into cheating on another, or public indecency.
        """
    }


erinyes : RaceDetails
erinyes =
    { race = Erinyes
    , tank = Med
    , affinities = [ Wind, Blood ]
    , charge = High
    , content = """
        Erinyes witches can trace their Way back to celestial lineages- to a fallen angel, a past incarnation of themselves. They have boney dark feathered wings and their skin is marked with inky black stains showing where their tears fell in the process of branding them with celestial runes identifying their sin and punishment. The celestial faction may or may not be related to the celestials behind Abrahamic religions. Erinyes cease aging between 18-26 and if slain, can re-awaken from a new mortal host somewhere unless ritually sacrificed, which stops their reincarnation for a century. Erinyes have a peculiar effect wherein the last person to physically harm them receives a mirror of any further harm to the Erinyes for 24hrs.

        __Erinyes draw Mana__ from Pain, when they cause another being physical or emotional torment, amplified by vocal expressions of that suffering; ie screams or crying. Note; This could be consensual in the case of certain tastes, or a something like a scream house attraction where some are there for it Or they might even be a physical therapist.
        """
    }


hannya : RaceDetails
hannya =
    { race = Hannya
    , tank = High
    , affinities = [ Water, Mind ]
    , charge = Med
    , content = """
        Hannya are known as raging drunkards heavily addicted to alcohol, almost always blushed with at least a light buzz. They have long slender horns that also 'blush' like their cheeks. This blush effect is amplified into overdrive when they experience strong passions, turning bright red when enflamed by anger, which easily applies to most combat scenarios among other things as they have a habit of working themselves up. The redder they get, the more physical prowess they have, both in strength, and dexterity, and general stamina. They actually age in reverse, but whenever they are completely drunken their age will change to represent how they imagine themselves or want to be on a whim until they become sober. A dead Hannya can be brought back to life by first mending the body, then dousing with ice cold water.

        __Hannya draw Mana__ from Alcoholism, drinking alcohol isn't just a quirk but their primary energy source. They aren't immune to getting drunk, but don't experience hangovers.
        """
    }


taura : RaceDetails
taura =
    { race = Taura
    , tank = Low
    , affinities = [ Nature, Beast ]
    , charge = Med
    , content = """
        Taurai are very obvious at a glance for their large animal lower ‘halves' where the upper torso of a human from the hips up replace an animal head and neck, the whole body shoulders-down of another animal replacing what would be their human legs. They may or may not have animal features on their head or back as well up to the extent of being Sphinx-like. This can be any legged animal from lizard or spider to rabbit or horse. This is usually a hooved animal but pawed animals aren't that rare, and spider taura are fairly common in places. Their lower bodies are generally powerful and have a high sprint speed and endurance.

        __Taura draw Mana__ from Conservation, when acting in harmony with a natural state of being, which generally revolves around sustainability and continuity of life, land protecting nature or historical artifacts or buildings 60+ years old. Non-invasive improvements or protections will cause a passive mana gain for 100x the time it took to build/fix/set up. High charge while actively guarding within a 100m area.
        """
    }


wulong : RaceDetails
wulong =
    { race = Wulong
    , tank = High
    , affinities = [ Beast, Mind ]
    , charge = Med
    , content = """
        Wulong are exclusively a type of witch, associated with the asian lung dragons Wulong have jagged but usually smooth tipped horns like deer, or coral, and stained or painted-like arms up past their elbows that can look like stained glass or tie-dye, and they have long tails with a strip of soft fur ending in a tuft. Wulong are able to fly by force of will with somewhat strenuous effort equivalent to a full sprint Their blood is an ink the same color as the dominant color of their horns and arms. Wulong age like Elves, slowing from age 3 until reaching 18 at 100. Aslain wulong can be reborn from a painting or statue that the wulong had formed a bond with using a drop of blood. Post-death, If someone with enough passion and knowledge of the wulong's body creates an accurate masterwork, they can be reborn without a prior bonded work of art, from no remains.

        __Wulong draw Mana__ from Artistry, charging in proximity to works of art. Any work of art, made with creative and/or meaningful intent imbued with subjective value of the creator. Paintings, Storybooks, Statues, etc.
        """
    }


dravir : RaceDetails
dravir =
    { race = Dravir
    , tank = Low
    , affinities = [ Beast, All ]
    , charge = Med
    , content = """
        Draviri are what happens when those princesses end up sacrificed to the dragon with no hero(ine) to save the day. Leaving the details aside, Draviri have the horns, tail, and scales of a dragon, typically having full claws on their hands and feet. Draviri pick their secondary elemental affinity, which is manifest in a breath weapon that affects a 15ft cone or 30ft line of flame [Fire], lightning [Wind], cold and ice [Water], stone/metal shrapnel (Earth) [Metal], or poison gas / thorns [Nature] For every 10 years of life, this area increases by 5ft. Their dense draconic muscle gives them strength and stamina like a Daeva Draviri age like Elves, and if slain an egg can be found within their body that will hatch the reborn dravir within a year in the right conditions, until hatched or destroyed.

        __Dravir draw Mana__ from Destruction, when they undo the work and labor that went into producing something of value based on its value and purpose to someone else. This includes the taking of life, particularly human or witch life, which briefly provides a High charge rate.
        """
    }


doll : RaceDetails
doll =
    { race = Doll
    , tank = Low
    , affinities = [ Soul, Necro ]
    , charge = High
    , content = """
        Dolls are artificial beings. If you've awakened into a Doll form, your soul is likely a reincarnation of a soul used in the creation of a Doll in the past but it could be exposure to experimental energies that corrupted your witch type. Dolls get their name as most Dolls are literally ball jointed dolls of wood or porcelain but dolls can also be fully organic made of sewn flesh of different bodyparts, others are clockwork, or recently Alphazon has been making fullsynth dolls. Black market deals in Asia have been making dolls with limited self-governance and selling them.

        Dolls don't age or have biological functions by default, though some can have living organic parts. A doll can't die, only cease function. If repaired then they regain function. They have all normal senses, even touch- Pain or pleasure both, but a maker can adjust these along with personality elements or even memory,

        __Dolls draw Mana__ from Service, obeying a request or command from a sapient being endows them with charge and gives them a dopamine hit comparable to sugar.
        """
    }


vanir : RaceDetails
vanir =
    { race = Vanir
    , tank = High
    , affinities = [ Water, Nature ]
    , charge = High
    , content = """
        Vanir are beings associated with winter, and hidden groves of life amid the frost. The presence of a vanir will allow plantlife to thrive in the cold and while creatures will still feel cold, they won't suffer actual harm, including themselves, though they don't feel discomfort at all with the cold. Their bodies are blue, white, or gray emitting a cold chill, from which ice crystals form like scales, spikes, or horns.

        Vanir age normally until their apparent age freezes randomly between 18-25. A slain vanir flash freezes and shatters into a mist of ice crystals. Somewhere within the nearest tundra (such as a snowcapped mountain peak), a new body will form out of ice over 3 months, then crack to free them with new life.

        __Vanir draw Mana__ from Resistance, when acting against a natural state of being both in nature and in social dynamics, such as going against popular opinion, or resisting baser natural desires, not wearing many layers in the cold, or turning down that slice of cheesecake.
        """
    }


changeling : RaceDetails
changeling =
    { race = Changeling
    , tank = Med
    , affinities = [ Body, Mind ]
    , charge = Med
    , content = """
        Some universes only produce Witches as changelings, albeit usually under different mechanics. Changelings look like human children anywhere from 0 to 14, and are notably able to freely shapeshift in the blink of an eye to assume any other appearance that matches that description, including perfectly copying the appearance of another at a glance. A changeling cannot physically age past 14 or even use transformation magic to push past it and are biologically immortal. A slain changeling will re-awaken from a mortal human child randomly within the same plane/realm. (ie; Earth), replacing them. The replaced soul and mind will find itself in The Nursery (A realm).

        __Changelings draw Mana__ from Identity, any interactions had wherein another believes the changeling to be someone that they knew. An impatient changeling is able to sow doubt and uncertainty in order to briefly spike mana gain to High. Worth noting that changelings very often have the *Charge Swap* perk, or seek out a way to gain it. It costs 3p less for them.
        """
    }


elf : RaceDetails
elf =
    { race = Elf
    , tank = High
    , affinities = [ Body, Nature ]
    , charge = Med
    , content = """
        Elves once ruled Earth, as plentiful as modern humans currently are, with great empires of megalithic works and a global atmospheric energy grid... anyway, surviving elves come in Sun, Moon, and Sky varieties. Suns are golden-bronze skinned and favor forests. Moons are silvery-plum skinned and favor subterranean living, typically short. Sky elves are fair skinned and taller than most. Elves start out aging like humans but rapidly slow down. 100 is equivalent to a human's 18, while they might look 30 at 1,000 and never exceed looking like 40. A slain elf will resurrect at the last Elven Lifeshrine they interacted with (Or their parent if they haven't yet). Lifeshrines are rare on Earth, but most factions will have at least one. Also, Elves are *not* lewd, they're a proud & noble people darn it.

        __Elves draw Mana__ from Meditation, entering a trance-like state where they look conscious but immobile with eyes glazed over. They charge 25% Mana per hour, and an hour of trance is comparable to 2 hours of deep restful sleep.
        """
    }


orc : RaceDetails
orc =
    { race = Orc
    , tank = High
    , affinities = [ Body, Earth ]
    , charge = Low
    , content = """
        Orcs were widely present along with the elves with a hostile relationship back then. Orcs come in three colors, green with tusks, red with horns, and blue with neither. Exceptions exist, and half-breeds often have very muted colors like a tint to a more typical skin tone. They typically have elf-like ears, but are known for being strongly built and large in stature with an average height of 6'8\\". They have low light vision capable of seeing in dark with the faintest light source, and can go without any air for hours, or days on thin air. Orcs have a sense for ore and faultlines. Orcs age normally, but when an orc dies they are reborn from the closest member of their family tree to rapidly age back to 20 equivalent within 3 months.

        __Orcs draw Mana__ from Birth, fueled by the growth of new life created by the orc More effective for female orcs growing it themselves, male orcs have about a tenth the same energy gain while a currently pregnant orc gains charge very rapidly then it levels out into background charge as the offspring grow.
        """
    }


pharon : RaceDetails
pharon =
    { race = Pharon
    , tank = High
    , affinities = [ Beast, Soul ]
    , charge = Low
    , content = """
        Pharon are animal-headed people that have been treated as gods in some human cultures. They have human bodies but the head and neck of an animal creature, somewhat opposite to the taura. This can be any animal including insects, if it's an animal, it's valid. They'll have any abnormal features and functions the animal head would normally have, from a beetle's mandibles to the eyes and neck rotation of an owl. They age as normal but are capable of creating ambrosia by distilling light between their hands. A Pharon consuming ambrosia will cease aging for 3 days during which time for each additional “Serving” they'll age in reverse at an increasing rate. If slain, ambrosia can return their body to life if placed inside the body (or in the ash pile).

        __Pharon draw Mana__ from Renown, fueled by their notoriety one way or another but increases proportional to intensity of feelings. The greater the respect, fear, reverence, or even hate and so on, the higher the charge in that moment of thought. They can feel their name spoken.
        """
    }


jotun : RaceDetails
jotun =
    { race = Jotun
    , tank = Med
    , affinities = [ Body, Blood ]
    , charge = Low
    , content = """
        Jotun are a truly ancient race that reigned prior to the antediluvian elven kingdoms where they were slaves. They can grow to 15 meters in height on demand with a around a 15% mana drain, negligible drain to return to normal. Proportional strength and speed. They can remain in either size indefinitely. Jotun cease aging} at 31 for males, 21 for females, though they all share the same max height. If slain, a jotun has a second body 1/30th off their size located where a heart would be encased in a crystal-like calcium shell impervious to most things short of a rift blade. This shell cracks to free and wake the body reborn in 3 days if slain at full size, 3 weeks if slain in human size.

        __Jotun draw Mana__ from Flesh. Any meat will provide Charge but, naturally, creatures with greater souls give greater charge... humans, or more still, witches Animal meats low, human is medium, witch is high. Charge is passive while digesting. Humanoid flesh gives them a spine-tingling sense of bliss that can be addictive. leading to some unpleasantries.
        """
    }


hollow : RaceDetails
hollow =
    { race = Hollow
    , tank = High
    , affinities = [ Soul, Metal ]
    , charge = Low
    , content = """
        Hollows are a race of people that are a merger of spirit and metal. They have bodies that behave like spirits, and spirits that behave like bodies. Their physical body is invisible and intangible, but bound to a suit of armor, and casts a colored glow based on their aura. Their spirit form on the other hand, has biological processes, which is how Hollows reproduce. In their spirit form with any other spirit. Their physical form carrying the offspring will have a visible orb of metal with a will-o-wisp-like glow around it, internally within their own suit of armor which will then grow over time as it collects metals and minerals. They eat spirit matter to survive, much like a Spirit Beast, but can consume spirit objects, not just ‘living’ spirits. The armor heals the way a body would, or with healing magics. Their spirit can remove the armor, but can only move within 30ft of it. A Hollow “Dies” if their armor is destroyed to the point of unusability, but can be brought back to life by reforging their armor if any part of it remains to incorporate in it.

        __Hollows draw Mana__ from Ore. They place raw ore within their armor where it suspends and breaks down over time, and is used for the armor's integrity.
        """
    }


dwarf : RaceDetails
dwarf =
    { race = Dwarf
    , tank = Med
    , affinities = [ Earth, Metal ]
    , charge = High
    , content = """
        Dwarves are a short race that stands between 3-4f tall. They have larger eyes proportionally to most humanoids. They're very long lived like elves but they stay more youthful for longer, never growing more from their youthful state until they begin wrinkling with old age after 300+ years. The main way to tell a Dwarf's age is the length of their hair (or beard for males, & muscles, more pronounced in males). Every dwarf has an associated metal up to the rarity of gold, which grows in fine threads like hair, including the beard of male dwarves, and their bones are all high carbon steel, their muscle sinews dense with incorporated metals, making them extremely hardy and durable as though armored with bones that very rarely break, though heavier than an Orc They seem to have boundless stamina and laser focus their pursuits.

        They have low-light vision & don't need air. A dead Dwarf given a dwarven burial (even if no body remains) can be reborn to a relative, to regain memories at age 10. Someone can volunteer at the funeral.

        __Dwarves draw Mana__ from Community, the presence of other dwarves or creatures with this charge method. Low with 1 other, Med with 6, High with 20+.
"""
    }


wither : RaceDetails
wither =
    { race = Wither
    , tank = High
    , affinities = [ Necro, Metal ]
    , charge = Med
    , content = """
        Withers are cursed people as a result of the horrific actions of their ancestors or past incarnation of themselves. They'll be born thinking they're Neutral but will be plagued with bad fortunes and health issues. At around age 18, they'll die of a mysterious condition only to rise again in 3 days to a cold body with blackened limbs rotting on the bone. They're no longer unlucky, but their undead body continually tries to rot from the extremities inward, they'd have skeletal fingers by the time their shoulders begin to blacken, over a week duration, They can delay and reverse decay through cannibalizing humanoid flesh leaving behind a skeleton, to regrow their own and remain whole for 1 week. An arm for an arm, body for body, ect. They can do this at a distance of 30ft. They have an aura of decay that makes people feel ill, and rapidly rusts metals. A nail would be dustin a minute They cannot die, they continue to reanimate every full moon unless impaled on a gold or silver spike with coins over the eyes / eye sockets.

        __Withers draw Mana__ from Rot, empowered by the presence of rotting organic matter, particularly meats, proportional to the quantity, & the sapience of the body in life.
        """
    }


mimi : RaceDetails
mimi =
    { race = Mimi
    , tank = Low
    , affinities = [ Beast, Life ]
    , charge = High
    , content = """
        Mimis are animal people much like Oreads, but lack the Earth connection, and come in two flavors of Amazons and Halflings, some subspecies being over 6f| tall while the others rarely exceed 4"8" Like Oreads, they have an associated animal that they attribute, usually mammals but it could be any natural animal. They're almost always lighthearted people full of life and vibrance, where Oreads can often be a melancholic sort, introverts to the Mimi's extroversion (On average). They can be impulsive and seem to naturally have improved luck hard to quantify, otherwise they benefit from typical tropes associated with their animal reference, having about 2 “Traits” (Stacks with Familiarity) other than physical characteristics which don't count against this. Mimi's stop aging at roughly 18 for halflings, or between 25-35 for amazons. A dead mimi can be brought back to life by erecting a shrine to them, and a tear drop.

        __Mimis draw Mana__ from Headpats, Belly Rubs, and Back Scratches, or similar forms of contact, recovering 5% of their mana capacity per second of contact. A Mimi can headpat themselves for 0.5%, but this makes them feel depressed and teary for some reason.
        """
    }


sword : RaceDetails
sword =
    { race = Sword
    , tank = Low
    , affinities = [ Metal, Blood ]
    , charge = Low
    , content = """
        Wait what? Oh I've heard of these before. Huh, you may be a Sword Yeah, a sentient sword and a living relic. “Swords” have a humanoid body that can visually look like any other race shown here, but they adopt a sword form that is their true form. If unconscious or slain, they'd adopt their sword form. They have no biological needs, but do age up until age 20 equivalent, their sword form starting out as a knife and will grow with their body over time into some type of sword based on their personality, Can include polearms, just swords with long handles, and axes, weird swords with long handles, or hammers... swords with very weird flat blades. Don't ask questions, just stab. Their sword form heals over time, damage reflecting in their humanoid body. Adead sword can be brought to life by using the inert sword to take a life. In sword form, they have all their magic abilities without requiring hands, and all magic manifests in some sword related way. They can synchronize with the thoughts and intentions a wielder.

        __Swords draw Mana__ from Battle, gaining MP during conflict. Increases to Med after 2 minutes, & High after 10 minutes.
        """
    }


type alias RaceDetails =
    { race : Race
    , tank : Size
    , affinities : List Affinity
    , charge : Size
    , content : String
    }


raceBox :
    Maybe Race
    -> RaceDetails
    -> Element Choice
raceBox selected { race, tank, affinities, charge, content } =
    let
        isSelected : Bool
        isSelected =
            case selected of
                Nothing ->
                    False

                Just selectedRace ->
                    selectedRace == race

        glow : Maybe Int
        glow =
            if isSelected then
                Just 0x00F3EA6F

            else
                Nothing
    in
    Input.button
        (Theme.cardAttributes glow)
        { label =
            Element.column [ height fill ]
                [ el
                    [ width fill
                    , height <| px 600
                    , Border.rounded Theme.cardRoundness
                    , inFront <|
                        el
                            [ alignTop
                            , Theme.captureIt
                            , Font.size 56
                            , centerX
                            ]
                            (gradientText 6 Gradients.yellowGradient <|
                                Types.raceToString race
                            )
                    , Background.image (Types.raceToImage race).src
                    ]
                    Element.none
                , Theme.row [ centerX ]
                    [ viewTank tank
                    , Theme.row
                        [ moveDown 2
                        , Border.width 4
                        , Border.rounded 999
                        , Background.color <| rgb 0 0 0
                        ]
                        (List.map viewAffinity affinities)
                    , viewCharge charge
                    ]
                , Theme.blocks
                    [ height fill
                    , Theme.padding
                    ]
                    content
                ]
        , onPress =
            Just <|
                Race <|
                    if isSelected then
                        Nothing

                    else
                        Just race
        }


viewTank : Size -> Element Choice
viewTank size =
    viewSize []
        Images.tank
        (List.map
            (\( r, g, b ) -> ( r // 2, g * 3 // 5, b ))
            Gradients.blueGradient
        )
        size


viewCharge : Size -> Element Choice
viewCharge size =
    viewSize [ moveRight 30 ]
        Images.charge
        Gradients.yellowGradient
        size


viewSize :
    List (Attribute msg)
    -> Image
    -> List ( Int, Int, Int )
    -> Size
    -> Element msg
viewSize attrs image gradient size =
    el
        ([ Theme.morpheus
         , Font.size 20
         , Element.onLeft <| Theme.image [ moveUp 10 ] image
         ]
            ++ attrs
        )
    <|
        Theme.gradientText 4 gradient <|
            Types.sizeToString size
