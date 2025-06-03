module Data.Magic exposing (Affinities(..), Details, all, elementalismIntro, intro, slotDescription, title)

import Generated.Types exposing (Affinity(..), Class(..), Magic(..))


type alias Details =
    { name : Magic
    , star : Bool
    , class : Maybe Class
    , affinities : Affinities
    , isElementalism : Bool
    , description : String
    , ranks : List String
    , dlc : Maybe String
    }


type Affinities
    = Regular (List Affinity)
    | Alternative (List (List Affinity))


all : List Details
all =
    nonElemental ++ elementalism


nonElemental : List Details
nonElemental =
    [ alchemy, runes, curses, hexes, witchery, familiarity, necromancy, consortation, portals, divination, aethernautics ]


elementalism : List Details
elementalism =
    [ firecalling, windkeeping, waterworking, earthmoving, naturalism, metamorphosis, psychotics, metallurgy, lifeweaving, visceramancy, arachnescence ]


alchemy : Details
alchemy =
    { name = Alchemy
    , star = False
    , class = Just Academic
    , affinities = Regular [ Water, Nature, Life ]
    , dlc = Nothing
    , isElementalism = False
    , description = """
        Or “Potions”, a classic magic employed by witches, many would say particularly iconic. I’ll provide you a large, cast iron cauldron and stirrer, plus a big electric hotplate (For modem witches with no chimneys). You can sell potions to people, but be careful to ensure that they’re seen as a mumbo-jumbo unscientific remedy by most people I recommend selling it as homeopathic natural medicine that would ruin profit margins if it were widespread. Sprinkle some extra conspiracy about how the government could kill you if they spread the secret, which isn’t entirely untrue in your case. So long as you don’t hand out more outlandish potions you should be fine.

        Potions have infinite shelf life, and usually take around an hour to make, but you can usually make multiple bottles of the same potion at once just by adding enough ingredients to the cauldron. The fine details of how each potion works are down to your imagination, else this section would be far too long. Customize your potions within reason in ways that don’t inflate the power beyond the general level shown in each rank, most witches have unique “quirks” to their potions, no two witches making the same potion will have exactly the same result: Which can be used to track the creator of a potion. Plus an overarching theme, such as roses, that influences all potions made by the witch.

        Potions can be then used as an ingredient in making any other ingested item. Including distilling it down to reduce a concentrated dose into a pill, vial, or fitting multiple doses into a dropper. A potion can be distilled with time down to a single drop. Potions start out at about 2 cups worth of liquid. Reducing takes 10 minutes per -50% volume.
        """
    , ranks =
        [ """
        With rank 1 you can make simple *Curatives, Stimulants, and Novelties*. *Curatives* can replicate the effect of any non-addictive and over-the-counter drugs such as allergy pills and basic painkillers. *Stimulants* are the same but for effects that boost performance past normal; including caffeine. Simple *Novelties* can duplicate the effects of mundane store bought items under $10, in potion form. For example, a whoopie cushion potion may cause a similar effect under a given condition, such as public speaking. Or you could make things like dyes for food, hair, clothes with vivid or natural-looking colors. For any beneficial effect you can also make a potion doing the harmful opposite. Simple potions can be made from simple grocery store items, or K10, 10 _Witch Kisses_. Effects can last anywhere from an hour to 24 hours, or instantaneous changes.
        """, """
        With rank 2, you can make the same potions as rank 1 but supernaturally boosted to 3x standard effectiveness without complications. You could create alcohol that never causes alcohol poisoning or hangovers for example, painkillers 3x better at killing pain without risk of overdosing. Your healing poultices provide boosted healing at 3x the rate without scarring, and antibiotics or disinfectant 3x more potent. You can emulate prescription pharmaceuticals as well as basic firework gunpowders.

        Basic potions can be made from the type of ingredients that are more niche store items you might have to shop around to find, or K100.
        """, """
        With rank 3 your potions are based on the conceptual intentions of the potion rather than physical properties of a mundane effect. A painkiller isn’t just blocking chemical receptors, but uses magic to target the mind’s ability to feel pain. A healing potion isn’t encouraging a natural process, but physically goes in to force the knitting of flesh and bone. Simple effects are 100% effective to the intended effect. Healing can heal any injury with enough time at a rate of a papercut per second for up to an hour per dose. Can create *Grenade* or “splash” potions that explode with a potion effect within a 10m area, or a literal explosion of force, fire, ice, acid, or electricity with the same level of damage.

        Adept potions can be made from rare special order type ingredients, or K1000. _Grenades cost 5x as much to make_.
        """, """
        Rank 4 master Curatives can cure any disease, mend any injury, and restore the recently deceased to life. Master Stimulants can boost any attribute or quality by 300% or grant perfect focus or memory, remove and prevent fatigue for up to 24hr. Aphrodisiac effects are full blown love potions connecting anyone who partakes of the same batch of potion. Master novelties can instantly and painlessly provide any cosmetic surgery effect as though it were natural and can include traits in the animal kingdom; Cat eyes, fox ears, etc. Grenade potions can now have effect area of 30m, on impact as before, or on a timer, or by pressure (Mine).

        Master potions are made from supernatural ingredients requiring witch connections in the magical worlds to acquire probably involving services or questing, or K10,000.
        """, """
        Rank 5 Arch-Curatives can restore youth and prevent aging, or instantly rejuvenate a body to peak health as an immediate full-heal, or restore the long-dead to life so long as a body is available. Arch-Stimulants can boost any attribute by 500% for up to 24hr, or perfect a skill for a month. Love potions could induce up to and including overwhelming addiction for another person or for a sensation. Arch-Novelties could completely grant a true physical redesign of a body to your precise specifications, or could cut, paste, delete, fabricate, or modify any memory. Place a memory into a potion form to share it, or make a person forget ever knowing you, or their last week, ect. Grenade potions can now affect a 300m area that stacks with concentrated combination potions and you can design them to have a propulsion aspect to them, turning them into rockets.

        Arch potions are made from very rare ingredients such as dragon’s blood, angel hair, succubus hors, flowers that blossom only once every few years, etc, note: it needs to have some reasonable thread of logic for the desired potion effect. You can’t use dragons blood for everything. A full heal might require Phoenix Feather, for example. Or K100,000.
        """ ]
    }


runes : Details
runes =
    { name = Runes
    , star = False
    , class = Just Academic
    , affinities = Regular [ Soul, Mind, Metal ]
    , dlc = Nothing
    , isElementalism = False
    , description = """
        Also known as _*“charms”*_, or _*“Enchantment”*_. A rune is a special word or phrase of power concentrated into a symbol that echos its meaning through spiritual channels that run deep into the fabric that holds reality together. Fragments of a dialect based on the First Language, possessing a sliver of that authority over the reality that we know. Simply knowing of a rune isn’t enough to utilize it, but requires practice to deeply harmonize your own contemplations to achieve understanding of that rune. To utilize runes, Witches prepare a ritual circle on which they place an item they wish to inscribe the rune onto, or placing the ritual circle on a surface around where you want to inscribe it, along with the necessary reagents to invoke the rune in question, and then place the rune on the item in some manner. Sewing, engraving, staining, or so on. The enchantment only lasts as long as the rune itself physically does, so more permanent methods of application are preferred. The ritual takes 1 minute, 10 minutes, 100 minutes, 1000 minutes, and 10000 minutes to complete, respective to rank. Rituals can be paused and continued, *and use 4x equivalent ingredients to a potion of its rank in the ritual process.* 75% of the ritual time can be passive, such as letting incense bum out, or gold dust to slowly melt into the item being enchanted, or other ritual elements not directly associated with the object, but such as performing a separate sub-ritual to harvest an ingredient or combine two ingredients into a necessary one.
        """
    , ranks =
        [ """
        You manifest the runes *Helák* (_Luck_), *Füsil* (_Fertility_), *Motdet* (_Courage_), and *Kalvÿr* (_Chill_). A _*luck*_-charmed object doubles the odds of a good outcome of an event wherein randomness is the primary factor. If used to harm, it doubles the victim’s odds of a bad outcome instead for 12 hours. A _*fertility*_-charmed object doubles odds of conception and good genes (Can guarantee witch offspring), or prevents conception. If used to harm, the inverse for 12 hours. A _*courage*_-charmed object removes the edge from fear and promotes clear thinking under high stress. If used to harm, it heightens the sense of panic and dread, and shock, for 12 hours. A _*chill*_-charmed object’s temperature maintains 0 degrees celsius while preventing state changes due to temperature. Water will remain liquid, or ice will remain solid, while in contact or within its space (A cup, or box). If used to harm, the freezing temperature of the object might be shocking and they will feel as though the air is 0 degrees for 12 hours (Mitigated by normal means; blankets, heater/fire, hot showers, ect.) *Vælbán* (_Augment_), choose one Affinity when inscribing a vælsÿn rune, and resist all forms of damage associated with it by 10% per Runes rank you use to create it. If used to harm, it instead applies a damage vulnerability on a target.
        """, """
        Manifest the runes *Sykóst* (_Disease_), and *Tøktn* (_Heavy_). A _*disease*_-charmed object passively sanitizes 99.99% of germs within a 6ft sphere around it. If used to harm, it cripples the immune system while doubling the rate of disease propagation within 6ft of them, for 12 hours. A _*heavy*_-charmed object weighs 1/10th its normal weight, along with anything within its space (A box, or cloak). If used to harm, damaged objects or parts of a body feel as though they weight 50% more, stacking, for 12 hours.
        """, """
        Manifest the runes *Tifhæt* (_Talent_), and *Vÿkst* (_Wind_). A _*talent*_-charmed object boosts hand-eye coordination, reflex, and memory recollection in ways that let you recognize and repeat actions or understand the rules of something with ease, letting you reach a high degree if skill in something within a few attempts depending on your exposure to the | correct processes, which you can uncover yourself through failure and recognizing why you failed. If used to harm, it dulls these same senses for 12 hours. A _*wind*_-charmed object emits a faint breeze in all directions that is amplified in the direction of a movement proportional to the speed and force with which it was moved, and air displacement. A fan could emit a gale like a jet engine. If used to harm, cutting force is amplified as though there was a silent chainsaw along an edge, or bludgeoning force is amplified with twice the concussive force and 10x knockback.
        """, """
        Manifest the *Hekyll* (_Spell_), and *Bàsynn* (_Warden_). A _*spell*_-charmed object imbues the object with the magic of another magic effect available to you that functions as normal on that object or when that object is worn, or triggered when it makes contact, depending on the nature of the effect. For example, with Familiarity, you could make a charm of fox’s cunning, or a ring possessing one life based on the 9 lives. A curse charm could passively curse people or specific targets within a short distance from it with a given curse. A _*warden*_-charmed object dispels magic on contact, for that one creature or object touched. If a permanent enchantment, that enchantment is suppressed for 12 hours.
        """, """
        Manifest the *Taldél* (_Time_), *Skyttse* (_Protection_), *Gjenÿkvie* (_Relic_) A _*time*_-charmed object stops the flow of time on the object barring outside influence. If worn, it prevents aging. If mostly encased in a charmed object, it’d fully suspend animation. A bowl of apples would never wither. Used to harm, such an object would age creatures by 2 years and objects by 10. A _*protection*_-charmed object prevents harm from occurring. 1/10th the prevented harm is directed to the charmed object instead as though it were struck itself, at 10% force. A _*relic*_-charmed object can copy the magical (Non-material/physical) effects on a relic from a relic on hand, into a rune which applies the same effect in a way that makes sense for the effect, onto another object or directly benefiting a wielder of the charm. Can apply the effects when worn, or on hit, as appropriate.
        """ ]
    }


curses : Details
curses =
    { name = Curses
    , star = False
    , class = Just Warlock
    , affinities = Regular [ Necro, Nature, Mind ]
    , dlc = Nothing
    , isElementalism = False
    , description = """
        Curses aren’t evil, just misunderstood. So you made a guy throw up or every day of his life a waking nightmare. Is that really so much different than simply stabbing someone? That guy you fireballed? Probably disfigured and in pain for the rest of their life. That lightning bolt? Lifelong neurological complications. Honestly, who’s the real monster here? The primary curses are _*Sickness, Pariah, Madness, Disaster, and Spellbind*_. They grow in power as you rank up your Curses magic, though you can always still cast them at a lower rank, which will generally be quicker and easier to cast. To use a curse, you must chant the particular words for the curse for 2 seconds while keeping a finger pointed at your target, and while within 30ft (10m), unless otherwise stated. Curses can be given Conditions in how they apply. ie; Only occurs on the next full moon, when they say a word, when someone else says a word near them, when they cause someone harm, or go within a certain distance, or so on. Itonly triggers once unless it has a duration, in which case you can choose a window for when it activates and divide the duration up between activations. You can also stack curses to add triggers, activations, or duration.
        """
    , ranks = [ """
        *Sickness* inflicts a bad but manageable cold that takes around half an hour to come round a day to recover from. *Pariah* makes the target hated by insects which turn - hostile within 30ft of it. *Madness* causes a rush of white-hot anger, but they’re still themselves and react as they would. *Disaster* causes the target to trip and fall within the minute at the first reasonable opportunity to do so, if not their own feet at the end of the duration. *Spellbind* lets you pair any other magic effect with a curse, of a rank equal to your curses rank. The magic effect must satisfy all its own requirements in creation or casting, then the activation and duration change occur when you curse a target. By channeling curse power when you drink a potion for example, you could reserve the potion effect as a curse effect, effectively like a piece of ammunition.
        """, """
        *Sickness* inflicts an incapacitating flu that requires bedrest, hard to do anything the world is spinning and chimps are playing with hammers in your brain. Takes an hour to set in and will last a good 24 hours. *Pariah* now also attracts small animals, and within 900ft (300m). *Madness* now also prevents the recognition of friend vs foe. *Disaster* causes an instant charlie horse muscle spasm in one or both calves of the target, which if you aren’t aware causes debilitatingly intense pain while the muscle violently contorts on its own in ways you didn’t think possible. Very surprising if it’s never happened to you before, believe me. *Spellbind* rank 2 magics.
        """, """
        You can now cast mass curses that afflict all in your field of view at once. You no longer have to point for the primary curses. The chant takes an additional 2 seconds per 20 targets. You can now also curse specific targets regardless of distance or view if you craft a effigy of the target out of natural material and add something that came from their body (hair, nail, blood) or a close possession of theirs. *Spellbind* rank 3 magics. New curse effect: *Voodoo*, you can form an effigy or doll of a target, and the target will feel anything you do to it, though it wont force movements or cause actual physical harm.
        """, """
        *Sickness* now immediately causes a high fever, dizziness, and vomiting. It will kill weaker people if they don’t receive medical attention within a few hours. *Pariah* now affects all animal life save humans within a mile, and plants will do what they can as well, such as bending towards them in their path, slow moving roots rising to trip, poison ivy or cactuses reaching out. *Madness* removes all higher thought processes to turn them into a thoughtless feral predator for several hours with resistance to harm and fatigue. *Disaster* causes accidents that can result in serious or fatal injury whenever the possibility is there for the next 12 hours, no more than 1 per hour, can include seizures or momentary microsleeps that can lead to disaster indirectly. This can cause pipelines to explode under the street, sinkholes to open up under their feet if geographically feasible, lightning to strike if there is a storm, ect. *Spellbind* rank 4 magics. *Voodoo* can now puppet the target’s movements if you manipulate the effigy/doll with intent, and damage transfers.
        """, """
        You can now curse people with immortality, exclusively a rank 5 curse. Recipient cannot die by any means (save for things designed to slay immortals) but are just as vulnerable to injury and pain. They will continue to age as normal even long past organ failures, even as their body decays around their bones like a lich. They’d still feel pain from phantom nerves that think they’re constantly exposed and raw. Alternatively, Immortality curse can strip an immortal from their immortality, their agelessness as well as their witch type’s method of resurrection, until the curse is dispelled. Rank 5 sickness, pariah, madness, or disaster are wide scale curses that afflict city sized areas with earthquakes, tsunamis, tornadoes, plagues of locusts or vermin, or deadly contagious diseases. *Spellbind* rank 5 magics. Curse effects bypass Warding Runes so long as the curse was placed while unprotected. *Voodoo* can now bind any number of victims to a single effigy/doll to influence them all at once. If you talk to the doll with intent, they will hear it as though in their own minds. Your manipulations of the dolls can cause them to levitate, and you can have fine control over movements based on your intent. Control over multiple people is based on intentions, so they would all move in a similar way as necessary, moving towards the same destination but not in the same direction for example. Curses placed on a voodoo effigy last until the doll is destroyed, afflicting the victims. All rank 5 curses require the light of a full moon and ingredients comparable to a rank 5 potion. Alternatively, K150,000 can be burnt to fuel a rank 5 curse, or you can sacrifice a limb at any time, blackening like charcoal unusably, which can’t be healed without full moonlight.
        """ ]
    }


hexes : Details
hexes =
    { name = Hexes
    , star = True
    , class = Just Warlock
    , affinities = Regular [ Body, Beast, Blood ]
    , dlc = Nothing
    , isElementalism = False
    , description = """
        Also known as _*“Transmutation”*_, these spells transform something into something else using the base logic of “Inequivalent Exchange”. You will need a Medium, usually a lump of raw material, part of which will vanish when you cast the spell. You can cast most hexes the reverse way unless otherwise stated. (ie; Flesh-to-Stone can also tum Stone-to-Flesh). Hexes that can’t be reversed in that manner can still be undone by recasting the hex on that target. Casting a hex is as fatiguing as jogging 1 mile per rank used. If you want to try to leverage transmutation to make money, I’d recommend against doing so on any significant scale, as financial institutions and agencies are more adept than you’d expect at tracking assets, easily leading to masquerade violations. Just use it tastefully or for your own private usage, you shouldn’t have any problems unless you go off trying to use it as a crutch you center a business around making people wonder about your sources. Any polymorph spell cast on an intelligent creature can be undone by any other polymorph spell cast on that same creature, even without knowing they were polymorphed before; They’d instead revert.

        *Rank 0 Effect*: Any witch is capable of transmuting raw mana into minting their own Witch Kisses. Takes 6 minutes per K1, metals aren’t required, only your mana. Has a very minor mana drain. Just need to learn how, with a level of focus while drawing a circle to start the conversion.
        """
    , ranks =
        [ """
        *Medium*: Any wood. Each spell uses roughly a twig’s worth to transform up to 1 cubic foot of material. Or K5, 5 Witch Kisses.

        *Hexes*: Stone-to-mud. Clay-to-glass. Iron-to-tin. Salt-to-sugar. Polymorph.

        *Polymorph* can target living creatures up to the size of an adult cat, and turn them into a other animal up to half or double the mass.""", """
        *Medium*: Any metal. Each spell uses roughly 4 quarters worth of metal per cubic foot of material being transmuted. Or K25.

        *Hexes*: Cloth-to-leather. Water-to-milk. Paper-to-plastic. Copper-to-bronze. Fuel-to-bread (Fuel being any combustible material to its calorie count worth of bread).

        *Polymorph* can affect creatures up to the size of a child, and can transmute into anything living born of your imagination within the scope of effectiveness of a normal animal.
        """, """
        *Medium*: Jewelry-quality quartz or topaz. Uses about 0.1 cm³ per cast. Or K100.

        *Hexes*: Egg-to-chocolate. Water-to-gasoline. Vegetable-to-meat. (Cooked veg produces cooked meat, raw meat produces raw veg). You can also now cast _*Animate Objects*_, which animates objects weighing up to 15lbs each with a basic loop comparable to programming. They will continue to try to perform this loop until you dismiss it or the object is destroyed, and they can hover and fly as part of this. They can be given up to 3 commands that cause them to shift loops to another programmed response. Recommend saving 1 for a stop function. Their levitation can support up to 25Ibs of weight. Walking can support up to 75!bs. Rolling on wheels or crawling on 5+ limbs can support up to 100Ibs. You can also create loops wherein a director object issues and dismisses directives to other objects. Example: A dish sponge that animates dirty dishes, washes them, and causes them to march to the shelves where they de-animate. This example pushes the limit in complexity.

        *Polymorph* can affect creatures up to the size of a human adult, into any smaller creature (Minimum size: mouse). Can now be used to reshape objects without changing material.
        """, """
        *Medium*: Ebony, ivory, amber, petrified wood, fossiles, or fulgurite. Roughly 100 grams per cubic foot of material or spell cast. Or K1,000.

        *Hexes*: Wood-to-cheese. Cloth-to-air. Steel-to-cloth. Fruit-to-treat. (Tum a fruit into ice cream or candies). Can cast _*Golemwork*_ to animate statues. Statues are as agile and mobile as a human person is. For every 7ft, reduce its mobility by 50%. It exerts force proportional to its mass and speed, and as durable as its material. Creating a golem requires human sacrifice to animate it unless you have Necromancy to bind a spirit that way instead. Alternatively, you can animate it without a soul and it will behave like Animate Objects instead, relying on basic program-like actions like a machine, or rough video game Al.

        *Polymorph* can affect any living creature into any other, from a tardigrade up to a blue whale in size, or into an inanimate object.
        """, """
        *Medium*: Platinum or jewelry quality diamond, sapphire, ruby, emerald, citrine, or opal. 0.1 cm³ per cast. Or K10,000.

        *Hexes*: Old-to-young. Water-to-wine (or any beverage people drink). Lead-to-gold. Flesh-to-stone. New spell: _*Counterfeit*_. Duplicate any object using medium. Magical effects aren’t copied. Requires equal platinum for any platinum part of the object being duplicated. Duplicated blood is generic blood (No ritual benefits), duplicated K are inert coins.
        """ ]
    }


witchery : Details
witchery =
    { name = Witchery
    , star = True
    , class = Just Academic
    , affinities = Regular [ All ]
    , dlc = Nothing
    , isElementalism = False
    , description = """
        A long time ago one of the very first witches to exist was very kind and motherly. She wove a spell to effect all her descendants, in flesh and in spirit which includes almost every witch to exist as her children became the dominant lineage and other families married in. Like Gengis Khan, you know, if he were a sweet mom.

        _*Rank 0*_: All witches with rare exceptions can meditate to call upon these _mothergifts_; The _Garment_, The _Hat_, and the _Rod_. Your garment is a single piece article of clothing like a robe, dress, toga, or so on. Your hat is a wide brimmed hat in some fashion. Your rod is anything between a wand and a staff that makes spellcasting slightly less fatiguing and slightly more potent. Without practice, ranks in witchery, the way they appear when you first manifest them is as they will always be, and they’re very plain by default. Experienced witchery practitioners can modify them for you, like a tailor. Takes 10 minutes of meditation to summon. They always appear fully repaired when summoned.
        """
    , ranks =
        [ """
        You deepen your connection to the _Mothergifts_, allowing you to summon them and dismiss them at your leisure with a visual flair that varies from witch to witch. When the Garment is summoned, it replaces your current clothes in totality, save for magical items and charms, so either enchant your underoos or keep your garment modest. The hat just replaces any headgear that gets in the way. Whatever your Rod is, you can now shift it between its base, and the form of a broomstick. This broomstick can carry your weight + 50%, at the speed of a bicycle. You can meditate for 10 minutes to alter your mothergifts into a preferable design.
        """
        , """
        Your garment now provides protection against the natural elements. You will never be too hot or cold while wearing them, they dry off in seconds, and self-clean, and now self-repair without having to resummon. Your broomstick can now fly at 60mph, and projects a shield of air that stops wind, bugs, rain, and other small airborne things up to slightly slower than an arrow. Your broomstick will automatically catch you from a fall, if there’s room to do so. It can carry three times your bodyweight now, allowing a second passenger. Only takes a minute now to meditate on changing the design of your gifts, and your garment can be multiple pieces that disappear if separated from the rest.
        """
        , """
        You expand your mothergift to the extent that you can tap the pocket dimension from which they appear, allowing you to store anything you could carry in its space, so long as you can fit it into your robes or hat in a way that it can’t be seen by anyone else. You conjure them back in the same manner, drawing your wand from your sleeve for example, or pulling a rabbit out of your hat. There is no breathable air in this space but biological functions are paused, including thought. Your broomstick can now go 200mph and the air shield now provides oxygen, pressure control, and air conditioning. It also provides an air cushion to ride in comfort without the discomfort of, well, I’ll leave it at that. Your broomstick can also become a magic carpet instead, capable of supporting more passengers but traveling at half the speed, and up to 6x your bodyweight, 8x for carpet. You can now summon/unsummon in the blink of an eye, changing the design of your gifts in the process.
        """
        , """
        Your garment is now able to turn you invisible like an invisibility cloak, and while on your broom or carpet this extends to any passengers and the broom/carpet itself. You can now travel without fear of being seen. Avoid restricted airspace, as some sensors can pick you up, but radar and visual perception wont. Thermal is the easiest, especially if you’re blasting your AC in your airshield. Your broomstick can lift 10x your bodyweight, or 15x for carpet, flying up to 600mph. Can modify the appearance of a willing witch’s garment or hat.
        """
        , """
        You are now also able to pull your hat down over yourself or turtle under your robes to disappear into your pocket dimension in the Aether, a starry void with no gravity, and no shadows, everything is perfectly illuminated from all angles. With meditation you can change this pocket space similar to changing the appearance of your gifts. To do so first find something real that you want to copy including just a view of a forest for example, and meditate with it in view to make a snapshot, adding it to assets you can draw on to add to your pocket world, adding gravity, landscape, ect. Things you manifest in this way don’t really exist and can’t leave or do physical harm to something real, but you can still feel them with all your senses and interact normally. While riding your broomstick or carpet, you can remove your garment or hat and throw it in front of you to have it ripple open like a portal you can fly through, taking your passengers into this space. You can also disguise your garment as a curtain door people pass through to walk into your realm on their own. Perfect for the illusion of a shop bigger on the inside than outside. Once inside anyone can leave by willing themselves home, to appear inside any residence or business they or a family member owns or rents, including yourself, or you can appear where you left, your hat or garment still there to emerge from unless you pulled them along with you.
        """
        ]
    }


familiarity : Details
familiarity =
    { name = Familiarity
    , star = True
    , class = Just Sorceress
    , affinities = Regular [ Beast, Soul, Nature ]
    , dlc = Nothing
    , isElementalism = False
    , description = """
        There’s an old spell, one of the first ever cast by a witch. It’s almost too easy to cast and never seems to leave a witch’s memory. A witch instinctively can figure it out for themselves just trying to use magic without direction as a rank 0 magic: _*Find Familiar*_. You can conjure a helpful benevolent spirit being, that may or may not parasitically conjoin with your own soul. It manifests in an animal form you can choose when you summon it. Cat, crab, crow, cow, bat, butterfly, dragonfly, frog / toad, hawk, horse, lizard, octopus, owl, snake, fish, rat, raven, sea horse, spider, weasel, ferret, goat, sheep, dog... or equivalent animals (Mockingbird > Raven, Squid > Octopus) It looks like a normal animal of your chosen type, but demonstrably has greater intelligence. It’s as smart as a trained dog and utterly loyal to you, and your survival and thriving is in its best interest as its life is tied to yours. If its body dies you can resummon, or dismiss, it casually, and it doesn’t age. You pick whether it is male or female. Familiars can interact with spirits. Reminder that this is rank 0. Any witch can have a familiar at this baseline.
    """
    , ranks =
        [ """
        You lose any allergy to any animals you may have had, as well as lactose intolerance, and you can now telepathically communicate emotions and intentions through your soulbond, even when it is dismissed. You can use magic through your familiar, such as having your familiar do the pointing while you chant a curse.
        """, """
        You can now talk to any of the above listed animals that are familiar options. (Not just your choice of animal). They understand your meaning, and you understand theirs. (No need for chicken squawking. Unless you’re into that, idk). Most animals won’t just do what you ask without incentive and their intelligence is limited. Even your familiar tends to behave in line with its respective animal by default and could use a bit of convincing depending on your relationship.
        """, """
        Some of your familiar’s qualities rub off on you, based on things that its species is known for and not actually the physiology of the animal, such as night vision and being able to jump high and land well for cats. Climbing and eating anything for goats (You’d be able to digest things and/or pass them harmlessly so long as you’re able to swallow it and it isn’t overtly toxic). Lighter body and better memory and intelligence for crows. (Flight is too much, no, but something like water breathing if your familiar is aquatic okay). Yes, a cow familiar does enlarge your breasts, h-hey! Eyes up here, also makes you stronger. Choose any 2 relevant animal traits. If future options grant any more forms for your familiar, you can pick one quality of similar power level to add to yourself. There is not hard rules on what constitutes a valid trait other than some reasonable cause, and it not being more potent than what’s shown here.
        """, """
        You gain the ability to shapeshift into your chosen animal. It’s just an animal, it doesn’t have special powers, but you can still cast curses, maintain the effects of potions, drink potions, wear charms, etc. It has some distinctive markings and resemblances to you but most people won’t look twice. This goes both ways, your familiar now has the ability to shapeshift into a human! It gains human level intelligence (in both forms), and has all the personality traits that humans stereotypically associate with that animal, though it’s still subservient towards you and obeys most commands with some room for mischief or stubbornness based on their personality. Looks wise, they’d have human traits that you’d associate with their animal. Lets just say my familiar gets as busty as me! You and your familiar both can adopt features from either/or forms in a blend, such as maintaining cat ears or a tail in human form. This is one of the primary reasons familiar spirits form these bonds with witches, they love to explore the world and are grateful for the opportunity to adopt a human form and their improved intelligence. Some may even fall in love with their masters!
        """, """
        Whatever your familiar choice, this magic is originated with cats and that extends to this; The gift of 9 lives. If you die by any means, you’ll wake up young again in your own bed. You can gain extra lives by saving a cat’s life, though never going above a reserve of 9 lives. A good way to stock up is by adopting cats from certain animal shelters, where they were sure to meet an untimely end. Look up which are kill shelters or not. If you see a dying cat (or any other animal, or child) you can use one of your 9 lives on it. You also age very gracefully, like a cat, and won’t start to look or feel old until you’re in your 70s, and you might even look like you’re still in your 20s depending on how you treat yourself, no more than 45 even if you didn’t take care of yourself. At any point when you’re over the age of 60, you can simply mentally use up a life to replenish your body without unpleasantries. Additionally, you can now shapeshift into any form your familiar possesses, other than a Metamorphosis form.
        """ ]
    }


necromancy : Details
necromancy =
    { name = Necromancy
    , star = True
    , class = Just Warlock
    , affinities = Regular [ Necro, Blood, Soul ]
    , dlc = Nothing
    , isElementalism = False
    , description = """
        Necromancy! The magic of souls, undeath, and viscera! Bending organic matter to your whim for your own ends and contesting the very nature of mortality, Now with less grave robbing, and Necromancy has an innate _*Rank 0*_ effect for all witches: All witches are capable of seeing through the veil separating life from the dead, to see ghosts and spirit creatures (Including most demons and other things out to get you that most humans can’t see). Most spirits are collected by reapers and escorted to the afterlife of a deity that claimed them, while unclaimed souls go to limbo until granted passage to an actual afterlife. But some spirits flee from their reapers or their death went unnoticed, which is dangerous, as predatory spirits can prey on the weak and the spirit can be consumed, or get twisted over time to become a spirit beast itself.
    """
    , ranks =
        [ """
        Beginner necros are able to weaken the veil so even humans can see spirits, as well as force spirits capable of hiding from witches to become visible. They can reanimate small corpses of animals for as long as they maintain the effect, up to 3 at once, to behave as they did in life with an echo of their life force imprinted on the body. If you visit any grave, you can use _“Communion”_ to attempt to speak with the spirit from wherever they are in the afterlife or spirit world, but they can not notice, ignore you like an unknown number, or may not exist anymore. Be careful who, or _what_, you contact...
        """, """
        With just rank 2, A necro can now purify corrupted or faded spirits that they manage to subdue in some manner with a 1 minute ritual that can restore the original intelligence, as well as optionally mark them so a reaper will find them to guide them on. Can now reanimate medium sized animals, up to 9. Reanimated bodies need not be composed of the same body, but can be amalgamations. With a word of command, you can control bone within 120 meters with up to the force of an arrow or swing of a weapon, up to 3 skeletons of mass.
        """, """
        The necromancer is now able to reanimate human sized remains, up to 27, and Ifa spirit is available, you can bind it to a reanimated construct to give that soul a physical form, and doing so means you don’t have to maintain the animating effect though it still counts to the limit, but won’t fall apart if you are unconscious, and it now can’t be dispelled. You can engrave the bones with runes if you know it. Souls need to be convinced to cooperate with you, you don’t have unnatural influence over their choices with this alone. Your manipulation of bone also extends to dead flesh, within 340 meters and up to the mass of 3 whale skeletons, which can now have the force of a cannonball. You can spontaneously generate bone matter equivalent to a human skeleton with a minor mana cost and a second or two of focus.
        """, """
        Now capable of reanimating up to 81 constructs up to the size of whales. Bodies or bound spirits will magically emulate biological and supernatural functions they had in life up to your own power level. You can now act as a reaper to send a spirit to an afterlife you know about, though a deity in charge can reject it unless doing you a favor. You can now control living flesh and bone in order to rip bones from the living or cause excruciating pain to those who are unprotected, such as via warding runes like most witches, within 600m in line of sight up to the mass of 10 whales with the force of an artillery shell. You can sense flesh and bone within 60m through walls. Constructs now don’t require focus, can spontaneously generate as much bone as you can control.
        """, """
        You gain the ability to become a lich. You can craft a phylactery you can put your soul into, which can be any object with a minimum size of a ring, so long as it’s one solid object as opposed to a composite ie; A stone slab vs a slab of tiled pieces of stone. Small objects can be hidden and moved easily but a large object might be harder to destroy, or perhaps even notice as being special. While you have a phylactery, any damage done to your body is superficial, you can continue to remote operate your body even when all flesh is stripped away leaving only bones, even just a single skull. At any time you can abandon your body to instantly snap back to the phylactery and regrow a new body over the course of a few days. If your witch type has other methods of life extension, you can choose which occurs first, or switch that method to occur next to the phylactery instead of a usual location and from any distance. The phylactery must have a path to the open sky (or equivalent in the cas of other planes), but it doesn’t need to be a straight path; It could be at the bottom of a dungeon labyrinth, and the path can be as narrow as 1 inch for up to 10ft at a time. Damage to the phylactery is true damage to you, proportional to the damage on that object. You’ll feel proportional pain, and if destroyed, you die unless you have other means of cheating death as backup. Since your soul is sequestered away, effects that “kill immortals” have no bearing on your lichdom. If your meat suit dies on you it can still be a viable target for healing magics to wake it back up, but it functions as normal even if you were to lose organic parts like eyes and ears, an undead body would still feel if it has flesh. Your _*reanimation*_ can now animate beasts of any size from dragons to leviathans and you have no cap on the number of individual reanimated constructs. Sense flesh and bone within 240m, and control within line of sight with the mass of 50 blue whale skeletons. If you have a pocketspace, you can use it as an afterlife for spirits, though it can be a little unsecure compared to a real afterlife.
        """ ]
    }


consortation : Details
consortation =
    { name = Consortation
    , star = False
    , class = Just Warlock
    , affinities = Regular [ Blood, Beast, Soul ]
    , dlc = Nothing
    , isElementalism = False
    , description = """
        Just because you can do something, doesn’t always mean you should. Demonology. To summon a demon you’ll need a summoning circle away from sunlight, and with fire in the area equivalent to 4 torches (or 16 candles), or more. Demons also appreciate incense, mood music, and mood lighting. Summoning them is fatiguing, but dismissing them is simple, like lifting something heavy and then just letting go. When summoned you have a psychic bond allowing two-way telepathic communication and know when the other wants to talk. They can’t refuse your coms, but you can refuse theirs. You have to provide your payment up front, after which they are bound into service until they fulfill their bargain.
    """
    , ranks =
        [ """
        You can summon _*Imps*_. The minorest of minor demons of Wrath, Gluttony, Greed, Sloth, or Lust, with personality and interests to match. They’re 6 inches tall and exert up to 10lbs of force, flying magically irrespective of their tiny wings. Greed likes shiny things, sloth just wants free time to lounge with food for an hour per hour of service, lust just wants internet access, or a show, maybe a grab or two, gluttony wants a full sized meal they can lounge in and eat beyond their fill. Wrath wants to cause harm. Greed wants a shiny trinket. Based on 24 hours of service per imp. They have decent skills relevant to their Sin type. Lust gives decent massages, gluttony is a reasonable cook, ect.
        """, """
        You can summon a _*Foliot*_, a minor demon of envy. Foliots are adept at thievery and will happily take small belongings from specified people and deliver them to you. As payment they will usually take something else precious from the target while they’re at it, or you can offer something important to you instead. They’re the size of a teenager and you can summon up to 3. They can work together to steal larger objects like a fridge. They are invisible on demand and can make objects or others invisible with a touch for up to 1hr.
        """, """
        You can summon the classic; A _*Succubus*_, or _*Incubus*_. Adept at seduction and ruining relationships, most like to summon them.... for personal use. They take any kind of sex as payment for their services, which can be on the job. They’re excellent infiltrators if at all possible to get in via seduction or physical appeal. You can also summon a _*Nabasu*_, demons of gluttony that are 5-star chefs, and they want to watch you overeat as payment, to the point of discomfort at least once for a week of service. Succubi, incubi, and nabasu have physical forms that look human, but witches and mediums can see their true forms in reflections. You have to provide the ingredients for a nabasu to do its job. It will do shopping itself if you provide the funds, but wants extra payment to do so, which generally involves creepily rubbing your belly after overeating, and of course, eating roughly half of what it buys for itself.
        """, """
        You can summon a _*Balor*_, a greater demon of Wrath. It cannot be controlled or tamed, it seeks only destruction and the implicit contract in summoning it is that it does its thing, and if you get in its way, it gets you. It’s the size of a house, flies, and breathes fire. It can throw a car half a mile and is immune to nonmagical weaponry, heat, and explosive force. You can loosely direct it in a direction you summon it in or by speaking the name of one target in the process of casting, and it will act like a bull in a china shop as a living explosion crashing its way towards the named target, deliberately causing as much collateral damage as it can. Lasts 10 minutes, no more no less. You can also summon _*Astarothi*_, sloth demons in the form of attractive but emotionless mute maids or butlers that do absolutely anything you want. They can serve you in almost all home related aspects but are non-communicative. They encourage laziness and want you to laze around doing nothing productive per hour of their own labor 1:1 basis. If you won’t be lazy, they will, until dismissed or you resume lazing, up to 12 can coexist on the same payment.
        """, """
        You’re good enough now to summon a _*Greater Desire demon*_. The Greater Demon (who would have a unique name), wants your fealty, dedicating yourself to offering souls in its name at least once a month as long as you live. Every sacrificed soul in the last month counts as an extra life for yourself, though you owe the demon back if used, and you no longer age. Your true form can become corrupted, gaining demonic features you might want to suppress with magic. Additionally, you are granted _*one*_ premade wish: *Power*: Gain 33 Power. *Fame*: Gain any 2 Companions, choose a career - you’ll become the best in that field with increased media attention, within 10 years depending on how competitive the field is. *Life*: Take The Value of Life cyoa applied to your witch [https://imgur.com/a/xoKqyel]. *Wealth*: Gain 1 million USD every night at midnight in the form(s) of your choice; cash, gold bars, gold coins, gemstones, or bank account balance, and gain 100 million Kisses. Benefits are halted if you fail to maintain your end of the bargain. You can bank extra souls at once ahead of time, so damning a whole village might set you for a thousand years. If the demon who holds your debt were to be slain (An epic quest on par with wrestling a Great Wyrm Dragon & not easy to get to), you would retain benefits. Or you could use bribery to pay off your debt with a feat worth a 1,000 years in one lump sum. Alternatively, you can become a Succubus, Nabasu, or Astarothi in hell directly under its command. It will use your services personally for 10 years before you can upgrade to a _*Dalihlah*_ demon, a demon witch regaining all your past witch abilities and witch type in addition to a demonic true form of your description. You will still be its personal servant or concubine and general use agent for an additional 90 years, then you will be free. During your service you can earn an extra wish or early freedom.

        _*As a Dalihiah*_, you can breed fusion demons: Demons with the traits of any two, including accepting either payment. The methods are up to your imagination.

        *Note*: Wishes from ANY sources cannot directly stack with any other prior wish. So no wish could grant more than 33 Power, including directly wishing for, say, a specific perk.
        """ ]
    }


portals : Details
portals =
    { name = Portals
    , star = False
    , class = Just Academic
    , affinities = Regular [ Life, Nature, Mind ]
    , dlc = Nothing
    , isElementalism = False
    , description = """
        The magic of portals is sometimes called gatekeeping, planeswalking, or helldiving. By default, all witches are able to see most basic level hidden portals or rifts, and can enter them where normal mundane humans would pass through most as though it weren’t there, oblivious, sometimes experiencing vertigo or deja vu. Portals can be hidden anywhere in any number of forms with three main archetypes; Portkeys, which are objects spatially bound to another location that act like a ferry shifting between locations at regular or random intervals. Gateways, which are arches or door frames enchanted to act as a portal when triggered. And Rifts, which are wounds in timespace cut through by a witch on demand, or anomalous natural events. Rifts open as fast as you can complete its outline. Their outer edges are the sharpest "objects" known to witchdom.

        Creating a rift is roughly as fatiguing as 2 jumping jacks, and 1 more jumping jack per 10 seconds active, and they cannot be made to intersect existing solid matter. A rift can be damaged from the back or sides, or objects too large to fit, with durability shown per rank. Creating a portkey or gateway takes a ritual sacrificing objects of monetary value equal fo $10 per mile linked. Portals are stationary, and can come in any variety of themes and behaviors based on the witch that created it.
    """
    , ranks =
        [ """
        With rank 1, you are able to create rifts 6 inches in diameter within 6ft of you connected to another point you can see within 300ft of you, or that you are very familiar within within 60ft. Plenty of uses for these guys, probably the most common is retrieving drinks from the fridge. Paper-like durability. You can create 1 Portkey or Gateway linking 1 location to another location you have been, within 50 miles.
        """, """
        At rank 2, you can create rifts that are up to 12 inches in diameter at double the range, and 3 portkeys or gateways which can now work within 200 miles. You can open a 1-way rifts to a proxima, a dimension that is adjacent to an Elemental Plane (As in, they’re like a window from your side). You can tap the proxima of the elemental plane of fire, and the elemental plane of Ice. Fire Proxima rifts can be between 150 to 500f, or 65-260c, degrees hot, which you control with your intentions creating the portal deeper into the proxima. Makes for good hot plates or grill tops. Ice Proxima rifts are between 32 to -80f, or 0-62c. Make great ice trays. Glass-like durability.
        """, """
        At rank 3, you can now create rifts without having to trace the outline by force of will but they’re 4x as fatiguing this way, entry rift being within 60m. A flicker of color varying from witch to witch ripples out in a spherical area of the same diameter as the rift being opened a moment before the rift snaps open, a process of about 1.5 seconds. Your rifts can be up to 3ft in diameter, doubling the range again. You can create up to 6 portkeys or gateways now, linking points up to 800 miles apart. You can now tap the Storm Proxima, creating electrical rifts that can power a city block at once. Reminder that since it is one way, you can hook jumper cables to it. Inch-thick steel durability.
        """, """
        At rank 4, you can now create two-way rifts to proximas, but moving through them is like moving through a wall of jello, requiring a level of force to push through with non-trivial effort. Your one-way rifts are like 6 inch tungsten plates if damaged. Your rifts can be up to 7ft in diameter, that are equivalent to 1ft of titanium plating. You can make up to 32 portkeys or gateways. You can now link portkeys or gateways to anywhere you have been, in any dimension or extraplanar realm you’ve been. You can now tap the Water Proxima, creating rifts functioning like water hydrants. Water rifts cease functioning after 12 seconds when submerged, so they automatically stop if flooded out. Your rifts now have a range of 10 miles provided you can visualize the location sufficiently.
        """, """
        With rank 5, you have no limit on the number of portkeys or gateways you can produce. Your rifts are now up to 14ft in diameter and connect to anywhere you have spent more than 72 hours at, or that you can sufficiently visualize, or you can blindly connect a rift to somewhere a set distance and direction from you (provided the rift does not intersect solid matter). Additionally, you can create permanent rifts that have no drain up to 12 inches in diameter, but they retain glass-like fragility. Your non-permanent rifts up to 3ft are impervious. You can now delve closer to the true elemental planes. Fire: 6,000f. Cold: -420.69f. Storm: Power all Tokyo at once. Water. 12 fire hydrants, potentially at high pressure if focused, cutting within 60m with laser-like focus. New plane: Void- Absolute nothingness. Tap into the Void itself, not an Elemental Plane, but nonetheless accessible. Pulls things in like a black hole with an event horizon that’s 3x the diameter of the rift itself and a pull felt with exponential intensity at twice that distance. You can hear it whispering sometimes, as can anyone that can see it, occasionally making witnesses feel compelled to walk into it. Things fade in the Void, disappearing in 24 hours.
        """ ]
    }


divination : Details
divination =
    { name = Divination
    , star = False
    , class = Just Warlock
    , affinities = Regular [ Soul, Life, Mind ]
    , dlc = Nothing
    , isElementalism = False
    , description = """
        {choice This magic can also have no affinity, and be taken by human Alphazon agents with observers.}

        Also known as _tactical magic_, Divination is magic of knowledge, information, data. This school of magic presents data to the caster in a manner adapted to their sensibilities and expectations. Past witches used to rely on acquiring data through stargazing, runecasting, smoke, reflections, or crystal balls, but conceptual refinement of modern witches (_at your option_) allows direct data interpretation within your own minds eye in your field of view.

        Divination has a *RANK 0 effect: _Sending_*. A witch can telepathically attempt to link with any intelligent being they have met to attempt to message them via text, voice, live image, or with r5: _telepresence_, as though you were face to face, even seeing their local surroundings. Sending can be _rejected_ or the sender _blocked_.
    """
    , ranks =
        [ """
        Divination begins with the spell _*Identify*_: A quick spell needing only a single word chant, it will provide basic public details about a target you can see. Such as publicly recognized names and 1 popular detail. As simple as identifying a rock, followed by an obvious observation about it. Can give you the name of a public figure and a factoid about why they’re a public figure, and their most known title. Such as - “Alyssia, Queen of Ix, known to have stolen the crown from her sister by subversion.”
        """, """
        _*Identify*_: Can now reveal magical qualities of items or active magical effects, the type of magic, affinities involved, and expected effects. You also now gain the _*Status*_ spell, which reveals any ongoing effects that alter the normal function of the target, up to the caster’s ranks in Divination. For example: Burned, Poisoned, Diseased, Cursed.

        You have the spell _*Augury*_, which can give you a Positive, Negative, or Neutral response to a question regarding events in the next 30 minutes.
        """, """
        _*Identify*_: Can now passively toggle effects, with things such as seeing names over people’s heads, or in your preferred display method, and priority relevant information.

        _*Status*_: Now reveals more specific information, and can passively nest information alongside Identify. Example: Burned: Hellfire, 4 minutes. Diseased: Viral, PX320, 2 weeks.

        You now learn the spell _*Map*_. By using this spell you map out the area with a line of sight pulse to produce map data you can review in whatever display method your divination manifests. You can view this information top-down in blueprint or aerial mode, or as a full 3D view. You can place custom markers and use Identify retroactively later on on your mapped information, which can identify Friend or Foe. With passive focus you can leave Map active to continue to generate your map data.
        
        You also have the spell _*Archive*_, which allows you to store information you see, like taking a photo, or scanning the visible contents of a page of information you can see and can keep track of items in your witchery _Pocketspace_, or owned properties.

        _*Augury*_ can apply to the next 12 hours.

        Gain the spell _*Foresight*_, with a quick ritual chant you can enter a precognitive battle meditation allowing you to see .5 second into the future, with visual shadows leading events.
        """, """
        _*Identity*_: Unless masked by wards or countermeasures, identify now reveals private information like your private name (not True Name), main place of residence, factions, and your ranks in magic specializations, perks, owned relics, and major relations (Companions, Family, Romantic entanglements, etc).

        _*Status*_: Now reveals the source of the status effect, how long it has been applied, and predicted results (eg: Death in 2 minutes), and suggestions on counteracting the status.

        _*Map*_: Mapping can now work retroactively through your memories to generate map data of places you’ve already been and It now requires no focus to keep active, and it automatically applies Identify information to map data.

        You now learn the spell _*Lock-On*_, which can bind another spell effect to targets or locations you mark with active mapping, within sight, your spell will gain 20x normal range and will home in on the marked target with the maneuverability of a basic paper airplane, if guided and propelled.

        _*Archive*_ no longer needs to see the information, so long as you can touch, say a book, It you can scan the contents of information. You can then do searches through its contents.

        _*Augury*_ can apply to the next 48 hours, and you can feel rough Bad Omens of significantly harmful events within the next month. By holding someone’s hand you can fish for omens specific to them.

        _*Foresight*_ extends 1.5 seconds ahead. With focused concentration you can actively play out a predicted series of events for up to 1 hour ahead, only a few seconds would have passed in your mind. However, it cannot incorporate unknown factors. If an unknown factor would alter events, those events appear distorted proportionally to how badly something Is interfering.
        """, """
        _*Identify*_: Identify now reveals True Names. Identifying a magic effect identifies the original caster. Identifies Complications and similar weaknesses. It identifies a person’s _Disposition_, their favorability towards you (Hostile, disliked, neutral, amicable, ally, low level love, high level love, etc.) or to others relevant to you or nearby.

        _*Status*_: Now reveals subversive statuses on enemies or allies. For example: “Casting Fireball, Doing homework, Walking to door, Contemplating murder”, and works with Map.

        _*Map*_: Any location you have previously mapped now remains as a live feed map you can observe through your map interface, unless the influence is wiped by some means such as dispelling magics, or the new spell _*Masking*_. Your mapping can also generate an intangible spirit body through which you can observe and map new terrain remotely, placed anywhere you have mapped before.

        _*Archive*_ can now archive the contents of documents like books remotely through map data, and record full sensory experiences that can be shared as a mote of light to playback information on contact.

        _*Lock-On*_ can now be applied from remote map viewing, and your locked on spells gain indefinite range or can manifest from somewhere in the sky over the mapped area, and you can line up a sequence of actions to take all at once, such as locking onto an arbitrary number of targets to target with a series of spells in order or at once.

        _*Masking*_ can purge local map data of any live feeds, and actively block it in mapping radius around you, or cast to alter or manipulate data that could be displayed to Identify or Status. You could instead alter map data to show things that aren’t there, or subtract something specific.

        _*Augury*_ can apply to the next week. You can feel bad omens of significantly harmful events within the next year. _Catastrophic_ events within 5 years. _Apocalyptic_ within 100, and you feel a rough idea of the broad strokes of what will happen without detail. You can use Augury on omens to try to narrow in on a sharper picture of the truth of what happens. You can now fish for _Good Omens_ roughly correlated to a subject held in mind, such as “Will I find love?” or “Will I get the job”, etc, generally limited to yes/no/maybe impressions.

        _*Foresight*_ can extend to 3 seconds ahead and includes tactile information, such as feeling an echo of a surprise attack yet to come from behind. You can predictively skim ahead up to a 24 hour period. Unknown factors now appear as local distortions around silhouettes of the unknown elements themselves, but you can roughly see what happens such that “A humanoid figure will appear the window in 3 hours”, however missing information can also be presented this way, such as a known figure being treated as an unknown because they won’t be there as expected. Your Foresight can also now be anchored to a bad omen to observe a 24 hour window of events centered on that omen. Actions taken can of course alter events, or reveal unknown factors on repeat viewings.
        """ ]
    }


aethernautics : Details
aethernautics =
    { name = Aethernautics
    , star = False
    , class = Just Academic
    , affinities = Regular [ All ]
    , dlc = Nothing
    , isElementalism = False
    , description = """
        Aethernautics is the magical studies of cosmological principles, planar physics, and spatial It’s field of magic used to safely explore the aether as well as long employed by astrologists and astronomers since ancient times to better glen insights from the stars above. Its utility in exploring space rather than the aether is a relatively more recent phenomenon, as space proves more difficult to navigate than the aether.
    """
    , ranks =
        [ """
        You learn the ability to receive the true name of cosmic bodies. If you see a star, planet, asteroid, etc, you can instinctively recognize its true name. You can name a cosmic body to instinctively sense its position in the sky and its distance to you. You gain a sharper mind for intuitively comprehending spatial geometry and distances.
        """, """
        You can now passively maintain the awareness of 3 cosmic body in your minds eye at once to have an accurate understanding of your position on a cosmic scale. Through meditation, you can isolate your body from the effects of the vacuum of space, from temperature and pressure, to radiation, and particle impact of mundane matter moving at over mach 8 up to light speed. By focusing in a direction when free floating, you can move in that direction at walking pace.
        """, """
        You can passively maintain awareness of up to 100 cosmic bodies at once for a sharper understanding of your position with ample room for tracking bodies for curiosity’s sake or any reason, which can now include any object you can see or are sufficiently aware of that is not fixed to a superior body (ie: A satellite in space, but not a building on the ground, or a flying plane. A moon, but not an astronaut or rover. etc.) unless it has some means of shielding itself from detection. Your free-floating movement is now up to the speed of your witchery broom, or you could of course simply use your broom itself. You now have spatial comprehension and magical connection sufficient that you can harmlessly create folds in space to link two points of space you can see, which lasts until you lose focus. If you have portals, this can be used for permanent fixed folds in space as a gateway. A fold is the size of a doorway and has no effect on the area that might conceptually be within the folded area itself. From the vantage of the fold, you can see space magnify or twist to the intended destination on the other side of the fold as though bringing that point in space closer to yourself.
        """, """
        You can maintain awareness of thousands of cosmic bodies. You gain the ability to exit an aetheric sphere, including the boundary of a pocketspace such as in witchery 5, to view a colorful space-like void of cosmic constructs, clouds, and crystal spheres- aetheric spheres. You can learn their true name the way you learn the name of cosmic bodies, allowing you to maintain relativity in this space beyond classical spatial awareness, to find your way in the infinite expanse of conceptual realities. Each sphere is another reality, or rogue demiplane, pocketspace, plane, or realm. If you know the true name of one such body or the creator entity who made one, you can comprehend its position to you allowing you to arbitrarily move through the aether to find that sphere, and unless protected, you can enter it.

        Moving dimensions is easier than moving in space, planar vs space travel, the difference between a dot moving across the page, vs bleeding up or down to other pages in a stack. Travel in the aether is easy as space does not truly exist here, your floating movement alone would be sufficient to travel great distances in its infinite expanse, as movement is based on intention, belief, and knowledge.

        You could now survive in the void with your meditation, but the void is not the aether, it is nonexistence, nothingness.

        The aethernaut’s space folding can now fold areas the size of a football field, creating large wall-like folds of space leading elsewhere, or creating geometries that can cover that area, such as pinching or twisting off that space entirely to fold the surroundings in on itself and hiding that football field sized area. Any aethernaut can perceive these twists and unfold them. This may also create areas of expanding space, such that a hallway could be twisted in a way that attempted movement continuously appears to elongate the space, or create repeating patterns. This can do shenaniganry such as turning around to see a continuation of what’s in front of you, where one came from lost. These twists can still be anchored as gates. Twisted areas interfere with teleportation and planar magics, preventing use within, putting an area on planar lockdown.

        Additionally, you can alter the relationship with gravity of yourself or another being or object you touch, redirecting it into a new direction relative to a new surface or point, which can be incorporated into twisted areas allowing relative gravity to any given surface, or static surfaces within the area.
        """, """
        Awareness of millions of cosmic bodies. If you know the true name of a witch of the same rank as you or less, you can breach the spheres containing their pocketspaces to force entry if otherwise protected. Once in, you can create normal portals to let others in.

        You can also lockdown pocketspaces, or demiplanes that are smaller than the size of Texas, or any Domain (including a Combat Zone), to prevent dimensional travel or teleportation effects until released. If another effect would dispel it or another aethernaut of equal rank to you tries to dismiss this lockdown, it is a contest of wills until one loses focus.

        The aethernaut’s folding can encompass the space of a large village or small town, and can include distortions in time- the area can be locked into a loop or paradox of repeating events, not affecting aethernauts with their meditation or warded witches. The aethernaut can now project a “spotlight” of starry ripples that visibly distort space, they can project their influence to alter gravitational relationships at a distance from themselves up to 60m in a narrow cone of influence, as though touching targets to alter their subjective gravity. Alternatively, they can affect local gravity to a surface by affecting that surface itself instead of a specific target’s own experience, turning a wall or ceiling into the floor, so to speak, up to a 1km area of influence.

        The aethernaut can now cause harmful sheers in space, these manifest as black distortions. warping space around them, and must start from the aethernaut’s location, traveling like a tear up to the speed of an arrow, and within line of sight of the aethernaut, leaving a the sheer behind as it travels. This will leave scars on space that aethernauts can detect. This shearing can be a Line like a thick cable, or a wall up to 1ft wide and 20ff tall. The length is arbitrary, growing until the aethernaut stops tearing. This bypasses any physical resistance, but will make you lots of enemies for its use - equivalent to causing a radioactive spill, including causing actual radiation comparable to an X-Ray per 15 seconds of proximity.

        The aethernaut at this rank is disjointed from coherent time, their body will not age and they can slide their body up or down to age themselves, this does not undo harm or illness, as though they were always that age to begin with prior to said harm. Should time be stopped by powers up to rank 10 equivalent, the aethernaut will remain active as normal, unaffected.
        """ ]
    }


firecalling : Details
firecalling =
    { name = Firecalling
    , star = False
    , class = Just Sorceress
    , affinities = Alternative [ [ Fire ], [ Metal, Wind ] ]
    , dlc = Nothing
    , isElementalism = True
    , description = """
        Elemental magic of Fire. Every rank in Firecalling reduces harm from Heat sources by 50% until you’re immune to natural sources of heat at rank 3, where you could lay on lava if you wished, but resistance to magical sources of heat continues to be reduced by half per rank. If you have the Pyre weakness, you do not benefit from this resistance except in respect to hot sunny days, hot sands, or other non-fire heat sources.
    """
    , ranks =
        [ """
        To start, you gain the ability to control candle size flames, including adding or subtracting candle size mass from a fire, within 30m of yourself. You can produce balls of compact flame in your hands that you can lob by hand within throwing range. They burst on impact with very little kinetic force but the flames spread like a molotov, around a 1 meter splash. Has negligible mana drain.
        """, """
        Control grows to the size of a torch within a 100m radius of yourself, which includes the balls of flame you can summon, which you can now telekinetically manipulate to do things such as put them in orbit around you, hover nearby like a handless torch, or just as self-propelled firebolts. They now have kinetic force equivalent to a solid human punch and the spread of flame grows to cover a 3 meter area. You also gain the ability to hold your hand out and cast a very wide 160 degree 5 meter cone of flame hot enough to cause 2nd degree bums in 2 seconds of exposure, 3rd in 4.
        """, """
        Control encompasses the size of a beach ball at a distance of 1,000f/300m Your firebolts can now have a 5 meter splash with force equivalent to a horse’s kick. The burning hands cone grows to 10 meters, and you can now conjure a short lived ball of flame the size of a beach ball that actively ignites flammable materials within 5 meters of it while leaving a 1 meter wide trail of flame 3 meters tall wherever you make it travel via your control, that’s as hot as the burning hands while obscuring vision. You can use jets of flame to boost jumps by up to an additional 5 meters or doubling the force of a melee impact.
        """, """
        Control is now the size of an elephant or large truck at a distance of 1 mile. Your firebolts burst with the force of a 40mph car crash spreading flame within 10 meters. Burning hands cone grows to 30 meters. It and your wall of fire are twice as hot. The wall is 2 meters wide and 6 meters tall. Your boosting jets add up to 15 meters of jump or sustain a glide, and can triple the force of a melee impact. You can now concentrate flame into a pinpoint of heat in front of your hands, fingers, or eyes, to project a thin searing ray of fire that can melt a hole through steel at a rate of an inch per second with mana drain comparable to the stamina loss of jumping jacks.
        """, """
        Generate fire out of nothing (No air?). Control fire in an area equal to a public pool at once at a distance of 10 miles, meaning you could very quickly bathe whole city blocks or forests in an inferno, or rapidly suppress the blaze of a building on fire. Firebolts are now full fledged fireballs with the force of a 60mph car crash with 20 meters of flame. You can set preset flight paths for them to take that they will continue to carry out until they are stopped by something or you dismiss it. Burning hands has a 50 meter cone. Wall of fire is up to 4 meters wide and 12 meters tall. Both are 3 times as hot as rank 4. Boost jets can now provide flight 120mph or flight (or boost flight by 120mph) or quadruple the force of a melee impact.

        You now lear to create self-sustaining feedback loops of flame as spheres that grow in power with time and distance, swelling from the size of a pinpoint bead of fire, to swirling miniature suns up to around 300m in diameter, doubling its current size per 10 meters traveled or 1 second of flight, whichever is slower. They burst with a shockwave that can level brick walls, in an area of 3x the size of the ball, with flames hot enough to melt steel beams in 6 seconds, though the fireball of the explosion only lasts 2 while igniting things in the area. Twice as hot and forceful within the area directly hit. The primary use case of course, is to cast these beads to the sky, and let them rain down on an area for bombardment. High mana drain. Low mana capacity could launch 2 before bottoming out. Med-5, High could launch 12. (But the Low likely has high charge rate to sustain a light bombardment for longer over time). Yes this was a long roundabout explanation of how to cast a classic Meteor Strike/Swarm, though that’s a bit of a misnomer as meteors are rocks. Performing this on Earth is almost guaranteed to be a masquerade violation. Curses 5 can at least pass as natural disasters.
        """ ]
    }


windkeeping : Details
windkeeping =
    { name = Windkeeping
    , star = False
    , class = Just Sorceress
    , affinities = Alternative [ [ Wind ], [ Nature, Soul ] ]
    , dlc = Nothing
    , isElementalism = True
    , description = """
        Elemental magic of the winds. Every rank in Windkeeping reduces harm from air pressure sources, electrical currents, and your need for air by 50% and provides a buffer against winds until you’re immune to natural sources of air pressure at rank 3, where you could comfortably survive in the vacuum of space and natural lightning (and radiation) causes you no harm, but resistance to magical sources of winds and shock continues to be reduced by half per rank.
    """
    , ranks =
        [ """
        Control a light breeze comparable to a common house fan within 30m, and whether it is warm or cool, like a hot or chilly day. You’re able to concentrate winds in a short range of your hands to act as though you had a mundane knife or hammer in hand. You can launch projectiles equivalent to a basic slingshot. Negligible drain. (A Sylph, but not Like a Duck, could be light enough to use this wind control to fly)
        """, """
        Control a blusterous breeze comparable to several high power fans, that can be hot enough to redden skin and dry moisture or cold enough to form a light frost where there is moisture, within 100m. Your control of concentrated winds can now emulate full size swords and sledgehammers, or leave stabilized air patterns equivalent to mundane objects such as a chair that can’t be seen, similar to Pantomime. You can launch projectiles comparable to a 501b bow, or like a strike with a baseball bat. You’re able to generate a static charge on demand, sufficient to make hair stand up and have a minor zap when you touch a doorknob. You can reduce your weight to become lightless enough that you can move yourself with your wind control with light mana drain, or no mana drain with Like a Duck.
        """, """
        Control a hurricane force gale capable of knocking over vans or people within 300m, equivalent to 6-10 jet engines in total wind at once. Concentrated winds have the same effect as before but you can now project your own wind constructs like projectiles; Launch wide blades of wind or narrow thrusts, or blunt concussive force, or spinning buzzsaws of wind. You launch projectiles with the speed and force of common firearms or cannons for the case of larger projectiles. You can produce electrical charge equivalent to a tazer you can apply to your wind constructs or on touch. You are now immune to G-Force, allowing for high acceleration to launch yourself with winds, or sudden direction shifts.
        """, """
        Control a strong cyclone force wind capable of lifting common street vehicles out to a mile away, equivalent to a somewhat slender tornado in total wind at once. If a cloud is in Teach you can alter the temperature to cause it to rain or hail like a light storm. Concentrated winds can now form large masses such as a blade up to 10 meters long or a mass of blunt force equivalent to a car. You can launch projectiles with the force of high power rifles, or throw larger things like a car up to around 10 meters or a person a few dozen, or yourself to provide an explosive burst of speed. You can create small sized real tornados with a mild mana cost that act independently or under influence of your wind control, more like very strong dust devils. You can produce electrical charge equivalent to touching a standard power line which could stop a human’s heart or be used like a defibrillator.
        """, """
        You can now generate air out of nothing. Control a unnaturally strong gusts of wind capable of toppling brick walls let alone ripping roofs off out to 10 miles away, equivalent to a somewhat thick tornado in total wind at once. You can move, burst, or gather clouds in range, your range increasing to be more around 30 miles vertically and 20 diagonally. By skewing the temperature and using hot winds below to send more humidity up to the clouds, you might sculpt storms and provoke rain, snow, or hail up to a heavy storm or blizzard. You can charge these clouds to cause thunderstorms. Your wind constructs can be up to 30 meters long or have the blunt force of a bus. Launch projectiles like high power artillery. Charge targets you can see such that lightning strikes gravitate towards them like a lightning rod, or your own electricity from within a few meters of yourself, generating charge in the air that then bridges the gap or as imbued into your wind constructs, being as potent as a natural lightning bolt. Create proper tornadoes on demand of normal size, or twist and build clouds until you can brew large supercells that can encompass towns.

        I’m sure I needn’t remind you about the masquerade violation that would occur if you mess with the weather on Earth let alone take a pet tornado through a walk in the city. But with careful tact, at least it could be less unnatural than giant fireballs.
        """ ]
    }


waterworking : Details
waterworking =
    { name = Waterworking
    , star = False
    , class = Just Sorceress
    , affinities = Alternative [ [ Water ], [ Life, Wind ] ]
    , dlc = Nothing
    , isElementalism = True
    , description = """
        Elemental magic of water. Every rank in Waterworking reduces harm from water pressure and sources of cold, and protection against water by 50%, until you’re immune to natural sources of water, water pressure, and cold at rank 3, where you could comfortably survive in the deep abyss of the ocean or in the middle of a blizzard, but resistance to magical sources of water or cold continues to be reduced by half per rank.
    """
    , ranks =
        [ """
        Control water equivalent to 1-2 cups, or a small water balloon, within a 30 meter area. You can spread it out over a volume of a 5 meter thin mist, or condense such an area of air moisture into water. Useful for instantly drying yourself out of a shower. (Maybe multiple uses to dry clothes) Within 5 meters of yourself, your influence is strong enough can cause this water to form high pressure edges or points as you force it into miniature riptides to cause cutting or piercing damage comparable to if you had a knife or needle.
        """, """
        Water control can cover up to a gallon, or moisture in a 10 meter volume within 100m. You can now force a change in state to solid, similar to changing the state to gaseous. This false ice is just a few degrees colder than it was as water, it’s just stabilized into a solid. You can form objects out of this ice though they’ll rapidly melt if you stop maintaining the effect passively. Your offensive reach with riptides increases to 15 meters. You can form pressurized spheres of water that burst when you stop concentrating on it, with the force of having a thick foam mattress thrown at someone and drenching everything in a public pool’s worth of water, sufficient to knock most people down and douse large fires. This water is spontaneously generated false matter that will slowly disappear as if rapidly evaporating over 10 minutes, but is viable for your water control. Can breathe water.
        """, """
        Control up to a home pool’s worth of water, or moisture in an area equivalent to a football field within 300m. Forced ice can now be below freezing in temperature, and when you would turn water into vapor you can heat it into steam, or you can force ice or steam to remain a liquid well past boiling or freezing points. Down to 0 degrees F, or up to 250 degrees F. Your riptides can reach 60m away, and your pressurized water bombs now have the force of a bulldozer in up to a 30 meters with an olympic pool’s worth of water at heavy mana cost, or just the public pool’s worth for a medium cost but the same force. You can now force natural water to double itself, easily causing an overflow or solving a town’s water shortage for a time, this is true water. With your water control you could easily move more water than it would suggest to create whirlpools and maelstroms.
        """, """
        Control the equivalent of an olympic pool and all air moisture within a mile. Ice/Steam/Water can be -200f or +450f. Riptides reach within 300m. Water bombs and water duplication are now a sustained effect continuously generating water until you stop concentrating on it. Your water control can easily pressurize water with the force of up to artillery shells, your icicles could pierce concrete walls. You can easily control water with precision such that you could walk on the surface of water or lift yourself on columns of water or ice, or surf on clouds of intense steam, skate on ice created beneath your feet, or so on.
        """, """
        You can now generate water out of nothing completely on demand without relying on water bombs and all water created is true water. Control the equivalent of 10 olympic pools. at once, within 10km. Your ice can reach absolute zero and your steam can be up to 1,000f. Riptides match the full reach of your water control. Bursts of water can match modern conventional explosives while your icicles could have the force (and hardening) to pierce iron bunkers. You can set up walls of water that prevent the movement of water past it, or act as currents flowing in a given direction. You could use this to create reversed waterfalls for example, or "Ceilings" of water below the surface, or otherwise creating dry areas where there was water, to walk across lake bottom or just prevent water from flowing back to fill an area where you’re working on something. These remain stable within range of your control, by maintaining one for 24 hours you can make them permanent until dispelled, or after 1 year of sustaining they will return even after dispelled, being just a temporary disruption.

        You are now able to pent up control over water to store produced water to release all at once, or influence large bodies of water all at once with your control set ahead of time for when you release it. Just as you can create, you can destroy, and destroyed water can be converted to stored water to contribute to sudden releases. With set up, you could easily cause major tsunamis affecting whole coasts, or with extra care you might sculpt it just right to afflict only one city or one harbor. Do mind the masquerade.
        """ ]
    }


earthmoving : Details
earthmoving =
    { name = Earthmoving
    , star = False
    , class = Just Sorceress
    , affinities = Alternative [ [ Earth ], [ Nature, Body ] ]
    , dlc = Nothing
    , isElementalism = True
    , description = """
        Elemental magic of earth. Every rank in Earthmoving reduces harm from earthen materials and any source of blunt force trauma by 50%, until you’re immune to natural at rank 3, where you could survive a fall from any height if you landed on earth, including concrete, and a falling boulder would do no harm to you (You decide if you meld into it, it bounces off like styrofoam, or shatters), but resistance to magical sources of earth continues to be reduced by half per rank.
    """
    , ranks =
        [ """
        Control a simple bucket’s worth of dirt, sand, mud, and marble-sized pebbles within a 30 meter area. This can be in a single loose mass, or up to 4 loose masses at once. You can blast this earthen material with force equivalent to dumping a bucket, or a slingshot in the case of focusing on single decent sized pebbles. You can pretty effectively dig loose earth quite quickly, until you reach bedrock or large stones, and don’t diss the power of pocket-sand, or instantly cleaning a shiny rock and letting them orbit your head.
        """, """
        Control a bathtub’s worth of earthen material up to the size of golfballs controlled in up to 10 loose masses at once within 100m. The force is comparable to ball muskets and blunderbusses in bursts. You can do a quick chant and tap a larger stone to reduce its weight by 90% in your hands, it regains its weight when it leaves your hands. You can also use your bare hands on an affected stone to mold it as though it were clay.
        """, """
        Control a home poot’s worth of earthen material, individually up to the size of a basketball sized stone, within 300m. Force equivalent to a cannonball or sandblaster. No longer requires a chant to affect stone, just simple intentions, and you can use your earth control to mold stone as though it were clay or rip out chunks of a size you can control. With a specific spell that takes a few seconds to focus on, you can cause slabs of stone to erupt from the ground 1m thick and 15m long, or up to 8 individual lances of stone, 4m tall.
        """, """
        Control a public pool’s worth of earth, about 4x rank 3, individually up to car sized stones, within a mile. You can hurl car sized stones comparably to a trebuchet, with your stones hitting with artillery-like force. You can rapidly shape stone into new shapes with enough sudden force behind movements that forming a spike can be comparable to a spear thrust, or forming many small spikes and causing them to launch like bullets. You can summon stone slabs like before, now without any more than a gesture, or focusing for a few seconds to summon stones from the ground that are up to Sm thick, 60m long, and 15m tall, or up to 30 individual lances of stone. You can also do the inverse to create fissures with twice the dimensions. You can likewise focus to build up turbulence in the earth causing quake in a 30m diameter area within control range that starts as a rumble. The force of the rumble and the dimensions double every few seconds of concentration until the shaking is enough to level basic unprotected structures after 4 minutes and max out at 2km. You could easily ride on propelled slabs of stone, or launch yourself into the air great distances.
        """, """
        Control a city block’s worth of earth with individual stones up to the size of small homes, within 10 miles, with force sufficient to throw such a stone with the speed of an arrow, while smaller stones might be simple to high caliber bullets. A stone lance could penetrate an iron bunker. You can also explosively multiply pressures inside stones to cause them to explode with artillery like force, sending shrapnel flying. You can now casually summon the slabs or fissures from rank 4, or focus for longer to create stone walls up to 10m thick, 300m long, and 40m tall, or up to 80 individual lances of stone, or fissures 20m wide, 600m long, and 80m deep. You can permanently rob the weight from stone in an area of a size you can control, leaving them weightless or lighter than air to an extent that you can create lift stones useful as cores to flying ships. You can meditate with a chant allowing you to increase the area you can influence with this effect by double per second, until you can affect up to a 10 mile area at once, allowing the creation of floating islands. Your earthquakes can now scale to reach 10miles as well with force sufficient that standing on the ground is like trying to ride a mechanical bull, most structures not protected by magical means of earthquake proofing cannot endure this. The earthquake can also continue independently of your concentration for up to 3x as long as you spent concentrating on it You can choose to not release this force while you concentrate to instead build up a sudden shock though with reduced area to a 5 mile area, which may be sufficient to pulverize a mountainside into a great landslide and may be a bit of meat grinder to anything within 2m of the ground surface during the intense few seconds the shock is in effect. Naturally, it’s strongly advised not to do this anywhere on earth...
        """ ]
    }


naturalism : Details
naturalism =
    { name = Naturalism
    , star = False
    , class = Just Sorceress
    , affinities = Alternative [ [ Nature ], [ Life, Water ] ]
    , dlc = Nothing
    , isElementalism = True
    , description = """
        Elemental magic of plants and woods. Every rank in Naturalism reduces harm from plant based materials and chemicals derived from plants or animals as well as diseases, and aggression of animals, by 50%, until you’re immune to natural sources at rank 3, where you could comfortably lay on a bed of poisonous thorns and the most belligerent of bears would be ambivalent or friendly toward you unless manipulated, but resistance to magical sources continues to be reduced by half per rank.
    """
    , ranks =
        [ """
        Control plant matter equivalent to a 3m of rope moving with about the speed and dexterity of a snake, within 30m. Focus on a plant to increase its growth rate by 25% for its current life or harvest cycle. By talking to plants during their growth, you can either boost beneficial aspects of the plant, or harmful aspects of the plant, by 50% when it matures. You can talk to intelligent animals like certain dogs, cats, dolphins, etc, and they somewhat accurately understand the meaning of your words. Includes Fungi
        """, """
        Control plant matter equivalent to 10m of rope or a very large bush, up to 3 independent plants or vines, with the dexterity of octopus tentacles, within 100m. Plant growth can be boosted by 150% for 2 cycles. Talking to plants can boost effects by 100% and can now work at any time in its life cycle up until blooming or ripening when you would harvest. Intelligent animals accurately understand your intentions and you get an idea of theirs. Less intelligent animals can get a general idea of what you mean in simple terms. You can take a single trait from one animal or plant, and imbed it into a seed or pregnant animal and the offspring or new plant will possess that trait. (Animal to animal, plant to plant)
        """, """
        Control plant matter equivalent to an aspen tree, up to 9 independent plants or vines within 300m. Wood can bend unnaturally, and the controlled plant matter can stretch up to 25%, with the speed of a snake’s strike and fully prehensile. In addition to the stretching, you can induce growth at a rate of 10% per second (Max new growth caps out at equal to your maximum plant control per second), and within the natural possibility for a plant to achieve. Any animal can understand you, gaining human level intelligence when you talk to it and while acting on something you’ve asked it to do. You can cause non-magical plants to bloom, produce, or spore early, on demand, with boosted effects by 300%. You can take and implant up to 2 traits at once into a new life.
        """, """
        Control plant matter equivalent to a mature spruce tree and up to 20 independent plants or vines. within a mile. The controlled plant matter is as hard as wood, or if wood then 5x as hard. Can stretch up to +100% of its natural reach or length. Vines could move as fast as a bullet, though not prehensile, moving more normally to perform fine actions or grapple. You can actively induce growth as you desire it up to the speed of control, and these plants can grow up to twice as large as normal before factoring stretch. Includes the size of produce and flowers, etc. You can permanently Awaken animals to full human intelligence and implanted with 1 “human” skill you wish at time of awakening. You can do the same with plants. You can implant up to 3 traits at once, one of which becomes Hereditary, passed on, or one trait into a currently living creature or plant.
        """, """
        Control plant matter equivalent to twice the size of a mature Redwood tree and up to 60 individual plants or vines at once, within 10 miles. Wood is 16x harder under your influence, stretching up to 500% natural reach. Plants under your influence have 600% greater size than normal before accounting for stretch, only regarding height- Vines, roots, or branches have no maximum length so long as supported. Vines can exert sufficient force to claw into stone bedrock and you can spontaneously produce any other plant feature you have come across onto any other plant you are controlling. Make a great tree branch behaving like a vine grow large toxin filled thorns, or bloom large mushroom. caps producing clouds of paralytic spores. You can cause produce to grow out of immature plants, ie; An apple seed growing a single shoot that ends in a ripe apple that has the health benefits of ginger. With momentary focus and a brief chant, you can divide your plant mass control limit into countless individual plants such as single blades of grass, and have each grow fully into a mature tree, even individual redwoods, that each bypass your control limit for the purpose of near instantaneously growing to full mature size and given one command to continue to act on. Any awakened plant can control itself as though by your level of control as far as its speed and prehensile dexterity is concerned, though its growth remains as you last left it. You could very easily cause massive overgrowth to overtake a city with streets patrolled by treants. Use with caution to avoid breaking the masquerade, at least while on earth. Plant control is the active influence, an entire plant need not fit in the limit, just the part you’re controlling.
        """ ]
    }


metamorphosis : Details
metamorphosis =
    { name = Metamorphosis
    , star = False
    , class = Just Sorceress
    , affinities = Regular [ Beast ]
    , dlc = Nothing
    , isElementalism = True
    , description = """
        Metamorphosis is a school of magic built around shapeshifting the self. While Hexes can be used on yourself as well, Metamorphosis focuses on invoking inherent natures within the witch. As such, it is exclusive to witch types that possess the [Beast] affinity or witches with Improved Familiar, in which case this form is what the familiar itself can take. Siren, Naiad, Oread, Lamia, Gorgon, Taura, Mimi, Wulong, Dravir and Xeno. Naiads contain the form of the _*Leviathan*_. Sirens, Oreads, Taura, Mimis, and Pharon contain the form of a _*Zooarch*_, Lamia and Gorgons have the form of the _*Basilisk*_. Wulong, Dravir and Xeno have the form of the _*Dragon*_, asian long, european classic and Xeno respectively. A Hybrid can adopt the form as normal. A Hybrid of 2 Beast types chooses one unless they buy this again, though they can _visually_ adapt its appearance to be a mixed form. If they bought it twice they can turn into either, or tum into a full hybrid beast. Likewise if you have Improved Familiar and qualify yourself, both the familiar and the witch could have a form, bought separately. {choice _Each Metamorphosis form is considered Rank 5 magic and costs 15 points as though it had all 5 ranks as such, meaning it costs 9p if you have the Beast affinity (which you should to be eligible for it in most cases)_}. It takes a flat rate of 20% of your mana capacity to assume the form or trigger your familiar’s form, which can be indefinitely maintained. Transforming back costs nothing and refunds 10% the mana cost.
        """
    , ranks = [ "", "", "", "", """
        *Leviathan*: _Can also be available to Taura with aquatic animal halves._ Colossal terrors of the ocean, leviathans are over a mile in length and have the width of 5 blue whales. Their appearances beyond that vary a lot. They may or may not have a roughly humanoid “head” portion with or without a portion of the humanoid torso, comparable not to a taura or naiad, but more like a sphinx. They often have a number of tentacles and fins concentrated around the “head” region of their length and then lightly spread throughout the rest of their length, which is scaled with diamond-like scales the size of dinner plates. Extremely hard, but could shatter with heavy blunt impacts effectively ignoring damage short of an armor piercing missile, but once overcome provides no protection in that spot. They are uncomfortable to the point of pain when in normal or low atmospheric pressure, at home in crushing oceanic depths, immune to high pressure and temperature extremes of water. Inversely, they’re actually also comfortable in vacuum despite their issues with low pressure.

        {center} [OR]

        *Zooarch*: _Can also be available to Naiads that would rather be an aquatic animal, and Lamia/Gorgons who’d rather be an actual snake_. Assume the form of a great animal beast, like primal avatars at the apex of a species. They look obviously similar to the base animal but with any number of additional modifiers such as horns, spikes, scales, markings, abnormal color. They can be normal sized, or grow up to the height of a 12 story building in their natural gait. (Could be taller if they stood on hind legs for example, if a quadruped). Their hide, scales, or exoskeletons are averaged out whatever the case to be as resistant to harm as steel armor of equivalent mass (to the hide and fur, scales, ect). They have proportional strength. Predators have increased physique 200% extra than you’d expect for their size, herbivores have increased magic effectiveness by 20% per 100% size. Omnivores/non-predators that eat meat split the difference at 100% / 10%. Siren Zooarchs can be capable of flight, such as great eagles or thunderbirds.

        {center} [OR]

        *Basilisk*: Worshiped in some places as serpent gods, the basilisk has the lower body of a lamia and the upper body of a gorgon, and 4 or 6 arms. They’re roughly 100m long while the humanoid body is like that of a giant. Each of the snakes of their hair has a gorgon’s gaze while being the size of a full grown anaconda while the basilisk’s own sight causes neurological activity to stop outright causing death in most mortals while beings that don’t need organs might simply mindblank as though time were stopped as far as their mind is concemed. Their scales are like steel plating. Each snake head can assume the focus of any ongoing magic effect that would require the witch’s concentration.

        {center} [OR]

        *Dragon*: Transform into a scaled beast the size of a A380 airliner in the case of Classic European dragons, or as wide as a nuclear submarine and as long as an aircraft carrier in the case of Asian Longs. Their scales are equivalent to 4 inch thick mythril plating and they have an elemental breath of flame [Fire], lightning [Wind], cold and ice [Water], or metal shrapnel [Metal], poison gas or wood splinters / thorns [Nature]. If dravir, this is the same breath affinity as they already chose, a wulong gains one now when in this form, and can keep it when not in dragon form, not having an additional affinity from it. A familiar can choose one when this is taken. This breath affects a 300m cone, 1,200m line 5ft wide, or lobbed bursts within 1,200m that burst for 60m diameter. The resulting damage can melt a tank in 2 seconds (Fire or lightning), freeze the tank into brittle glass (water(ice)), or have the raw force to crumple or shred a tank (Ice, water, or metal). They fly at 300mph by default, adding your Witchery broom speed if applicable. 

        Xeno Dragons do not have wings, but they gain incredibly strong acid blood and sweat that can be produced on demand to produce an Acid based “breath weapon”, and allows them to very easily burrow through the earth leaving unnaturally smooth and hardened walls within the tunnels they leave. The acid breath in an area remains dangerous for 5 minutes, melting through the ground at a few feet/min
        """ ]
    }


psychotics : Details
psychotics =
    { name = Psychotics
    , star = False
    , class = Just Sorceress
    , affinities = Alternative [ [ Mind ], [ Life, Soul ] ]
    , dlc = Nothing
    , isElementalism = True
    , description = """
        Elemental magic of the psyche. Every rank in Psychotics reduces harm from memetic hazards (Which includes observations of intrastellar phenomenon and Outsider Entities), psychological trauma, telekinetic assault, and psionic constructs by 50%, until you’re immune to natural sources at rank 3, but resistance to magical sources continue to be reduced by half per rank. Unless otherwise specified, all Psychotic effects require minor concentration that doesn’t take much effort.
    """
    , ranks =
        [ """
        Control psychic vibrations sufficient to make small objects disappear from one sense, up to the size of a softball, in one person’s mind. You’re able to make the same small objects instead look like another object. Make an 2 of hearts look like an ace of spades, or a pokemon card. You can also twist psionic wavelengths in someone’s mind sufficient to cause a headache, which you can link to an associated pattern in the brain, such as the sight of a chair or a certain thought.
        """, """
        You can now affect objects up to the size of a beachball disappear from two people’s minds, using two senses. You can likewise induce headaches (conditional or not) into two people at once, with two different conditions if desired. The headache is now strong enough to be strongly uncomfortable and lightly disorienting. You can now manifest psionic constructs that behave like any mundane uncomplex object, within 5 meters of yourself. Such as a psychic dagger. You can physically interact with your own psychic constructs, but they phase through other objects and creatures: Doing so disrupts neurological processes, making an arm feel numb for example, and more clumsy, as though intoxicated, especially if the psychic dagger passed through their brain. This is considered a direct magic effect, blocked by Warding runes. Lasts up to 12 minutes.
        """, """
        Psychic vibrations affect up to the size of the average car from up to 6 people’s minds using 3 senses. Induce headaches in up to 6 people at once, with any number of optional Conditions. These headaches are now dizzying migraines. Psychic constructs can now exist within 30m of yourself and can include projectile objects like psychic bows and arrows, or firearms. Numbness lasting up to 1 hour. You are now capable of localized twists in the psychic fields to switch your position in space with that of a space you can see within 60 meters. Teleportation.
        """, """
        Psychic vibrations affect up to the size of a suburban house from up to 30 people at once, using all 5 basic senses. Induce headaches in up to 30 people at once. These headaches cause the eyes, ears, nose, and mouth to bleed lightly and are maddeningly painful. So long as the headache has a conditional modifier set, it can also be made permanent, removable by magic that can cure mental illnesses or curses. Psychic constructs can now exist within 90m of yourself, and now instead of numbing, an affected nerve or limb can’t be felt at all as though it weren’t there. A headshots lights out. Lasts up to 12 hours. You can teleport within 1 mile. At rank 4, you’re now capable of inserting a psychic construct into someone’s brain to view their mindscape, a realm of thought and memory where you can explore to find memories and opinions in networks of associations. If you use psychic constructs within here to destroy something, it is forgotten, or you can use your ability to twist psychic vibrations to create new associations and false memories.
        """, """
        Psychic vibrations affect up to the size of a skyscraper, and is now a passive effect generated by a field you imbue the object itself with, allowing it to affect any number of observers and without your concentration. Your “headaches” can now cause people’s heads to explode, which with conditional modifiers you can make give painful warnings and a number of “strikes” before the big pop. Psychic constructs can exist anywhere you are able to observe, and you can teleport within this same range. You can now do mass edits to a mindscape to edit things along basic conditions or filters. You can blanket nuke a mindscape for complete ego death. Though mindscapes are backed up in the soul, and could be restored by some healers and the soul might “Bleed” emotional memory. Likewise you can fabricate an entire identity and false life within them. In a similar vein, you can enter the psychic plane of existence via your teleportation to see an ethereal realm similar to the spirit world, full of emotional constructs, echoing memories, and thought patterns of individuals living in the material. A ghost like mirage of yourself echos your position in the material world, so you aren’t invisible. In this place if you use your psychic vibrations to shroud a person or object from the psychic plane at large. Doing so erases memory and knowledge of that person or object from any unprotected mind. So most mortals, and new or neglectful witches who don’t have a warding rune or psychotics who created a shroud around their own mind, protecting them from this and constructs.
        """ ]
    }


metallurgy : Details
metallurgy =
    { name = Metallurgy
    , star = False
    , class = Just Sorceress
    , affinities = Alternative [ [ Metal ], [ Fire, Earth ] ]
    , dlc = Nothing
    , isElementalism = True
    , description = """
        Elemental magic of metal, of Iron and Steel, of Gold and Silver. Every rank in Metallurgy reduces damage from metal sources by 50% until you’re immune to natural and primitive sources at rank 3, so simple metal weapons will not harm you, neither will small arms fire, but resistance to magical or high-power sources of metal continue to be reduced by half per rank. If you chose to take the Iron vulnerability, then it ignores this.
    """
    , ranks =
        [ """
        Control a handful of coins worth of metal, moving and levitating it with comparable speed to physically moving it around with your hand, within 30m. You start out with the ability of cold forging, any metal under your control you can freely weld and merge to other metals, easily welding metal to metal or patching damaged metal objects, or sharpening blades.
        """, """
        Control metal equivalent to a longsword, with speed comparable to an arrow, within 100m, moving beyond that on its own power if thrown with this effect. Your cold forging is now seamless, as though part of the original form, and now you have a sense for metal within your control range larger than a coin. With focus you can sense and control fine metal traces within loose material like dirt and sand, and extract it with your control and turn it into something usable with cold forging, such as needles or outright blades.
        """, """
        Control metal equivalent to a full knight’s armor- And that of an armored warhorse. Or roughly a car’s worth of metal, within 300m. Large projectiles like the car at throwing speed, while you bullet sized objects can be moved with common bullet speeds (9mm). Your cold forging can process raw ores into pure metals, ingots or otherwise and you can draw metal out from stone at a depth of 3ft/ 1m, rather than just dirt or sand. With time you could separate alloys into their base components, or cold forge alloys from base metals. At this rank, you can also learn to create a form of puremetal gunpowder devised by a the first gunwitch, John Moses Browning. You can incorporate this in your creations to create cannons or actual firearms, it may take some skill to consistently get the right ratios both for the powder, mass you want to move, and the strength of a given barrel. You can produce a handful of powder at once with a minor mana cost, bear that in mind for if you want to quickly construct a cannon.
        """, """
        Control metal equivalent to a 3 busses or a cohort of mounted knights, within 1 mile. You can hurl large full metal ballistas at the speed of a bullet, or small projectiles at high velocity rifle speeds. You no longer take any extra effort to split or meld alloys, becoming a standard part of your cold forging, and you can draw and extract metals from throughout the environment within your control range, filtering trace metals from deep in the ground beneath you for example. You can now form motion metals, an active infusion that allows solid metal to actively mold and conform to movements instigated from one side of the metal, such as from within a solid armor encasement without gaps or the need for joints. You can now create a barrels worth of puremetal powder at once and learn to infuse the powder to magnify its explosive potential by a factor of 10. You could easily create a solid row of cannons on demand, presuming you master the skill in precisely calculating the ratios still.
        """, """
        Control metal equivalent to a train 25 cars long, within 10 miles. You can hur ballistas with the speed of a rifle bullet, or small rods at extraordinary velocities- railgun shots. Your cold forging can now forge metals out of thin air, materializing metals as you need them without having to draw on existing metals, though drawing on existing metals is less mana intensive. You can now make motion metals that respond to any kinetic force and feed in on itself to act in a designated manner, such as self-reinforcing to make a thin sheet of metal provide the resistance as a much harder and thicker layer of metal, transferring some of the force of an incoming attack into its own resilience, or a motion metal that applies the force of its own movement into additional rotational energy for continued acceleration, forming drill or saw effects or emulating rifling for either increased speed and flight paths, r for more damaging attacks from cutting buzz saw motions, or piercing spin of a drill lance, or forgoing the theatrics, simply amplify the force of a normal blade as though it were up to 10x its actual weight. The mana expense is low but it all stacks up. You can of course leverage your metal control to do similar drill effects and movements, but motion metals are just more empowered and take less of your own focus, to the extent that if you overdo it it could be difficult to interrupt. fou can use motion metals to form the barrels of guns and cannons to resist far stronger explosions within to handle far stronger projectiles, and a motion metal cannonball may even reach the moon, or you could create an normal array a full heavy bombardment of cannons, creating over a dozen barrels worth of puremetal powder at one time. Additionally, you can cold forge metals into roughly similar metals. You can turn Lead to Gold.
        """ ]
    }


lifeweaving : Details
lifeweaving =
    { name = Lifeweaving
    , star = False
    , class = Just Sorceress
    , affinities = Alternative [ [ Life ], [ Fire, Soul ] ]
    , dlc = Just "Loose Assets"
    , isElementalism = True
    , description = """
        Lifeweaving is the pseudo-elemental magic of Life, encompassing a spark of genesis from the moment of creation, it can also be considered the Light element. With Life magic, every rank increases your natural healing factor by 200%. At rank 3, you will never scar and lost organs or limbs can be recovered. At rank 5, you will continue to regenerate from the point of death so long as a living cell remains. If you have a means of cheating death that would be faster, you can choose to default to that first.
    """
    , ranks =
        [ """
        Control motes of light comparable to a few dull LED points of light, which can be used to banish rank 1 curses and heal superficial injuries from scratches and papercuts to small bruises, and heal sicknesses from bacteria or viruses that have not yet shown symptoms. These motes can also purify food or drink from bacteria or viruses.
        """, """
        Control motes of light comparable to a flashlight, which can banish rank 2 curses and heal minor injuries from simple lacerations to decent sized bruises, can heal sicknesses that have started to show symptoms, or other forms of disease that have not yet shown symptoms including a genetic flaw, so long as you’re aware of it. These motes can purify food and drink from any natural contaminate. Can now cause intense flashes of light sufficient to blind onlookers for a few seconds, which directly harms undead and demons similar to as though it were a rank 2 Fire spell with the added advantage of effectively having no travel time, though this only applies within soft
        """, """
        Control motes of light comparable to a spotlight, which can banish rank 3 curses and heal moderate injuries from lacerations to significant bruising, can heal natural diseases outright, and magical sourced diseases caused by up to rank 2 magic, including curing genetic conditions or body functions gone out of control. You can cause intense flashes of light that can permanently blind unless you wish otherwise, and can cause burning damage equivalent to rank 1 fire magic, while the light harms undead like a rank S fire spell, damage applying within 100ft. Can now bathe a target in a warm soothing light that reveals any curses or ailments with both a visual cue and a sixth sense in your mind. This light also provides pain relief lasting up to 8 hours, and calms anxiety or emotions of despair and fear.
        """, """
        Control motes of light comparable to stadium lighting, which can banish rank 4 curses and heal severe injuries or heal any disease of a natural source or magic source up to rank 3. The flashes of light you cause can affect 300ft areas and you can focus the flash into a cone shape up to 600ft long, SOoft wide at the end, which you can maintain like a spotlight. This flash within this area can optionally cause harm as though it were a rank 2 fire magic, while harming undead or demons as though it were rank + Beyond the given range, it’s just normal light with normal behavior. The soothing light now can now be distilled into a drop of liquid light which can be applied to a form of bath, hot springs are favored, to produce healing waters with a deep soothing effect that provides pain relief for up to 24 hours and the waters itself will remain potent for 6hours. Lifeweaving can now be used to generate hardlight objects emulating any mundane object, or enchant existing objects (even hardlight you made) with the effects of the Sun Shard relic. Hardlight objects you make can be manipulated with your will similar to telekinesis, allowing a floating shield as though supported by your own strength, or simple halos of light that envelop existing objects to move them. Hardlight can emulate projectile weapons but they destabilize outside of 300 meters.
        """, """
        Control motes of light comparable to the light of day, which can banish rank 5 curses and heal critical or would-be fatal injuries and any disease including from magical sources up to rank 4. Flashes affect 1,000ft areas or 1 mile long cones, which harm like a rank 3 fire spell if desired, or a rank 5 harm to undead and demons. Flash lighting can now heal allies with the rank 3 healing tier, even as it’s harming enemies. Soothing light can permanently alleviate currently existing sources of chronic pain, physical or mental, and can banish Psychotic manipulations. A distilled drop can permanently enchant a spring’s waters with its effect, until that water is too dispersed or more than a mile from the source. Alternatively, you can use a distilled drop to anoint a body in a 30 minute ritual to resurrect the dead so long as the body still has flesh. Hardlight objects are 4x as durable and can now replicate soft features, such as clothing, with dynamic movements and abnormal material properties. With a 5 minute ritual, you can call down the light of the sun to bathe an entire city in the effects of the Flashes, sustained for up to 24 hours. Can stack these rituals to intensify the effect.
        """ ]
    }


visceramancy : Details
visceramancy =
    { name = Visceramancy
    , star = False
    , class = Just Sorceress
    , affinities = Alternative [ [ Blood ], [ Body, Necro ] ]
    , dlc = Just "Loose Assets"
    , isElementalism = True
    , description = """
        Visceramancy is the elemental magic of Flesh. With each rank, you become resistant to forced transformations up to your rank in visceramancy and immune to those from 1 rank less, resistance means the duration is halved or if there is no duration, it gains a duration of 5 minutes. You can focus to eat away at the duration faster at 1 minute of duration per second of focus. At rank 3, you cannot scar and healing effects can always result in the return of bodily function, limbs can be reattached or organs placed back. At rank 5, your body will always maintain functionality regardless of missing components. If your heart is removed, it would keep beating on its own, and your body would operate without it. You can still see and feel from removed parts. ie; placing an eye somewhere lets you keep sight of that area, for example.
    """
    , ranks =
        [ """
        Control or generate strands of flesh comparable to a 10ft length of twine, 1 length or multiple adding up to it. You can use these strands as flesh sutures to stitch wounds shut- or mouths, eyes, or stitch your initials into someone for fun, you do you. They pierce flesh as easily as fine needles and move from a point on your own body out to within that 10ft reach. You can also use these as extra fingers for manipulating objects.
        """, """
        Control or generate flesh comparable to an adult human limb in a rough lump sum mass, or a 50ft length of twine that can be up to the thickness of a rope and just as durable. Your generated flesh can include nerves that feed sensory information to you, for now just the sensation of touch. You can project lumps of flesh with the force and distance of a slingshot, which can then disperse into twine or rope to grapple. The mass or ropes may or may not drip blood.
        """, """
        Control or generate flesh comparable to an adult human body in a rough mass, or 100ft length of twine that can be up to 5x the thickness of a rope as durable as a wood branch. Generated flesh can now grow a sensory organ any of the 5 senses, such as growing eyes that count as your own to look around a corner or through a vent. You can link the nerves of your generated flesh to the nerves of another to share sensations. When doing so, you can now control their flesh to grow additional nerves where you please and reshape their flesh adding or subtracting mass, and alter pigmentation to change hair, eye, or skin color. If you have necromancy, you can reshape their bones through this connection. With force, you could cause someone to tear apart with this effect or open lacerations. Note: Your tendrils or outright tentacles have the dexterity and speed of that of an octopus, while you must preprogram what your launched gobs of flesh will do ahead of time, without an active link to control it live.
        """, """
        Control or generate flesh comparable to an elephant in rough mass, or S00ft lengths of tendrils that can be up to the thickness of an adult limb and as durable as aluminum. You can now grow sensory organs of senses that exist in nature, such as eyes that see other wavelengths of light, see better in night, ears that hear new frequencies, or change existing sensory organs in a linked target to exclude a certain wavelength. You can mend or cause flesh based conditions on a genetic level such that their offspring might have the same changes. You can resuscitate dead flesh, though a dead person would still lack their soul if not resuscitated within 10 minutes unless a witch whose cheat death method has not been invalidated yet. You can have your tendrils immediately interact with nerves such that they are not felt on contact.
        """, """
        Control or generate flesh comparable to 3 blue whales in rough mass or 1,000ft lengths of tendrils that can be up to the thickness of an adult elephant’s trunk and as durable as steel. You now remain connected to flesh you generate from any distance, and the globs of flesh you can launch can now have up to the speed and distance of a 9mm bullet (An effective range of up to 90ft or 30m, with skill). Your control over your tendrils is more dextrous and speedy now, being as fast as the movements of your own fingers or arms rather than the slower movements of an octopus. You could do it manually, but now you can set autonomous conditions for growths to achieve some goal or passive responsibility, such as an eye organ that only gets your attention when it sees something, specific or otherwise. A skin layer on the ground that responds to pressure to cause tendrils to emerge and do with the victim what they were told to do, and/or alert the visceramancer. Eyeball spiders are popular uses as well.
        """ ]
    }


arachnescence : Details
arachnescence =
    { name = Arachnescence
    , star = False
    , class = Just Warlock
    , affinities = Regular [ Beast, Necro, Mind ]
    , dlc = Just "Loose Assets"
    , isElementalism = False
    , description = """
        Arachnescence is the magic of spiders woven by the spidergoddess personally to bless her favored children. This magic governs the weaving of webs through mana, body reinforcement, providing a base ability set of Arachnes. The form of an arachne can be imitated by spider Taura, who are more simply a manner of drider, where the Arachne is made after the pattern of the spider goddess.
    """
    , ranks =
        [ """
        With rank 1 users obtain the ability to shapeshift into any mundane spider and produce the same web at the same rates that it does, have the same movement, ect, all its traits. They must have seen one of these spiders in person first. After assuming a form once, the witch gains one adaptive slot for applying a spider trait from any spider they’ve adopted, changing it when desired, but only having the one active. For example, maintaining the Black Widow’s venom outside of the Black Widow form, including other spider forms, or the ability to produce spider web even in the form of a neutral if that was their type. It can’t be major form altering traits like spider legs.
        """, """
        At rank 2 practitioners of this craft gain a second adaptive slot and the ability to shapeshift into supernatural spiders that don’t have magical abilities, such as giant spiders, dire spiders, ect. They can now grow a pair of gem-like beads as a secondary pair of eyes on their forehead which allows them to have a dark vision that sees in black and white but without requiring light. These eyes can notice things and warn you like a sixth sense when you might not have noticed them otherwise. Additionally, the witch can now climb on any surface that can support their weight, though they must properly support themselves so it may not look the most graceful.
        """, """
        Rank 3 brings with it a third adaptive slot, and the ability to partially shapeshift into the various available spiders but for now it’s limited to replacing an existing body part with an equivalent, without adding extra parts. Spider parts are more durable, like wearing wooden armor. They grow a second pair of spider eyes which provide thermal vision. Regardless of adaptive slots, the witch is now able to produce webbing from their body in any 2 different ways you wish, functionally identical to what you’d expect from spiderman. Sticky ropes behaving like grappling hooks anchoring to nearly any surface. Being made of mana, you can banish them on demand as to not make a mess.
        """, """
        With rank 4 they gain a fourth adaptive slot, and can now add spider body parts to themself rather than replace body parts. They can acquire spider limbs from their back or hips for example, and they can now grow spider eyes where they wish across their body which can increase their field of view or ability to notice threats at back angles without messing with their field of view. They could imitate an arachne form at this stage but they aren’t quite there yet in truth. This is limited to adding the same parts available to one spider form if it were scaled up to human height, so they couldn’t just keep adding mass and extra legs like an abomination. They can now fire web projectiles offensively like bullets, arrows, or harpoons. They can apply any venom effects they have to their webbing either on injury or on contact more slowly through the skin. Rather than wood, spider parts are more like thin steel armor, and the web is as durable as a cable rather than rope. Their spider eyes are hypnotic, having a lulling effect that captures the attention like the enthrall perk, while eye contact is maintained, and you can issue suggestions like the suggestion perk when this is in effect. If you have either perk, the effects are twice as strong as the perk describes.
        """, """
        At rank 5, they have a fifth adaptive slot and they can combine equivalent mass and parts to 3 prior spiders, being larger if desired or just adding extra eyes. Every eye is now a partial brain forming a networked mind. So long as they have gem eyes, they have a functioning brain regardless of damage to the head. They can now shapeshift or make use of parts and traits from magical spider beings, provided the effects are roughly balanced to compare to a rank 5 magic effect. If the arachne has curses, they can deliver curses wordlessly through eye contact or an eye can be sacrificed (occupying that space for an eye until healed) to deliver another spell effect directly onto the target without travel time, such as using a Firecalling evil eye to cause them to self immolate. The Arachne can control their spiderwebs as though they were prehensile and telekinetically animated. If a being is wrapped with at least 10 loops of web, the arachne can induce suspended animation lasting until the web is removed.
        """ ]
    }


title : String
title =
    "# The Magic"


intro : String
intro =
    """
    "Time for the fun part. We’ve isolated your true form and primed it for emergence, but that’s only a small part of what you can actually do. The magic varies from witch to witch, we’ll run through the possibilities so I can get a sense of what’s resonating with you. It’s a pretty reliable method of detecting what you’ll be capable of in the future as you explore your abilities and grow your talents."

    Note that these are the possibilities isolated for you, not all witches would have the same opportunities you do. You are exceptional and have more options, and to a higher ceiling than most. Rank 3 in one or two specializations would be considered a capable witch. There are whispers of witches with rank 6 magic, while 7+ are the domain of gods, who are very real.

    {choice Each rank in a magic specialization below costs power equal to its rank, in sequential order}. le; Rank 5 magic costs 15 points in total, rank 3 would be a total of 6p. {choice All Specializations have associated Affinities tagged}. If you have one of these affinities, the magic costs half the power, {choice *rounding up*}. [All] are universally discounted to all affinities.

    For every Rank 5 magic, you must have at least one other magic at Rank 4. For every rank 4, you need 1 magic of rank 3 or less. This does not apply to the either Slot game mode changes, which behave in isolation. Slots stand on their own. This applies to the player, but non-player characters need not adhere to player mechanics and can be presumed to have various less notable magical traits not listed.

    {choice [star] next to the name show that a magic specialization has a universal “Rank 0” effect, which IS available to nearly every witch}, though this does not imply any innate skill with the magic specialization that was built on top of some aspect of that magic.

    If you have at least 1 rank in a Magic Specialization, you can spend _Focus_, _Might_, or _Favor_ to temporarily “power up” to use the higher rank of magic, equal to what the Power cost would be to unlock that rank (e.g.: 5 power = 5 Focus). You can use it for 10 minutes, or extend it for an additional 10 minutes by pushing past your limits resulting in unconsciousness when time runs out.

    {center} {choice Don’t like math? Have a reference table.}
    """


slotDescription : String
slotDescription =
    "If playing in a Slot Mode (Skill Tree or Constellation), Magic instead costs a Slot as shown. Folk slots can buy rank 2 magics. You can have white “Free” slots granting rank 1 magic as granted by your Class discount on options that would cost 0."


elementalismIntro : String
elementalismIntro =
    """
    The magic presented here in this section is referred to as Elementalism. These single-affinity or monotype magics operate just a little differently from other schools of magic, in that {choice any witch other than sorceresses are incapable of learning more than one elementalist magic _that they do not have the affinity for_}.

    Each magic will show an OR clause. {choice You can qualify for Affinity discounts if you possess both component affinities}, including taking it without the primary monotype affinity.

    If you have more than one elementalist magic, you can combine effects to create combination magic. Such as Firecalling and Earthmoving for lava effects, Windkeeping and Waterworking for storm effects. This is beyond the scope of what the cyoa can detail and up to reasonable interpretation with wiggle room for creative liberties. {choice You can also perform _Harmony_ magic, combining elements of cooperating witches, twice as potent as normal}.

    Witches with an elemental magic at rank 3 are often called _Hazards_. Rank 4 are _Disasters_, and Rank 5 can be labeled as _Calamities_. This designation can influence the weight of consequences placed on you for reckless behavior, people don’t trust living weapons with a track record of being careless when many lives can be at stake if you prove unstable, or outright malevolent. Witches with _Curses_ also receive this designation, as well as others on a case by case basis, such as some grenadier alchemists.

    {choice Elementalist magics cannot be taken for the _Restriction_} complication unless you have the affinity for it.
    """
