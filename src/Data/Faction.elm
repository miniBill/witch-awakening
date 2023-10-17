module Data.Faction exposing (Details, all, intro, summaries)

import Generated.Types exposing (Faction(..))
import Images exposing (Image)


type alias Details =
    { name : Faction
    , motto : String
    , description : String
    , location : String
    , relations : String
    , perk : String
    , perkContent : String
    , images : { image1 : Image, image2 : Image, image3 : Image, image4 : Image, image5 : Image }
    }


all : List Details
all =
    [ arcadia, hawthorne, watchers ]


arcadia : Details
arcadia =
    { name = Arcadia
    , motto = "Enjoy a slice of life"
    , description =
        """Registering for college at Arcadia is free to any witch and comes with room and board in one of the dorms. Schedules are self determined with classes you can drop in and out of, and attendance works in time slots throughout the day, not a specific hour. If you miss 10AM, rain check for 2PM, or 10PM. There are classes for any specialization you have access to where you’ll be taught foundational principles and functions for your rank. There are classes for other specializations that exist but weren’t in the cards for your awakening (I can’t write every potentiality after all, third party extensions are encouraged!), which you can sit in for without credit if you want. Non-magic classes also exist; Such as planar theory, cryptozoology, esoteric chemistry, antediluvian mathematics, and more. There’s a very casual atmosphere letting witches move at their own pace. You DO have attendance, but it’s in totality at a the end of a cycle (3 month period). If you miss the day a class is taught, it can be made up within the cycle and no grades are set in stone until the cycle changes. Classes aside, you can expect an overabundance of entertainment and leisure options. Naturally, the arcade is a favorite destination within the town, and the entire town is routed through to the material world with Hex VPN, but by default they rely on their own local network first.

        Teachers are very fond of practical exams, applied learning, and field trips. APG atmosphere exists, but more adult activities are permitted if not publicly displayed, like the old red curtain back rooms of old movie stores, for example. Order is managed by a very capable police-military that double as basic problem solvers and community service. They’re the ones that get the cat out of the tree and organize service and charities. They can afford to be spread out since crime is so low and they default to warnings, not cuffs. That said, Arcadia at large is not actually a democracy but more comparable to private property of the eccentric witch that started it all within her own pocketspace from Witchery. After founding Arcadia and establishing its laws, she left it to coast from there without a recognizable government to make up new laws. She hasn’t been seen since, as far as we know."""
    , location =
        """Arcadia is particularly known for, well, Arcadia, the town itself. It’s a beautiful scenic town of a few thousand people, a third of them currently active students with the rest staff and alumni or otherwise residents operating various shops and services. Commerce is a little unusual to mortals in that it’s a service-barter economy operating on trading goods or favors. Think of it like a quest. If you want the Potion Maker’s potion of hair-b-gone, maybe you’ll need to do a fetch quest for them first. Usually such requests relate to acquiring the ingredients they need to make it plus a bit of compensation for their own time and effort in the production. Gold is used sporadically as a valued trade medium not necessarily as a currency, but as a raw trade good used in a number of rituals and crafts.

        The entire town is made with walking or broomsticks in mind, as such there aren’t many actual roads except as far as they’re just wider paths that expect more people, or to leave a more open space with good sightlines of the town and it surrounding woody mountainside to the north, rolling hills to the northwest, rocky cliffs to the northeast, sandy beaches to the east, grassy plains to the west, rivers and woods to the southwest, south, and southeast. The exact particulars change with every Cycle, the landscape beyond the town limits dissolving into dream mist and reshaped based on the student board’s vote, including impossible geology. The landscape is designed by a collaboration between planar theory classes with physical education clubs to design good features optimal for extracurricular activities. Hiking, swimming, spelunking, rock climbing, mountain biking, even road tips. The town does have some main roads, and an outer ring that can connect to roads that get generated in the cycle to lead up to various points of civilization. It’s seen as rustic and old school to use an automobile in place of a broom, for the experience of it. Enchanted, of course, operating on any number of forms of magical intervention to not require gas or produce emissions.

        Cycle generation works a lot like programming a random generator for an open world, relying on base logic and assets it pulls from to then scale out infinitely, as well as incorporating custom locations designed with more precise care and consideration, either placed in fixed locations from the town or as another part of generation to sprinkle throughout, and can then string roads to certain types of locations including temporary towns or whole cities generated for that cycle. The zombie theme a few cycles back was a favorite."""
    , relations =
        """The College is in an uneasy rivalry with Hawthorne, but the dynamic is akin to an older and younger sibling. They might posture and roughhouse, but ultimately Hawthorne just wants the best for them, and for them to keep safe, and won’t pull punches in defending them if need be. Arcadia has a cathedral operated directly by the Watchers. Hespatia is as secretive as ever, but two families have been noted as operating within Arcadia"""
    , perk = "True Treasure"
    , perkContent =
        """Joining Arcadia grants a lot of access to a strong community of witches with a number of shared core values that connect them together in similar challenges as witches learning their magic and enjoying the refuge and adventure all close at hand here in Arcadia. Join in on any number of clubs and hobbyist groups.

        Take 1 Arcadian companion and 1 from any faction that can reasonably befriend you, for free."""
    , images = Images.factionArcadia
    }


hawthorne : Details
hawthorne =
    { name = Hawthorne
    , motto = "School of Witchcraft and... an Unexpected Amount of Latex"
    , description =
        """Hawthorne admissions are open to any witch, and is a polar opposite to the College. Witches are tested for aptitude and potential where they will be sorted into Houses of comparable individuals sorted both by personality, and the magic specializations they have access to. Classes are mandatory. Once you complete all classes you have access to, which includes non-magic specialization courses such as physical health, mental health, and spiritual health, alongside social health. Combined the Health courses ensure residents can coexist in Hawthome and manage discourse. These courses include describing your opinion of your peers, and why, and if you have any suspicions about them that are a danger to the health of other students, themselves, or the Academy. Rules are plentiful and judiciously enforced alongside their own Common Law, being their own nation state. Capital punishment exists, though only for severe violent actions or gross malevolence, and exile for irreconcilable differences is otherwise the Final extreme, otherwise Hawthomne employs a wide array of disciplinarian tactics above and beyond what mortals would consider ethical. Generally discipline is specific to different houses, as houses in part represent personality characteristics, different discipline methods are more or less effective in the different houses. If one house is failing to discipline appropriate, re-evaluations occur and you can be reassigned to a house with different discipline methods. Houses likewise have their own uniforms though they all are approved by the Oversight Board to share certain qualities and colors, generally a degree of formality. The goal? Hawthorne may seem totalitarian, but their purpose is genuineThe excellence of their students, by any means necessary. They drive you to find your limits, and push beyond them to grow above and beyond what you thought you could ever achieve yourself. Your days are scheduled and optimized for you by councilors that have everything about you and your record on file. You _*will*_ succeed at Hawthorne, no matter how many times you stumble along the way towards success. Every failure is a learning experience adding to your growth, with certain discipline measures to remember it."""
    , location =
        """Hawthorne is an incredibly impressive sight to behold, occupying a large cavern beneath the Greenland ice sheet, it has been spatially warped and bent to the will over the Overseers over time and has grown into a powerful entity in its own right, expanding out from the Academy to a sprawling urban city of towering gothic spires that rise up, as well as descend down from the cavern’s ceiling, like stalagites and stalagmites of stone and glass. The cavern is big, but it’s disproportionately warped as to currently be around the size of Germany, completely filled with a sprawling metropolis. That said, it has a low population density concentrated around the Academy as new buildings grow on their own automatically by the heavy enchantments that grow the extradimensional pocket, and that scale doesn’t account for being doubled by the ceiling being occupied space itself, with a central dividing plane where there is no gravity wherein you can orient yourself to the opposing side.

        This layer is also where clouds get generated, and the entire cavern is illuminated by a persistent illusion effect that produces a horizon relative to the observer, with a great looming moon that illuminates all. As it dips below one horizon in a shallow arc to the west, it can already be seen rising in the east, alternating between reddish light and blueish light, in a state of perpetual well illuminated night he city is inhabited by veteran students, alumni, and staff, who agree to terms of citizenship where they can be called upon to help in affairs ranging from defense or offensive factions, hazard management, the odd bit of labor that can’t be managed by automation or undead, or a call in to help teach a class on a relevant subject to your expertise.

        While dominated by urban blocks and streets, there are still also parks and courtyards that act as nature preserves and miniature biomes, alongside dedicated greenhouses. Some being themselves enchanted for expanded space within the already expanded cavern, as well as sprawl that has grown through portals to connect to other caverns throughout the world, as well as on the moon, connecting Hawthorne to Lunabella, further complicating measurements for census."""
    , relations =
        """Hawthorne sees The College as a troubled younger sibling, a bit naive and airheaded, and vulnerable to threats they don’t understand. They have strong affiliation with Lunabella and both The Watchers, and Hespatia. Hawthorne strives to provide the best of the best, and accommodates different personalities that can fit with either."""
    , perk = "Master Wand"
    , perkContent =
        """Gain a House Uniform as a Garment and Hat, and you will be issued a specially crafted wand fine tuned to the results of your aptitude results. Yours is more powerful than most. Any numerical value from Magic Specializations are boosted by 50% when cast through your wand, and double your Wands spell slots. You are required to wear your uniform within. Hawthorne, baring Staff direction or privacy."""
    , images = Images.factionHawthorne
    }


watchers : Details
watchers =
    { name = Watchers
    , motto = "Consorting with Angels"
    , description =
        """Watchers have branches based on every major Abrahamic religion from Judaism to Mormonism, including Islamic faiths. The Watchers get their name from their stance as a third party observing the events playing out at the hands of human faith -- And strange celestial beings as real as Witches already know Demons to be. Through these observations they’ve studied the influences and nature of the divine, and are generally convinced of an overarching theory connecting the Abrahamic traditions, and have come to the conclusion that there is a coming apocalypse. Watchers are embedded in religious institutions where they keep their finger on the pulse and catalogue and investigate reports of divine influence and visionary experiences, finding that all religions experience them to different degrees. While some religions revolve around ancient witches and extraplanar entities, Abrahamic faiths interact with a category of beings referred to as Celestials. Watchers learned methods to emulate these beings through magic. They classify them as Pre-Mortal Celestials and Post-Mortal Celestials. Premortals are like living chariots of rings, eyes, and wings, and are spiritual constructs while Post-Mortals fare humanoid shaped entities with physical forms that can interact with the material world if they chose but bare elements of their prior construct form, rings becoming halos in various designs for example, and may have more extreme body shapes and proportions.

        As a watcher, your purpose is to keep tabs on religions and report on interesting events within your jurisdiction. Other Watcher agents may be approached to dedicate themselves to other roles, such as becoming a politician for an area of interest, or more often and historically available; Wife to a powerful figure, not just for the sake of gender roles, they have women in key places as well, but the logistics and avoiding spotlights, as well as playing nicer with the treaties against direct involvement. Another reason they typically advocate for democracies; They can hold positions of power while claiming the will of humans as opposed to their own intentions. So long as they convince the population to vote the way they’d prefer. Which they’ve gotten very skilled at doing, with no direct use of magic involved. Where Hespatia is cloak and dagger, Watchers hide in plain sight and are more likely to be involved with something like corporate espionage, though most Watchers do just that. Observe. Agents themselves may not know what their purpose is, just what they need to do without context of how it fits into a greater picture. The higher up the food chain you go the more of the puzzle you’ll see, but even at the top they skate along on complex interfaith theories and mysticism, not fully comprehending the forces they’re trying to dance along the periphery of, balancing ancient treaties, mortal interests, and not stepping on celestial toes. Because of their influence in the mortal world’s institutions, mainly in Europe at the time, that the “Witch Trials” were greatly reduced compared to the colonies, acting through Catholicism to denounce it in colonial puritanism."""
    , location =
        """Watchers generally coexist within religious institutions, but in 300AD a witch established contact with a celestial entity and negotiated a deal that lead to the creation of a private celestial plane. Nobody knows what this deal was, but being taken to this plane she was able to create portals leading others to it, and establish gateways. They’ve named it Eden, it’s a floating isle within a smoke ring around a binary star system with a brilliant golden star that illuminates the vistas of clouds in a golden glow in place of night, and a blue-white star providing a more even daylight experience.

        The isles are overwhelmingly fertile and resource rich where animal life all evolved wings, while being fluffy furry variations on felines, canines, or avians. Instead of a fly, you’d see a tiny six-legged kitten you could hold handfuls of at once. Some islands are claimed where witches have retired to, otherwise the Watchers operate their HQ here on one large island where they have a large palatial castle of white marble and gold. New certified witches are brought to Eden where they go through initiation. Some initiated witches manifest False Light, gaining a halo and wings. Within Eden, there are great gardens, bath houses, monuments, office spaces, residences for temple workers and the retired, a grand hall, and the general rooms you might expect of an estate and castle"""
    , relations =
        """Many watchers graduate from Hawthorne (Primary faction would be Watchers and you’d be in contact through your education, if you attend). They tend to view Arcadia as being on a slippery slope to degeneracy. They have a strong dislike for Hespatia that sometimes results in skirmishes but maintain shared long term goals."""
    , perk = "False light"
    , perkContent =
        """The gift of the Near-Light emulates the glory of the celestials. It grants feathered wings, flying at 160mph + the max speed of your broom, and above your head glows a halo of light. You can’t be blinded as a result of light or sunburned, and looking at your halo is like looking at the sun, illuminating the area with bright light, including UV Radiation. This can be dimmed to a simple glow and either can be toggled on/off.
        """
    , images = Images.factionWatchers
    }


intro : String
intro =
    """
    "I mentioned I can hook you up to other witches if you want. It helps to be less of a fish out of water, and help you practice your magics. Especially for academics or hard learners, though everyone still needs practice and hands on experience before you get the hang of it. Different factions employ different methods of staying on the down low or avoiding humans entirely. As witches, we aren’t limited to this world. Did you get a little overwhelmed by the idea of having to avoid earth governments? Don’t worry about it. The factions have you covered and have hammered out ways to avoid dealing with mortals. Most witches have abandoned Earth after they hecking _*nuked our ancient library*_. Calm. I’m calm. But yeah, most witches just don’t see a whole lot of reason to stick to earth these days. There are a lot of extraplanar interests in mortals, and primordial hierarchies, so even if we wanted to go to all out war against humans it’d just draw out larger entities and... Suffice it to say, we’ve run the numbers and it’s just better for everyone if we go our own ways. We get to explore different dimensions and planets, humans are stuck on this rock. Let them have it. So play by their rules when on their turf and you shouldn’t have any issue. If you want to go crazy with magic experiments, then knock your socks off anywhere else, it’s not hard" {choice You can pick a primary faction.} This does not prevent you from associating with the others, unless hostile.

    "Quick run down on the factions I think are more relevant" Beyond aesthetics, {choice your choice of primary faction grants a shown perk or relic}. You can forgo this perk to instead gain 2 Power, or you can skip choosing a Faction to be Independent for 4 Power. Your Faction’s magic is half cost to you, further reduced by half if you have a matching Affinity.

    {choice You can still take the Specialization of a faction you don’t belong to, but its power cost is doubled.}
    """


summaries : String
summaries =
    """
    {choice _Summaries:_}

    - {choice __*The College at Arcadia*__}. American upstarts running their own pocketrealm. Rural american town vibes melded with a mild city experience, lots of personal liberties but they maintain a degree of country justice to keep the order. Heavily integrates modem lifestyle and conveniences you might expect while having quick access to expansive wildlife and wilderness to explore with paradisiacal climate. {choice _Modernity. Homey town surrounding the college. Endless expanse surrounding it full of shifting changing features._}

    - {choice __*Hawthome Academia*__}. A very old prestigious institution that has continued to grow over time. Labyrinthian gothic architecture that heavily employs space folding and relativity, no up or down. It’s located somewhere under the Greenland ice sheet but parts of it sprawl out from connected portals that span the globe and Lunabella. They’re strict and disciplinarian with uniforms and not limited to mortal ethics. {choice _Extremely safe, discounting any punishments. Effective education, magic and mundane. Extraordinary architecture._}

    - {choice __*The Watchers*__}. Also known as the Followers of the Apocalypse, but it’s a mouthful. A bit more of a traditional coven, the Watchers date back to a few hundred years BC, and officially founded around 30AD. They integrate Abrahamic cosmology and teachings gleaned from all following churches into one overarching narrative from a third party perspective. Watchers form an interfaith network throughout, and take closer interest in mortal affairs and politics, with influence in most governments to bury things in red tape when needed. {choice _Wide connections within the mortal world. Access to wealth and influence. Help in most churches if you say the right things._}

    - {choice __*The Hespatian Coven*__}. Also operates under many alternative names with the way it subdivides into families that have overarching connections that cooperate in shared interests while retaining independent oversight for the most part. Members of a ‘family’ may not know about the broader scope while just leadership roles are in the know and in communication with other families. They are the classic secret society, to outright secret cults, and they incorporate mortals into their structure as a way to gain leverages, blackmail, and other resources. {choice _Assassins and thieves of the highest caliber. Many conspiracies may relate to Hespatian families. Hell access._}

    - {choice __*Lunabella*__}. It may surprise you to know that this city on the moon was started in the 6th century by a lich with broomstick and a dream. It’s a glistening city that since grew to accommodate life and terraform into a garden region contained by crystal spires that maintain atmosphere- And hide casual observation from mortals. They’ve produced outposts on other planetary bodies. {choice _Killer view. Olympian luxury. Arcane transhumanism. Particular advances in transmutation magic._}
    """
