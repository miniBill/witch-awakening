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
    [ arcadia
    ]


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

        Take 1 Arcadian companion and 1 from any faction that can reasonably befriend you, for free. _*Digicasting*_ is half price for you, stacks with affinity."""
    , images = Images.factionArcadia
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
