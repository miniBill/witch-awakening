module Data.Faction exposing (Details, humansIntro, intro, summaries, toShortString)

import Generated.Types exposing (Faction(..))
import Images exposing (Image)


type alias Details =
    { name : Faction
    , motto : String
    , isHuman : Bool
    , description : String
    , location : String
    , relations : String
    , perk : String
    , perkContent : String
    , dlc : Maybe String
    , images : { image1 : Image, image2 : Image, image3 : Image, image4 : Image, image5 : Image }
    }


toShortString : Faction -> String
toShortString raw =
    case raw of
        FactionTheCollegeOfArcadia ->
            "Arcadia"

        FactionHawthorneAcademia ->
            "Hawthorne"

        FactionTheWatchers ->
            "Watchers"

        FactionTheHespatianCoven ->
            "Hespatia"

        FactionLunabella ->
            "Lunabella"

        FactionAlfheimrAlliance ->
            "Alliance"

        FactionTheOutsiders ->
            "Outsiders"

        FactionTheORC ->
            "ORC"

        FactionAlphazonIndustries ->
            "Alphazon"

        FactionIndependents ->
            "Independent"


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

    - {choice __*The College at Arcadia*__}. American upstarts running their own pocket realm. Rural american town vibes melded with a mild city experience, lots of personal liberties but they maintain a degree of country justice to keep the order. Heavily integrates modem lifestyle and conveniences you might expect while having quick access to expansive wildlife and wilderness to explore with paradisiacal climate. {choice _Modernity. Homey town surrounding the college. Endless expanse surrounding it full of shifting changing features._}

    - {choice __*Hawthorne Academia*__}. A very old prestigious institution that has continued to grow over time. Labyrinthian gothic architecture that heavily employs space folding and relativity, no up or down. It’s located somewhere under the Greenland ice sheet but parts of it sprawl out from connected portals that span the globe and Lunabella. They’re strict and disciplinarian with uniforms and not limited to mortal ethics. {choice _Extremely safe, discounting any punishments. Effective education, magic and mundane. Extraordinary architecture._}

    - {choice __*The Watchers*__}. Also known as the Followers of the Apocalypse, but it’s a mouthful. A bit more of a traditional coven, the Watchers date back to a few hundred years BC, and officially founded around 30AD. They integrate Abrahamic cosmology and teachings gleaned from all following churches into one overarching narrative from a third party perspective. Watchers form an interfaith network throughout, and take closer interest in mortal affairs and politics, with influence in most governments to bury things in red tape when needed. {choice _Wide connections within the mortal world. Access to wealth and influence. Help in most churches if you say the right things._}

    - {choice __*The Hespatian Coven*__}. Also operates under many alternative names with the way it subdivides into families that have overarching connections that cooperate in shared interests while retaining independent oversight for the most part. Members of a “family” may not know about the broader scope while just leadership roles are in the know and in communication with other families. They are the classic secret society, to outright secret cults, and they incorporate mortals into their structure as a way to gain leverages, blackmail, and other resources. {choice _Assassins and thieves of the highest caliber. Many conspiracies may relate to Hespatian families. Hell access._}

    - {choice __*Lunabella*__}. It may surprise you to know that this city on the moon was started in the 6th century by a lich with broomstick and a dream. It’s a glistening city that since grew to accommodate life and terraform into a garden region contained by crystal spires that maintain atmosphere- And hide casual observation from mortals. They’ve produced outposts on other planetary bodies. {choice _Killer view. Olympian luxury. Arcane transhumanism. Particular advances in transmutation magic._}
    """


humansIntro : String
humansIntro =
    """
    {center, mono} "Hey there, champ, miss, whatever. This is a pre-recorded message. If you’re seeing this, congratulations, you’ve just awakened as a Witch and we estimate now is about when the guidance ritual triggering this response has just finished going over some “Factions” of witches."

    {center} Allow me to introduce ourselves; Humans. Or “Mortals” as some witches say. As you may have noticed, we, and I, have some tricks of our own. Contrary to what most Initiator or “Guidance” witches say, such as the witch that guided your awakening just now or you wouldn’t be seeing this message, I’m sure you’ve heard some spiel about surveillance and “They nuked our library” or some such, fact is humans and witches have gotten along fine for over a century now.

    {center} Sure the nuke thing was more recent, but it’s what firmly shook some sense into the broader witch community. That library held some of the most insidious tomes of magic ever written that could- and did -make demons weep. Facts are, we’re not some weak incapable sheep to be herded anymore. Not all of us. We’re aware of the Treaties of the Masquerade, and we at the ORC... and those suits at Alphazon, act within the bounds of the masquerade to represent human interests. There are some others, but let’s keep it simple for now, and we aren’t just humans, we just don’t isolate ourselves and pretend that we’re not. I’m a witch the same as you, but we’re still human too, our gifts don’t have to change who we are.

    {center} Help us, and we help you. We don’t relish having to make enemies, but some witches let power get to their head, and they start viewing ungifted humans as pawns or resources to be used or spent.

    {center} There are two major “factions” of us, so we’ll present it like those you’ve seen so far.

    {center, mono} "Rolling film, let’s take a look."
    """
