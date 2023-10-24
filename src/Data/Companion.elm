module Data.Companion exposing (Details, Power(..), all, intro)

import Generated.Types exposing (Class(..), Companion(..), Faction(..), Race(..))


type alias Details =
    { name : Companion
    , class : Maybe Class
    , race : Maybe Race
    , hasPerk : Bool
    , cost : Int
    , power : Power
    , teamwork : Int
    , sociability : Int
    , morality : Int
    , quote : String
    , description : String
    , positives : List String
    , negatives : List String
    , mixed : List String
    , has : String
    }


type Power
    = Witch Int
    | SpecialEffect
    | NotAWitch


intro : String
intro =
    """
    It’s dangerous to go alone, take this! A Companions section. {choice You have one free companion from your chosen faction, and one free that shares the same witch type} as you (either Race or Class). Companions have 4 main statistics giving a _rough idea_ of their general impact in a group dynamic, except the {choice POWER stat, which primarily serves to give free floating Power points to spend to _customize_ your companion's abilities} on top of the default abilities they come with. Beyond that, interactions are inferred in how you interpret their personality and how different stats play out in relation to it and others. {choice You can buy additional companions using Reward Points}, listed to the left of their name. A {choice +} indicates they have their Type Perk.

    You can spend your own Power on behalf of a member if you so choose.

    **Really, don't stress the details too much and feel free to fill in the blanks. I do what I can to provide a reasonable summary of an array of characters, but it's up to you to interpret it as you will.**

    Take your pick:
    """


all : List ( String, Maybe Faction, List Details )
all =
    [ ( "The Arcadians", Just Arcadia, arcadians )
    , ( "Hawthorne", Just Hawthorne, hawthorne )
    , ( "The Watchers", Just Watchers, watchers )
    , ( "The Hespatians", Just Hespatian, hespatians )
    , ( "The Lunabellans", Just Lunabella, lunabellans )
    , ( "The ORCs / Badges", Just TheOrc, theOrcs )
    , ( "The Alphazonians / Suits", Just Alphazon, alphazonians )
    , ( "Independents / Other", Nothing, independents )
    ]


arcadians : List Details
arcadians =
    [ rachelPool, anneLaurenchi, candayWesbank, tessaMarieKudashov, evelynnPWillowcrane, johnDoe ]


rachelPool : Details
rachelPool =
    { name = RachelPool
    , class = Just Academic
    , race = Just Neutral
    , hasPerk = False
    , cost = 4
    , power = Witch 5
    , teamwork = 8
    , sociability = 4
    , morality = 9
    , quote = "_*“Has anyone seen Rachel around? We have a double date in 20 minutes and she totally vanished.” - an Arcadian student.*_"
    , description =
        """Rachel is very introverted and socially awkward, she grew up largely alone without a lot of interaction, so other people may as well be aliens for her and she freaks out about saying the wrong thing, but once she makes real friends she cares for them a lot and can find her stride. She's very fond of animals and spends a lot of time with her cat, and going on walks alone as to not bother anybody."""
    , positives =
        [ "+ Unexpectedly athletic/nimble."
        , "+ Goes out of her way for friends."
        , "+ Talks to animals."
        ]
    , negatives =
        [ "- Slow to adapt."
        , "- Very nervous."
        , "- ... more than people."
        ]
    , mixed =
        [ "+ Will at least _try_ things beyond her comfort zone."
        ]
    , has = "Rachel has Potions 2, Witchery 1, Hexes 2, Familiarity 3, and Naturalism 3"
    }


anneLaurenchi : Details
anneLaurenchi =
    { name = AnneLaurenchi
    , class = Just Academic
    , race = Just Sylph
    , hasPerk = True
    , cost = 4
    , power = Witch 4
    , teamwork = 4
    , sociability = 6
    , morality = 7
    , quote = "_*“Is..Is that Anne? How on earth can she sleep like that. Is that safe? feel like I should help her out” - An Arcadian student.*_"
    , description = "Anne is a bit of an oddball. Rather than simply being introverted, she's pretty much outright antisocial seeing no need or reason to bother and actively prefers keeping a smaller friendgroup, but she tries to do right by those that do make that cut. She's known for sleeping most her days away and not just in her bed, but anywhere she goes, some think she has narcolepsy; but she just enjoys dreams a ton."
    , positives =
        [ "+ Endless imagination."
        , "+ Powerful when she wants to be..."
        , "+ Highly skilled digital artist."
        ]
    , negatives =
        [ "- Ungrounded in reality."
        , "- ... Would rather nap."
        , "- Slow even when awake."
        ]
    , mixed =
        [ "+ Likes to show off dreamworlds with friends."
        ]
    , has = "Anne has Familiar 1, Aether 2, Portal 3, Psycho 3, Digicasting 4, as well as _Beauty Sleep_"
    }


candayWesbank : Details
candayWesbank =
    { name = CandayWesbank
    , class = Just Sorceress
    , race = Just Neutral
    , hasPerk = False
    , cost = 2
    , power = Witch 4
    , teamwork = 8
    , sociability = 7
    , morality = 8
    , quote = "_*“Wess has been out there all day long helping clean up the river after that necroshark attack.” - An Arcadian student.*_"
    , description = "Goes by “Wess”, to avoid people calling her “Candy” which rubs her the wrong way, but she doesn't make a big deal about it and goes with the flow. She has a calm reserved intelligence about the way she carries herself in a mature manner that suggests a lot of life experience that has seen some of the worst life has to offer, but also some of the best, and she appreciates what she can. She's a bit of a watchdog, always locking out for others, friend or rival."
    , positives =
        [ "+ Very competent and focused."
        , "+ Very physically fit and lean."
        , "+ MMA background."
        ]
    , negatives =
        [ "- She keeps it in check, but she's very paranoid."
        , "- Struggles to sleep."
        ]
    , mixed = [ "+/- Celebate until marriage. {smol (Though not a virgin).}" ]
    , has = "Wess has Runes 5 and an Improved Familiar"
    }


tessaMarieKudashov : Details
tessaMarieKudashov =
    { name = TessaMarieKudashov
    , class = Just Warlock
    , race = Just Neutral
    , hasPerk = False
    , cost = 2
    , power = Witch 8
    , teamwork = 5
    , sociability = 6
    , morality = 4
    , quote = "_\\*Unintelligible shrieking\\*_ _*“WHO TURNED MY BROOM INTO A F%#$^ TENTACLE MONSTER!” - An Arcadian student.*_"
    , description = "Tess is a bit of a nonviolent delinquent. She first awakened as a witch out of anger when she inadvertently hexed her human school bully into a slug, being a Warlock rather than an Academic, things are more instinctual and primal for her rather than well understood, and is somewhat easily annoyed, can trigger reactionary hexes with stray impulses, giving her a bad rep, but is cunning and personable when she wants to be and gets along when she needs to."
    , positives =
        [ "+ Casually brews potent potions."
        , "+ Very talented in Hex usage."
        , "+ Enjoys magic experiments."
        ]
    , negatives =
        [ "- Comically bad cook."
        , "- Academically lazy."
        , "- Morally ambiguous."
        ]
    , mixed = [ "+/- Bit of a freak. take that as you will." ]
    , has = "Tess has Potions 3, Hexes 4, has a _Hydron_"
    }


evelynnPWillowcrane : Details
evelynnPWillowcrane =
    { name = EvelynnPWillowcrane
    , class = Just Academic
    , race = Just Neutral
    , hasPerk = False
    , cost = 2
    , power = Witch 4
    , teamwork = 9
    , sociability = 10
    , morality = 7
    , quote = "_*“You're heading out with Evelynn again? I want to come too! She always finds the best places”. - An Arcadian student.*_"
    , description = "Evelynn is a high functioning socialite that always seems to know what someone needs to hear to get back on their feet or to keep a party going. She's an active go-getter always setting up new activities and getting people to attend without dragging their feet, and is as adventurous as she is social, finding people to tag along as she heads out to explore to find interesting places to host events."
    , positives =
        [ "+ Outgoing"
        , "+ Empathetic"
        , "+ Adaptable and cooperative"
        , "+/- Also known for being dtf"
        ]
    , negatives =
        [ "- Not the most powerful"
        , "- Can be caught up in the moment"
        , "- A _little_ pushy, tactfully"
        ]
    , mixed = []
    , has = "Evelynn has Waterworking 1, Witchery 1, Divination 2, Digicasting 2, Runes 3 & _Fascinate_"
    }


johnDoe : Details
johnDoe =
    { name = JohnDoe
    , class = Just Sorceress
    , race = Just Changeling
    , hasPerk = False
    , cost = 4
    , power = Witch 6
    , teamwork = 7
    , sociability = 5
    , morality = 10
    , quote = "_*“Little man is so f#&% cute holy shit”* \\*John turning red\\* *“Haha, he gets even cuter when embarrassed!” - An Arcadian student.*_"
    , description = "John is a very uncommon male witch... And rarer still: A Changeling. He can't remember much of his past so doesn't know how old he really is, but he tries to look as old as he can, He must have been a soldier or knight pre-awakening, because he has a firmly rooted sense of chivalry, justice, and mercy, and great reflexes and is a natural with weapons He's eager to learn all he can and studies his lessons closely."
    , positives =
        [ "+ Very intent listener."
        , "+ Unintentionally adorable."
        , "+ Latent martial master."
        ]
    , negatives =
        [ "- Quickly fatigues, socially."
        , "- Feels like he has to prove himself in everything."
        ]
    , mixed =
        [ "+/- Overly focused on gaining new experiences."
        ]
    , has = "John has Alchemy, Runes, and Witchery at 2, and unspent _Jack-of-All_, and _Memorize_"
    }


hawthorne : List Details
hawthorne =
    [ hannahGrangely, elizabellSinclaire, ashleyLovenko, sylvanneMaeKanzaki ]


hannahGrangely : Details
hannahGrangely =
    { name = HannahGrangely
    , class = Just Academic
    , race = Just Neutral
    , hasPerk = False
    , cost = 6
    , power = Witch 5
    , teamwork = 7
    , sociability = 6
    , morality = 7
    , quote = "_*“I'm about ready to throw myself in a snake pit over this assignment, I need to ask Hannah to tutor me tonight.” - A Hawthorne Student.*_"
    , description = "Hannah is top of her class, an excellent student that has the rules memorized and wholeheartedly embraces Hawthorne methods. She applied for the hardest courses and assigned House Lionfeather. She rarely makes the same mistake twice, and when she's not practicing magic, she's reading theory, or helping housemates with problems they're having trouble with."
    , positives =
        [ "+ Knowledgeable"
        , "+ Good at teaching"
        , "+ Always up to date on policy."
        ]
    , negatives =
        [ "- Unintentionally arrogant"
        , "- Not great at reading people,"
        , "- Doesn't get out much"
        ]
    , mixed =
        [ "+/- Helps avoid rulebreaking, but obligated to report" ]
    , has = "Hannah has 1 in every core Specialization, and has Waterworking 3, Wands 4 & _Master Wand_."
    }


elizabellSinclaire : Details
elizabellSinclaire =
    { name = ElizabellSinclaire
    , class = Just Warlock
    , race = Just Erinyes
    , hasPerk = False
    , cost = 10
    , power = Witch 2
    , teamwork = 7
    , sociability = 6
    , morality = 5
    , quote = "_*“Miss Grangely! Report to Eliza's office at once. Leave your things here. Yes, Everything.”*_ _\\*Crack of a ruler\\*_ _*- A Hawthorne Teacher.*_"
    , description = "Elizabell is a Hawthorne veteran at the top of her class and is her house's assistant disciplinarian, that is among some to employ more... novel, methods of discipline and reward structures, She's straight up a mistress dom and enjoys her job, fully dedicated to employing the methods of her house rules to get witches under her care to achieve their best She's firm but very warm and comforting when appropriate."
    , positives =
        [ "+ Incredible willpower."
        , "+ Highly competent."
        , "+ Skilled with weapons and tools."
        ]
    , negatives =
        [ "- No rulebreaking!"
        , "- Can be a bit of a nanny"
        , "- A strict perfectionist."
        ]
    , mixed =
        [ "+/- Isn't a domme by heart, but a talented switch." ]
    , has = "Eliza has Wands 3, Occultism 3, Witchery 3, Consortation 4, Windkeeping 5, and _Energize_."
    }


ashleyLovenko : Details
ashleyLovenko =
    { name = AshleyLovenko
    , class = Just Sorceress
    , race = Just Neutral
    , hasPerk = False
    , cost = 6
    , power = Witch 3
    , teamwork = 7
    , sociability = 9
    , morality = 8
    , quote = "_*“Where'd Ash go? Oh there she is, taunting the pond fish with fish-sticks again.”*_ _\\*Splash\\*_ _*“There she goes.” - A Hawthorne Student.*_"
    , description = "Ashley has been moved between two house already. She isn't a delinquent, but... she's an eccentric airhead with a goldfish-like memory at least when it comes to rules or lessons. She's very social and likable but often rubs people the wrong way at first with her enthusiasm and talent for messing things up. Her magic seems to have a mind of its own."
    , positives =
        [ "+ Naturally powerful"
        , "+ Boundless energy"
        , "+ Enthusiastic"
        ]
    , negatives =
        [ "- But with ditzy incompetence"
        , "- Very naive"
        , "- Often in discipline sessions"
        ]
    , mixed = [ "+/- Hard to dislike her the more you get to know her, despite often really bad first impressions" ]
    , has = "Ash has Familiarity 2, Witchery 2, Firecalling 3, Windkeeping 4, Wands 5 & _Magic Friendship_."
    }


sylvanneMaeKanzaki : Details
sylvanneMaeKanzaki =
    { name = SylvanneMaeKanzaki
    , class = Just Sorceress
    , race = Just Luxal
    , hasPerk = True
    , cost = 8
    , power = Witch 10
    , teamwork = 6
    , sociability = 7
    , morality = 8
    , quote = "_*“Mae is so cool.. When I broke my leg on a mission she held me while a gold light slowly fixed me.” - A Hawthorne Student.*_"
    , description = "If Willpower is a shield, Determination is the sword, the active driving force to push forward. Mae is determined to become the best and never fail another again since she lost a student to a band of goblinoids who couldn't be recovered for 4 days and was never the same. She's strict and can come across as cold, but would rather die than fail a student again. Uses similar methods to Eliza but much less, ah, _intrusive_."
    , positives =
        [ "+ Razor sharp determination."
        , "+ Head of House Dragonrose."
        , "+ Expert with a sword."
        ]
    , negatives =
        [ "- Watches you like a hawk."
        , "- Hates secrets. needs to know everything."
        ]
    , mixed = [ "+/- Little too quick to put her own life on the line" ]
    , has = "Mae has Wands 1, Familiarity 2 (Hawk), Ministration 3, Witchery 4, _Hat Trick_ and _Sun Shard_."
    }


watchers : List Details
watchers =
    [ francisIsaacGiovanni, ifraAlZahra, sariahJSnow, claireBelmontegra ]


francisIsaacGiovanni : Details
francisIsaacGiovanni =
    { name = FrancisIsaacGiovanni
    , class = Just Warlock
    , race = Just Neutral
    , hasPerk = False
    , cost = 6
    , power = Witch 6
    , teamwork = 4
    , sociability = 2
    , morality = 7
    , quote = "_*“He gives me chills. He's been a Watcher since the Crusades. Which he fought in. I heard he stormed Jerusalem in 1099” - A Watcher.*_"
    , description = "They aren't wrong, Isaac is a Crusader that participated in most at least seven different crusades. He's a Warlock with an angry soul that simmers in a fine boil for the rape and murder of his entire family while he was away on campaign, which is what caused his Witch Awakening wherin a Balor emerged out from his rage which was stopped by a Contemplar when it nearly killed him too. Catholic."
    , positives =
        [ "+ Intimidating presence"
        , "+ Renown warrior."
        ]
    , negatives =
        [ "- Avoids having to speak"
        , "- Has been a loner for centuries."
        ]
    , mixed = [ "+/- Learned to stop thinking in blanket generalities and now views evil as more of an infection or virus" ]
    , has = "Isaac has Consortation 4, Ministration 4, Runes 5, _Sun Shard_, _Mythril Armor_, and _Memorize_."
    }


ifraAlZahra : Details
ifraAlZahra =
    { name = IfraAlZahra
    , class = Just Warlock
    , race = Just Changeling
    , hasPerk = False
    , cost = 4
    , power = Witch 3
    , teamwork = 4
    , sociability = 9
    , morality = 7
    , quote = "_*“Hey Ifra, where'd these doujins come fro-”* \\*Poof\\* \\*Frog croaking\\* *“Oh hey Ifra, Where'd Kat go? Oh what's this-”* \\*Poof\\* *-Last Tue*_"
    , description = "Ifra grew up Muslim and managed to live a full life until the old age of 83 when she finally had her awakening -- into a changeling finding herself young again. A little more than she'd have hoped but hey. She believes in Islam though she's lived long enough to see a lot of changes, that she just adapts to whatever the local custom is, otherwise she's somewhat casual, but makes sure to pray 5 times a day."
    , positives =
        [ "+ Cunning with a sharp wit"
        , "+ Shapechanging."
        ]
    , negatives =
        [ "- Lazy and stubborn"
        , "- Surprisingly needy."
        ]
    , mixed = [ "+/- Reads a lot of, in her words, “degenerate filth!” like premarital handholding in both “Weeb stuff” & romcoms." ]
    , has = "Ifra has Ministration 1, Familiarity 2, Curses 3, Hexes 4, Earthmoving 4 and has _False Light_."
    }


sariahJSnow : Details
sariahJSnow =
    { name = SariahJSnow
    , class = Just Academic
    , race = Just Nymph
    , hasPerk = True
    , cost = 8
    , power = Witch 4
    , teamwork = 9
    , sociability = 7
    , morality = 9
    , quote = "_*“Sariah? She wanders a lot, but when she stops by I can't think of anyone more pleasant to be around.”- An ORC Agent.*_"
    , description = "Sariah has been a witch for a long time already, and used to have a human husband early on who passed away before she could do something about it. She takes comfort in her Mormon belief in eternal marriages, and lives a life of dedication to doing good and stay worthy to be with him again someday, chats once in a while via necromancy."
    , positives =
        [ "+ Very calm and optimistic"
        , "+ Very true to her faith"
        , "+/- Friendly with the ORC"
        ]
    , negatives =
        [ "- Doesn't go out on Sundays, unless it's important"
        , "- Even avoids caffeine entirely"
        ]
    , mixed = [ "+/- Wants a new husband, but always holds her, late husband as her true partner to be reunited eventual" ]
    , has = "Sariah has Necromancy 2, Witchery 3, Divination 4, Waterworking 5, Ministation 5, _Oracle_, _Mythril Armor_ & _Artifact Keeper_"
    }


claireBelmontegra : Details
claireBelmontegra =
    { name = ClaireBelMontegra
    , class = Just Sorceress
    , race = Just Nymph
    , hasPerk = True
    , cost = 6
    , power = Witch 5
    , teamwork = 8
    , sociability = 10
    , morality = 4
    , quote = "_*“I heard Claire competed against a whole team of Hespatian succubi for trade secrets in Russia.. and won.” - Watcher secretary gossip*_"
    , description = "Claire isn't very religious but appreciates the power structures and views Watcher lore as a meta layer of hidden histories. She's a full fledged active Watcher agent with eyes and ears everywhere, and greatly enjoys her work in corporate espionage and uses certain Daeva advantages to her fullest. She often quickly finds herself in the offices of some of the most powerful people in the world."
    , positives =
        [ "+ Heartachingly beautiful."
        , "+ Highly tech savvy."
        ]
    , negatives =
        [ "- Deception ≠ Stealth."
        , "- Workaholic"
        ]
    , mixed = [ "+/- Doesn't really consider anyone a friend until they've shared a bed while on mission together more than once" ]
    , has = "Claire has Curses 2, Ministration 3, Wind 4, Witchery 4, Hexes 5 & _False Light_"
    }


hespatians : List Details
hespatians =
    [ lucilleMBright, kingDaemianKain, whisper, redMother ]


lucilleMBright : Details
lucilleMBright =
    { name = LucilleMBright
    , class = Just Academic
    , race = Just Lilin
    , hasPerk = False
    , cost = 6
    , power = Witch 5
    , teamwork = 7
    , sociability = 8
    , morality = 3
    , quote = "_*“Be careful dealing with her, Luke... she caused the American Civil War with her games, playing both sides because she could” - A Watcher.*_"
    , description = "Lucille is an eternal rebel and unquenchable contrarian that strains against all forms of authority or boundary line. If you can take something, it's yours, they failed to keep it. She admires spark, in any form, and likes to kindle them into embers, into infernos. A strong tree has to start out from a sprout after all. She's an information and arms broker."
    , positives =
        [ "+ Wealthy"
        , "+ Well connected"
        , "+ Treats friends well"
        ]
    , negatives =
        [ "- From illicit sources."
        , "- By illicit means."
        , "- So long as they treat her well."
        ]
    , mixed = [ "+/- Believes there is no truth but power, the strong survive, If something _can_ be destroyed, it _should_ be destroyed." ]
    , has = "Lucille has Necromancy 3, Hexes 4, Consortation 4, Divination 4, Familiarity 5, _Blood Witch, Necronomicon & Hellrider_"
    }


kingDaemianKain : Details
kingDaemianKain =
    { name = KingDaemianKain
    , class = Just Sorceress
    , race = Just Dravir
    , hasPerk = True
    , cost = 12
    , power = Witch 10
    , teamwork = 5
    , sociability = 6
    , morality = 2
    , quote = "_*“Son, it's not every day a young man turns 18, time to start helping out with the family business. For now enjoy some catgirl slaves” - Kain*_"
    , description = "When most witches who know their history think of a male sorcerer, Kain likely crosses their mind as an antediluvian warlord that ruled over ancient Siberia as a witch-king with his striking crown-halo of flame and his drake mount familiar, his own skin hardened with scales in places, and his breath like a dragon's own. Now? a single dad with 3 kids and a blue collar job... while head of 4 Hespatian families"
    , positives =
        [ "+ Positive role model by day:"
        , "+ Worlds best dad"
        , "+ Makes time for the kids"
        ]
    , negatives =
        [ "- Ancient risen tyrant by night"
        , "- Works late a lot"
        , "- Really strict though"
        ]
    , mixed = [ "+/- Great guy to know if you need to get rid of a body" ]
    , has = "Kain has Familiarity 3, Firecalling 4, Curses 5, Consortation 5, Metamorphize 5 (Dragon) _Secret Magic & Family Line_"
    }


whisper : Details
whisper =
    { name = Whisper
    , class = Just Warlock
    , race = Just Neutral
    , hasPerk = False
    , cost = 10
    , power = Witch 9
    , teamwork = 7
    , sociability = 8
    , morality = 1
    , quote = "_\\*Shadows slither of their own volition as you lay atop a mountain of bodies slick with blood, a red glow illuminating a woman\\* *- A Vision.*_"
    , description = "Whisper is a high ranking Wraith of Hespatia and a prolific serial killer and ritual coordinator leading-a particular network of hespatian families as a pleasure-death cult indulging in the most egregious sins more than can be spoken here that make the murder, rape, and necrophilia seem tame. Every passing month marks between 6 and 60+ new victims between her rituals and her assassinations"
    , positives =
        [ "+ Insane situational awareness."
        , "+ Actually very charming."
        , "+ She's like, _really_ hot tho."
        ]
    , negatives =
        [ "- Highly manipulative"
        , "- Habit of vanishing,"
        , "- Might flay you if bored."
        ]
    , mixed = [ "+/- Wont trust you if you don't join in her “parties”." ]
    , has = "Whisper has Witchery 3, Familiarity 5, Occultism 5, Psychotics 5, _Visceramancy 5, Toximancy, Blood Witch, & Shroud_"
    }


redMother : Details
redMother =
    { name = RedMother
    , class = Just Academic
    , race = Just Neutral
    , hasPerk = False
    , cost = 8
    , power = Witch 6
    , teamwork = 7
    , sociability = 7
    , morality = 3
    , quote = "_*“M..Mother...”* \\*Suggestive breathing\\* *“May I?.”* \\*Deep rhythmic drums and chanting\\* *-Audio pulled from captive ORC Agent.*_"
    , description = "Was the student Mae lost. Goblin shamanism attempted to turn her into a broodmother and it left deep scars in her mind that have manifested themselves during recovery She runs a Hespatian family where she's “Mother” to a cult of pleasure and blood, though with less murder than Whisper's shtick and more emphasis on a creepy incestuous undertone and a religious charade involving sadism."
    , positives =
        [ "+ Cares for her family."
        , "+ Anyone can become “family”"
        , "+ Rewards good behavior."
        ]
    , negatives =
        [ "- And _only_ her family"
        , "- Vampiric weaknesses"
        , "- Severe punishments."
        ]
    , mixed = [ "+/- Won't _kill_ children or parents, but might abduct" ]
    , has = "Mom has Occultism 4, Necromancy 5, Alchemy 5, Consortation 5, _Visceramancy 5, Blood Witch, Family Line, and Immortal Blood_"
    }


lunabellans : List Details
lunabellans =
    [ diana, cassandra, kingCulicarius, einodiaKate ]


diana : Details
diana =
    { name = Diana
    , class = Just Sorceress
    , race = Just Neutral
    , hasPerk = False
    , cost = 6
    , power = Witch 8
    , teamwork = 9
    , sociability = 7
    , morality = 9
    , quote = "_*“Around here when you hear the wolves howl to the moon, you can feel safer, knowing the huntresses are here.” - ORC agent on a certain grove.*_"
    , description = "Diana isn't actually a changeling despite her youthful appearance, but settle into maintaining her looks a certain way a long time ago to be with her late changeling husband who she hasn't seen in over a thousand years. She doesn't know what happened to him, but has kept her vows ever since, living as an eternal maiden huntress with her wolf, and started a sorority of celebate huntresses to join her."
    , positives =
        [ "+ Perfect accuracy."
        , "+ Expert survivalist."
        , "+ High perception."
        ]
    , negatives =
        [ "- A little impatient with men."
        , "- Effectively asexual and aromantic."
        , "- A hint of persistent sadness."
        ]
    , mixed = [ "+/- Rarely goes without at least 2 huntresses with her" ]
    , has = "Diana has Ministation 2, Familiarity 4 (Owl), Witchery 5, _Improved Familiar (Dire Wolf), & Memorize_"
    }


cassandra : Details
cassandra =
    { name = Cassandra
    , class = Just Sorceress
    , race = Just Dravir
    , hasPerk = False
    , cost = 6
    , power = Witch 3
    , teamwork = 6
    , sociability = 5
    , morality = 7
    , quote = "_*“I love visiting Cass but I don't get why she's so reclusive. She's obviously not antisocial, I just don't get it...” - Lunabellan.*_"
    , description = "Used to be a renown oracle and her spat with Culicarius stuck in the rumormill long enough to become fable, back in his asshole phase in Greece, though she wasn't a saint either. They both got better. She manages her own domain bubble where she emphasizes large tall mountains and cliffs topped with estates where she practices ritual magics."
    , positives =
        [ "+ Generous and hospitable"
        , "+ Dotes on friends and servants"
        , "+ Loves serving others when able."
        ]
    , negatives =
        [ "- Bit of a shut in"
        , "- Actually narcoleptic"
        , "- Hard to self-motivate."
        ]
    , mixed = [ "+/- Dreamer with her head in the clouds, she thinks about the future at the expense of the present a lot." ]
    , has = "Cass has Familiarity 2, Potions 3, Occultism 3, Hexes 4, Divination 5, Domain 5, with _Oracle_"
    }


kingCulicarius : Details
kingCulicarius =
    { name = KingCulicarius
    , class = Just Academic
    , race = Just Daeva
    , hasPerk = True
    , cost = 15
    , power = Witch 4
    , teamwork = 9
    , sociability = 8
    , morality = 8
    , quote = "_*“I want to see Culicarius.. I want to know if I have a future with Mariah but I wonder what else to ask him.” - Random Lunabellans*_"
    , description = "The King of Lunabella and an ancient male Daeva that inspired the myth of Apollo, He can be in a number of places at once and has seen so much, not just in his life but in exploring futures. He's fair but firm. Harem of 1,013, wives & some husbands because he rarely turns them down."
    , positives =
        [ "+ Literally the body of a greek god."
        , "+ There's a lot of him."
        ]
    , negatives =
        [ "- If you abuse your relationship with him, you'll lose it"
        , "- Very jaded, though whimsical"
        ]
    , mixed = [ "+/- You can't convince him to change Lunabella, for better or worse, without serious evidence that it isn't working: and, it is, and he’s seen a thousand alternatives to their ends." ]
    , has = "He has Ministration 3, Potions 4, Curses 5, Consortation 5, Firecalling 5, Divination 5, Oracle, Synthetic Hive, Embody Time"
    }


einodiaKate : Details
einodiaKate =
    { name = EinodiaKate
    , class = Just Academic
    , race = Just Daeva
    , hasPerk = False
    , cost = 15
    , power = Witch 1
    , teamwork = 4
    , sociability = 5
    , morality = 6
    , quote = "_*“She's too talented for her own good, straining against the limits of the masquerade...and higher powers.” - Culicarius to a friend.*_"
    , description = "Also goes by Kate, Kate is the lich founder of Lunabella, but she has no interest in management, she just opened the way and chose her brother Culicarius to rule in her stead. She's a legendary witch that got entangled in the myth of _Hekate_, to the extent she began the process of mantling, becoming one, but was pulled out of it by her brother. She's a traveler at heart that's always yearning for more knowledge."
    , positives =
        [ "+ Very productive."
        , "+ Top tier power."
        , "+ High _skill_ in magic use."
        ]
    , negatives =
        [ "- Grows restless with stagnation"
        , "- May or may not have had an affair with her own brother once."
        ]
    , mixed = [ "+/- Driven by curiosity, secrets eat at her mind." ]
    , has = "Kate has Witchery 4, Potions 5, Runes 5, Windkeeping 5, Necromancy 5, Portals 5, Aethernautics 5, and Domain 6"
    }


theOrcs : List Details
theOrcs =
    [ victoriaWatts, richardMaxJohnson, bethadonnaRossbaum, mirandaQuincy ]


victoriaWatts : Details
victoriaWatts =
    { name = VictoriaWatts
    , class = Nothing
    , race = Just Neutral
    , hasPerk = False
    , cost = 8
    , power = NotAWitch
    , teamwork = 10
    , sociability = 8
    , morality = 7
    , quote = "_*“TYPE RED TYPE RED, OH SHIT, oh-”* *static* *“Fall back to site\u{00A0}B6. V on route to intercept.” - ORC Radio chatter.*_"
    , description = "V is not a witch, but is nonetheless a top ranking ORC Agent and a master of her job. She's 87 years old, 88 in a few months, but potions don't benefit only witches and she's been in peak prime for the last 6 and a half decades, committed to either dying on the job or for the universe to tell her when it's time to settle down, but no end in sight."
    , positives =
        [ "+ Highly professional"
        , "+ Quickly adapts to teams."
        ]
    , negatives =
        [ "- Nearly no personal time."
        , "- Not a witch."
        ]
    , mixed = [ "+/- On speaking terms with some suits at Alphazon enough to acquire some of their tech without much pressure" ]
    , has = "V has Integration 4 (_No Observer_), and Gadgetry 5 with the _ORC License_ and _Collection_"
    }


richardMaxJohnson : Details
richardMaxJohnson =
    { name = RichardMaxJohnson
    , class = Nothing
    , race = Just Neutral
    , hasPerk = False
    , cost = 5
    , power = NotAWitch
    , teamwork = 7
    , sociability = 5
    , morality = 6
    , quote = "_*“He came back from a mission one time with a chainsaw tied to the stump of a missing hand, and a shotgun pegleg” - Doc Ginger.*_"
    , description = "The quintessential male action hero made flesh, Max is the love child between Indiana Jones, Clint Eastwood, and MacGyver with a splash of Geralt of Rivia. He gets the job done one way or another and doesn't shy away from risk or pain, Doesn't drink because he hates anything that dulls his perception, and alcoholism cost him is first marriage, Avoids showing emotion in front of anyone, including anger (within reason, outside of combat), Has many safehouses memorized."
    , positives =
        [ "+ Rugged dependability."
        , "+ “Rub some dirt on it”"
        ]
    , negatives =
        [ "- Rough around the edges"
        , "- Horny bastard (with self control)"
        ]
    , mixed = [ "+ Can produce gadgets from a box of scraps & bubblegum" ]
    , has = "Max has Gadgetry 5, _ORC License_, and a metric Freedom load of firearms and ammo"
    }


bethadonnaRossbaum : Details
bethadonnaRossbaum =
    { name = BethadonnaRossbaum
    , class = Just Warlock
    , race = Just Lilin
    , hasPerk = True
    , cost = 12
    , power = Witch 1
    , teamwork = 8
    , sociability = 9
    , morality = 5
    , quote = "_*“I... I heard Bethadonna was a cultist for a long time before joining up... The things she must have seen...and done..” - An ORC Agent.*_"
    , description = "Bethadonna is a domestic goddess that radiates warmth and a motherly presence in contrast to the devilish horns you might miss at a glance from the front, nevermind if your eyes didn't have somewhere else drawing their attention. She acts as a detective utilizing spirits and communion to work scenes, while being a heavy hitter for when SHTF. She cultivates a private pond of Golden Fish for her rituals."
    , positives =
        [ "+ Particularly unkillable"
        , "+ Has many, many pets"
        ]
    , negatives =
        [ "- A bit smothering & handsy."
        , "- Scary combat transformation"
        ]
    , mixed = [ "+/- Sweet christmas, you do _not_ want to make mother mad." ]
    , has = "Beth has Familiarity 5, Consortation 5, Necromancy Portals 5, Occultism 5, and _ORC License, Golden Fish, Third Eye, and Pantomime_. _Wished for Power_."
    }


mirandaQuincy : Details
mirandaQuincy =
    { name = MirandaQuincy
    , class = Just Academic
    , race = Just Erinyes
    , hasPerk = True
    , cost = 8
    , power = Witch 8
    , teamwork = 6
    , sociability = 7
    , morality = 6
    , quote = "_*“Why have doctors when healing potions are a thing?”* \\*bonk\\* *“Nitwit, potions are expensive, for emergencies.” - An ORC Agent.*_"
    , description = "Miranda, also called Gingerdoc or Doc Ginger, is a medical and general life-saving specialist responsible for keeping people fit for duty and enables risk taking in the face of overwhelming odds with her mastery of anatomy and what keeps a body ticking. That, and magic potions, of course She spends a lot of time micromanaging resources to keep everyone in shape and supplied so there's no excuse for a loss."
    , positives =
        [ "+ Legitimate doctor for real"
        , "+ Values her word / promises"
        ]
    , negatives =
        [ "- Somewhat cold and sharp."
        , "- Brutally honest"
        ]
    , mixed = [ "+/- Doesn't believe in mercy, for enemies, or for allies; Will save your life no matter the pain, to eventually recover" ]
    , has = "Doc has Gadgetry 2, Hexes 3, Witchery 4, Firecalling 4, Potions 5, and Familiar 5 (Fox), _ORC License_"
    }


alphazonians : List Details
alphazonians =
    [ samanthaNatPonds, jenniferKYoung, agent7Y, agent9s ]


samanthaNatPonds : Details
samanthaNatPonds =
    { name = SamanthaNatPonds
    , class = Nothing
    , race = Just Neutral
    , hasPerk = False
    , cost = 8
    , power = NotAWitch
    , teamwork = 6
    , sociability = 10
    , morality = 5
    , quote = "_*“Seventh quarterly is in, 12% average across the board. Asset 4z13-Samantha Ponds, continues to excel above expectations” - An Exec.*_"
    , description = "Sam is a high level Alphazon agent known for cooperation with the ORC. She has a preferred appearance as shown, but makes full use of her skinsuit and full body replacements and remote operations to make more use of changing forms, and whole bodies, more than even most expert Hex witches She takes it a step further by having trained her personality to become highly adaptive and instinctively suit any given situation. She doesn't really know who she is anymore."
    , positives =
        [ "+ Very low maintenance."
        , "+ Incredibly adaptive."
        ]
    , negatives =
        [ "- Literal maintenance; Synth"
        , "- No self-drive, craves purpose."
        ]
    , mixed = [ "+/- Not naive, but doesn't ask questions that aren't directly relevant to a given task" ]
    , has = "Sam has Divination 3, Gadgetry 4, Integration 5"
    }


jenniferKYoung : Details
jenniferKYoung =
    { name = JenniferKYoung
    , class = Nothing
    , race = Just Neutral
    , hasPerk = False
    , cost = 5
    , power = NotAWitch
    , teamwork = 4
    , sociability = 8
    , morality = 4
    , quote = "_*“Lucky bastard in O5 was assigned to monitor Asset f9E21's observer, and cohost.”* \\*Sigh\\* *“Could have been me...” - an Operator.*_"
    , description = "Jen... is not a typical agent or employee of Alphazon. She's a daughter of a high level executive that grew up never knowing the feeling of being unable to get something she wanted and was a designer baby derived from daeva genetics. This didn't make her a witch, but the results do speak for themselves. She's somewhat naive and spoiled, but in a minimally toxic way, understanding her position and being   grateful for it, though actively indulges in vices."
    , positives =
        [ "+ Peak party girl"
        , "+ Generous in all respects."
        ]
    , negatives =
        [ "- Can be a lot to handle"
        , "- _Very_ extroverted"
        ]
    , mixed = [ "+/- Smarter than she presents herself to be, as an easy ditzy airhead, and does contribute with info gathering." ]
    , has = "Jen has Divination 3, Integration 4 & a _Gold Card_"
    }


agent7Y : Details
agent7Y =
    { name = Agent7Y
    , class = Nothing
    , race = Just Neutral
    , hasPerk = False
    , cost = 12
    , power = NotAWitch
    , teamwork = 7
    , sociability = 7
    , morality = 1
    , quote = "_*“A-7Y is a mastermind with the utilization of the GS Arrays, upper management is considering assigning her 2 more,” - An Operator.*_"
    , description = "“Sev” is a human working for Alphazon that operates the 4th and 7th Godsight Arrays; Stealth satellite-ships with colossal telescope arrays that look like large canons running their full length while hosting Alphazon server farms and datacrypts housing the phylactery-like pods that host the minds of those implanted with Observers, semi-autonomously operated via an Observer like a synth body."
    , positives =
        [ "+ Speaking terms with Lunabella"
        , "+ Can tell you how your ex is doing."
        ]
    , negatives =
        [ "- No morals, only corporate policy."
        ]
    , mixed = [ "+/- Can read your ID through the reflection on the wall through the window. Knows what you did last night." ]
    , has = "Sev has Integration 5 (many reserve bodies), DIvination 5, & _Gold Card_, (+2 Spysats, with observers)"
    }


agent9s : Details
agent9s =
    { name = Agent9s
    , class = Just Warlock
    , race = Just Aurai
    , hasPerk = False
    , cost = 10
    , power = Witch 8
    , teamwork = 4
    , sociability = 6
    , morality = 2
    , quote = "_*“I reviewed the Observer case logs of A-98. Twelve vampire ninjas. Twelve. With a hairpin and two missing limbs.” - An Operator.*_"
    , description = "“Nines” was a common human employee before awakening and finding a niche with potions and ritualcasting for some time before coming into her greater potential; As a corporate hitman with her spider familiar. She has a legitimate business front as a dayjob with Alphazon that sees her traveling a lot “for business”, does love to relax though."
    , positives =
        [ "+ Remarkably chill and homey in private"
        , "+ As tenacious as Max"
        ]
    , negatives =
        [ "- May ‘disappear’ people that annoyed her"
        , "- Used to working alone."
        ]
    , mixed = [ "+/- A semi-lethal rivalry with the ORC, Richard Max in particular, a little Mr. and Ms. Smith style" ]
    , has = "Nines has Integration 2, Witchery 3, Potions 3, Occultism 4, Psychotics 4, Familiarity 5 & _Gold Card_"
    }


independents : List Details
independents =
    [ alexKHalls, isabellaMableOaks, evangelinaRosaCostaval, penelope ]


alexKHalls : Details
alexKHalls =
    { name = AlexKHalls
    , class = Just Sorceress
    , race = Just Neutral
    , hasPerk = False
    , cost = 2
    , power = Witch 10
    , teamwork = 7
    , sociability = 5
    , morality = 8
    , quote = "_*“I sensed this one awaken not long ago. Poor thing was lost and confused while her human family didn't notice a change.” - Penelope.*_"
    , description = "Alex is a brand new witch that just awakened a few days ago... and is still a little slow to adjust to losing her... trunk, having been a man this time last Tuesday. She's an adorable fish out of water that doesn't know much of anything, she visits Penelope's shop when she has the free time to learn while trying to keep her previous life going as best she can, most humans adjust as though she always was a woman, but it's more complicated than that. Wanna help her out?"
    , positives =
        [ "+ Brand new blank canvas."
        , "+ “Girl next door” cute."
        , "+ Straight A student."
        ]
    , negatives =
        [ "- No experience at all"
        , "- Still in High School {smol (But legal)}"
        , "- Needs new clothes."
        ]
    , mixed = [ "+/- Really easily flustered by her new body and attention." ]
    , has = "Alex has Familiarity 1 (Cat), & _Jack-of-All_"
    }


isabellaMableOaks : Details
isabellaMableOaks =
    { name = IsabellaMableOaks
    , class = Nothing
    , race = Nothing
    , hasPerk = False
    , cost = 15
    , power = SpecialEffect
    , teamwork = 6
    , sociability = 9
    , morality = 8
    , quote = "_*“I detected this one earlier before you showed up. She hasn't awakened yet, but should naturally within the week.” - Penelope.*_"
    , description = "Isabella is for now a human going about her life attending her first year of college while working part-time at a convenience store, spending most her free time volunteering for events and charities, or service projects. When she's not at class, work, or volunteering, she's cooking or cleaning. if not for herself then for her wheelchair-bound mom, missing both legs from an accident."
    , positives =
        [ "+ Incredibly wholesome"
        , "+ Widely loved by people"
        ]
    , negatives =
        [ "- Very unathletic."
        , "- Usually busy volunteering."
        ]
    , mixed = [ "+/- Has never even dreamed of causing harm to anybody will take a lot to be much help in a fight." ]
    , has = "“I dunno what she will have yet.” (Build her up as a second build from scratch)"
    }


evangelinaRosaCostaval : Details
evangelinaRosaCostaval =
    { name = EvangelinaRosaCostaval
    , class = Nothing
    , race = Just Neutral
    , hasPerk = False
    , cost = 12
    , power = SpecialEffect
    , teamwork = 7
    , sociability = 8
    , morality = 5
    , quote = "_*“Rosa is in town, she's very capable and perhaps you two could come to... a mutually beneficial partnership.” - Penelope.*_"
    , description = "Rosa is an old vampire _(As per second-hand Immortal Blood)_ with an unnatural ability to copy magical abilities temporarily, and with her many years of life, she's learned to make good use of most powers out there. For a time she was a member of the Hespatian Coven, but has largely distanced herself from them in recent years, acting on her own. She doesn't know if her master is still alive or not, and the thought of his return fills her with dread."
    , positives =
        [ "+ Charming and pleasant"
        , "+ Very knowledgeable."
        ]
    , negatives =
        [ "- Can't disobey the vampire that turned her, her master,"
        ]
    , mixed = [ "+/- Is stronger when well fed, weakens with hunger" ]
    , has = "Rosa is a vampire + copies the (magical) power & perks of the last person she fed on"
    }


penelope : Details
penelope =
    { name = Penelope
    , class = Just Sorceress
    , race = Just Daeva
    , hasPerk = False
    , cost = 10
    , power = Witch 4
    , teamwork = 6
    , sociability = 8
    , morality = 6
    , quote = "_*\"Oh hey, would you look at that. Looks like I might be in your future myself, You can crash here if you want.\" - Penelope.*_"
    , description = "I'm technically a Lunabellan at heart, but I've carefully positioned myself as a neutral party as a Guidance Witch that can induce early magical awakenings in witches, so I live hidden among human society with eyes open for new witches, or those not yet awake. None of the big factions bother me as a result, so long as I do my best not to play favorites."
    , positives =
        [ "+ Is this where I say an Office quote?"
        , "+ I've got a great rack?"
        ]
    , negatives =
        [ "- None, clearly"
        , "- I must stay neutral"
        ]
    , mixed = [ "+/- \"Sometimes, I just care too much, I love to travel and take long walks on the beach. I'm not like other girls\" /s" ]
    , has = "Pen has Potions 3, Hexes 3, Domain 3, Familiar 4 (Cow), Witchery 4, Necromancy 5, Divination 5, _Cornucopia, Prestidigitation, Third Eye & Yaga Root._"
    }
