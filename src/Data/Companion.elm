module Data.Companion exposing (Details, all, intro)

import Generated.Types exposing (Class(..), Companion(..), Faction(..), Race(..))


type alias Details =
    { name : Companion
    , shortName : String
    , class : Class
    , race : Race
    , hasPerk : Bool
    , cost : Int
    , power : Int
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


intro : String
intro =
    """
    It’s dangerous to go alone, take this! A Companions section. {choice You have one free companion from your chosen faction, and one free that shares the same witch type} as you (either Race or Class). Companions have 4 main statistics giving a _rough idea_ of their general impact in a group dynamic, except the {choice POWER stat, which primarily serves to give free floating Power points to spend to _customize_ your companion's abilities} on top of the default abilities they come with. Beyond that, interactions are inferred in how you interpret their personality and how different stats play out in relation to it and others. {choice You can buy additional companions using Reward Points}, listed to the left of their name. A {choice +} indicates they have their Type Perk.

    You can spend your own Power on behalf of a member if you so choose.

    **Really, don't stress the details too much and feel free to fill in the blanks. I do what I can to provide a reasonable summary of an array of characters, but it's up to you to interpret it as you will.**

    Take your pick:
    """


all : List ( String, Faction, List Details )
all =
    [ ( "The Arcadians", Arcadia, arcadians )
    , ( "Hawthorne", Hawthorne, hawthorne )
    , ( "The Watchers", Watchers, watchers )
    , ( "The Hespatians", Hespatian, hespatians )
    ]


arcadians : List Details
arcadians =
    [ rachelPool, anneLaurenchi, candayWesbank, tessaMarieKudashov, evelynnPWillowcrane, johnDoe ]


rachelPool : Details
rachelPool =
    { name = RachelPool
    , shortName = "Rachel"
    , class = Academic
    , race = Neutral
    , hasPerk = False
    , cost = 4
    , power = 5
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
    , has = "Potions 2, Witchery 1, Hexes 2, Familiarity 3, and Naturalism 3"
    }


anneLaurenchi : Details
anneLaurenchi =
    { name = AnneLaurenchi
    , shortName = "Anne"
    , class = Academic
    , race = Sylph
    , hasPerk = True
    , cost = 4
    , power = 4
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
    , has = "Familiar 1, Aether 2, Portal 3, Psycho 3, Digicasting 4, as well as _Beauty Sleep_"
    }


candayWesbank : Details
candayWesbank =
    { name = CandayWesbank
    , shortName = "Wess"
    , class = Sorceress
    , race = Neutral
    , hasPerk = False
    , cost = 2
    , power = 4
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
    , has = "Runes 5 and an Improved Familiar"
    }


tessaMarieKudashov : Details
tessaMarieKudashov =
    { name = TessaMarieKudashov
    , shortName = "Tess"
    , class = Warlock
    , race = Neutral
    , hasPerk = False
    , cost = 2
    , power = 8
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
    , has = "Potions 3, Hexes 4, has a _Hydron_"
    }


evelynnPWillowcrane : Details
evelynnPWillowcrane =
    { name = EvelynnPWillowcrane
    , shortName = "Evelynn"
    , class = Academic
    , race = Neutral
    , hasPerk = False
    , cost = 2
    , power = 4
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
    , has = "Waterworking 1, Witchery 1, Divination 2, Digicasting 2, Runes 3 & _Fascinate_"
    }


johnDoe : Details
johnDoe =
    { name = JohnDoe
    , shortName = "John"
    , class = Sorceress
    , race = Changeling
    , hasPerk = False
    , cost = 4
    , power = 6
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
    , has = "Alchemy, Runes, and Witchery at 2, and unspent _Jack-of-All_, and _Memorize_"
    }


hawthorne : List Details
hawthorne =
    [ hannahGrangely, elizabellSinclaire, ashleyLovenko, sylvanneMaeKanzaki ]


hannahGrangely : Details
hannahGrangely =
    { name = HannahGrangely
    , shortName = "Hannah"
    , class = Academic
    , race = Neutral
    , hasPerk = False
    , cost = 6
    , power = 5
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
    , has = "1 in every core Specialization, and has Waterworking 3, Wands 4 & _Master Wand_."
    }


elizabellSinclaire : Details
elizabellSinclaire =
    { name = ElizabellSinclaire
    , shortName = "Eliza"
    , class = Warlock
    , race = Erinyes
    , hasPerk = False
    , cost = 10
    , power = 2
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
    , has = "Wands 3, Occultism 3, Witchery 3, Consortation 4, Windkeeping 5, and _Energize_."
    }


ashleyLovenko : Details
ashleyLovenko =
    { name = AshleyLovenko
    , shortName = "Ash"
    , class = Sorceress
    , race = Neutral
    , hasPerk = False
    , cost = 6
    , power = 3
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
    , has = "Familiarity 2, Witchery 2, Firecalling 3, Windkeeping 4, Wands 5 & _Magic Friendship_."
    }


sylvanneMaeKanzaki : Details
sylvanneMaeKanzaki =
    { name = SylvanneMaeKanzaki
    , shortName = "Mae"
    , class = Sorceress
    , race = Luxal
    , hasPerk = True
    , cost = 8
    , power = 10
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
    , has = "Wands 1, Familiarity 2 (Hawk), Ministration 3, Witchery 4, _Hat Trick_ and _Sun Shard_."
    }


watchers : List Details
watchers =
    [ francisIsaacGiovanni, ifraAlZahra, sariahJSnow, claireBelmontegra ]


francisIsaacGiovanni : Details
francisIsaacGiovanni =
    { name = FrancisIsaacGiovanni
    , shortName = "Isaac"
    , class = Warlock
    , race = Neutral
    , hasPerk = False
    , cost = 6
    , power = 6
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
    , has = "Consortation 4, Ministration 4, Runes 5, _Sun Shard_, _Mythril Armor_, and _Memorize_."
    }


ifraAlZahra : Details
ifraAlZahra =
    { name = IfraAlZahra
    , shortName = "Ifra"
    , class = Warlock
    , race = Changeling
    , hasPerk = False
    , cost = 4
    , power = 3
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
    , shortName = "Sariah"
    , class = Academic
    , race = Nymph
    , hasPerk = True
    , cost = 8
    , power = 4
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
    , has = "Necromancy 2, Witchery 3, Divination 4, Waterworking 5, Ministation 5, _Oracle_, _Mythril Armor_ & _Artifact Keeper_"
    }


claireBelmontegra : Details
claireBelmontegra =
    { name = ClaireBelMontegra
    , shortName = "Claire"
    , class = Sorceress
    , race = Nymph
    , hasPerk = True
    , cost = 6
    , power = 5
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
    , has = "Curses 2, Ministration 3, Wind 4, Witchery 4, Hexes 5 & _False Light_"
    }


hespatians : List Details
hespatians =
    [ lucilleMBright, kingDaemianKain, whisper, redMother ]


lucilleMBright : Details
lucilleMBright =
    { name = LucilleMBright
    , shortName = "Claire"
    , class = Academic
    , race = Lilin
    , hasPerk = False
    , cost = 6
    , power = 5
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
    , has = "Necromancy 3, Hexes 4, Consortation 4, Divination 4, Familiarity 5, _Blood Witch, Necronomicon & Hellrider_"
    }


kingDaemianKain : Details
kingDaemianKain =
    { name = KingDaemianKain
    , shortName = "Kain"
    , class = Sorceress
    , race = Dravir
    , hasPerk = True
    , cost = 12
    , power = 10
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
    , has = "Familiarity 3, Firecalling 4, Curses 5, Consortation 5, Metamorphize 5 (Dragon) _Secret Magic & Family Line_"
    }


whisper : Details
whisper =
    { name = Whisper
    , shortName = "Whisper"
    , class = Warlock
    , race = Neutral
    , hasPerk = False
    , cost = 10
    , power = 9
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
    , has = "Witchery 3, Familiarity 5, Occultism 5, Psychotics 5, _Visceramancy 5, Toximancy, Blood Witch, & Shroud_"
    }


redMother : Details
redMother =
    { name = RedMother
    , shortName = "Mom"
    , class = Academic
    , race = Neutral
    , hasPerk = False
    , cost = 8
    , power = 6
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
    , has = "Occultism 4, Necromancy 5, Alchemy 5, Consortation 5, _Visceramancy 5, Blood Witch, Family Line, and Immortal Blood_"
    }
