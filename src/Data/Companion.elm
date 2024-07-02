module Data.Companion exposing (Details, MaybeClass(..), Score(..), all, intro)

import Generated.Types exposing (Affinity(..), Class(..), Companion(..), Faction(..), Race(..))


type alias Details =
    { name : Companion
    , class : MaybeClass
    , races : List Race
    , hasPerk : Bool
    , cost : Maybe Int
    , power : Score
    , teamwork : Score
    , sociability : Score
    , morality : Score
    , quote : String
    , description : String
    , positives : List String
    , negatives : List String
    , mixed : List String
    , has : String
    }


type MaybeClass
    = ClassAny
    | ClassOne Class
    | ClassSpecial
    | ClassNone


type Score
    = NormalScore Int
    | SpecialEffect { better : Int, worse : Maybe Int }


intro : String
intro =
    """
    It’s dangerous to go alone, take this! A Companions section. {choice You have one free companion from your chosen faction, and one free that shares the same witch type} as you (either Race or Class). Companions have 4 main statistics giving a _rough idea_ of their general impact in a group dynamic, except the {choice POWER stat, which primarily serves to give free floating Power points to spend to _customize_ your companion’s abilities} on top of the default abilities they come with. Beyond that, interactions are inferred in how you interpret their personality and how different stats play out in relation to it and others. {choice You can buy additional companions using Reward Points}, listed to the left of their name. A {choice +} indicates they have their Type Perk.

    {choice You can spend your own Power on behalf of a member if you so choose, although they receive 1/2 the Reward value of Quests, so you don’t _need to_.}

    **Really, don’t stress the details too much and feel free to fill in the blanks. I do what I can to provide a reasonable summary of an array of characters, but it’s up to you to interpret it as you will.**

    Take your pick:
    """


all : List ( String, Maybe Faction, List Details )
all =
    [ ( "The Arcadians", Just TheCollegeOfArcadia, arcadians )
    , ( "Hawthorne", Just HawthorneAcademia, hawthorne )
    , ( "The Watchers", Just TheWatchers, watchers )
    , ( "The Hespatians", Just TheHespatianCoven, hespatians )
    , ( "The Lunabellans", Just Lunabella, lunabellans )
    , ( "The ORCs / Badges", Just TheORC, theOrcs )
    , ( "The Alphazonians / Suits", Just AlphazonIndustries, alphazonians )
    , ( "Independents / Other", Nothing, independents )
    , ( "Outsiders", Just TheOutsiders, outsiders )
    , ( "Alliance", Just AlfheimrAlliance, alliance )
    ]


arcadians : List Details
arcadians =
    [ rachelPool, anneLaurenchi, candayWesbank, tessaMarieKudashov, evelynnPWillowcrane, johnDoe, sayaKurosawa, francescaAstrenichtys, elaineAVictorica, maimonadaMajesteim, azurellieaAdMadelline, melissaVincimvitch ]


rachelPool : Details
rachelPool =
    { name = RachelPool
    , class = ClassOne Academic
    , races = [ Neutral ]
    , hasPerk = False
    , cost = Just 4
    , power = NormalScore 5
    , teamwork = NormalScore 8
    , sociability = NormalScore 4
    , morality = NormalScore 9
    , quote = "_*“Has anyone seen Rachel around? We have a double date in 20 minutes and she totally vanished.” - an Arcadian student.*_"
    , description =
        """Rachel is very introverted and socially awkward, she grew up largely alone without a lot of interaction, so other people may as well be aliens for her and she freaks out about saying the wrong thing, but once she makes real friends she cares for them a lot and can find her stride. She’s very fond of animals and spends a lot of time with her cat, and going on walks alone as to not bother anybody."""
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
    , class = ClassOne Academic
    , races = [ Sylph ]
    , hasPerk = True
    , cost = Just 4
    , power = NormalScore 4
    , teamwork = NormalScore 4
    , sociability = NormalScore 6
    , morality = NormalScore 7
    , quote = "_*“Is..Is that Anne? How on earth can she sleep like that. Is that safe? feel like I should help her out” - An Arcadian student.*_"
    , description = "Anne is a bit of an oddball. Rather than simply being introverted, she’s pretty much outright antisocial seeing no need or reason to bother and actively prefers keeping a smaller friendgroup, but she tries to do right by those that do make that cut. She’s known for sleeping most her days away and not just in her bed, but anywhere she goes, some think she has narcolepsy; but she just enjoys dreams a ton."
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
    , class = ClassOne Sorceress
    , races = [ Neutral ]
    , hasPerk = False
    , cost = Just 2
    , power = NormalScore 4
    , teamwork = NormalScore 8
    , sociability = NormalScore 7
    , morality = NormalScore 8
    , quote = "_*“Wess has been out there all day long helping clean up the river after that necroshark attack.” - An Arcadian student.*_"
    , description = "Goes by “Wess”, to avoid people calling her “Candy” which rubs her the wrong way, but she doesn’t make a big deal about it and goes with the flow. She has a calm reserved intelligence about the way she carries herself in a mature manner that suggests a lot of life experience that has seen some of the worst life has to offer, but also some of the best, and she appreciates what she can. She’s a bit of a watchdog, always locking out for others, friend or rival."
    , positives =
        [ "+ Very competent and focused."
        , "+ Very physically fit and lean."
        , "+ MMA background."
        ]
    , negatives =
        [ "- She keeps it in check, but she’s very paranoid."
        , "- Struggles to sleep."
        ]
    , mixed = [ "+/- Celebate until marriage. {smol (Though not a virgin).}" ]
    , has = "Wess has Runes 5 and an Improved Familiar"
    }


tessaMarieKudashov : Details
tessaMarieKudashov =
    { name = TessaMarieKudashov
    , class = ClassOne Warlock
    , races = [ Neutral ]
    , hasPerk = False
    , cost = Just 2
    , power = NormalScore 8
    , teamwork = NormalScore 5
    , sociability = NormalScore 6
    , morality = NormalScore 4
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
    , class = ClassOne Academic
    , races = [ Neutral ]
    , hasPerk = False
    , cost = Just 2
    , power = NormalScore 4
    , teamwork = NormalScore 9
    , sociability = NormalScore 10
    , morality = NormalScore 7
    , quote = "_*“You’re heading out with Evelynn again? I want to come too! She always finds the best places”. - An Arcadian student.*_"
    , description = "Evelynn is a high functioning socialite that always seems to know what someone needs to hear to get back on their feet or to keep a party going. She’s an active go-getter always setting up new activities and getting people to attend without dragging their feet, and is as adventurous as she is social, finding people to tag along as she heads out to explore to find interesting places to host events."
    , positives =
        [ "+ Outgoing"
        , "+ Empathetic"
        , "+ Adaptable and cooperative"
        ]
    , negatives =
        [ "- Not the most powerful"
        , "- Can be caught up in the moment"
        , "- A _little_ pushy, tactfully"
        ]
    , mixed = [ "+/- Also known for being dtf" ]
    , has = "Evelynn has Waterworking 1, Witchery 1, Divination 2, Digicasting 2, Runes 3 & _Fascinate_"
    }


johnDoe : Details
johnDoe =
    { name = JohnDoe
    , class = ClassOne Sorceress
    , races = [ Changeling ]
    , hasPerk = False
    , cost = Just 4
    , power = NormalScore 6
    , teamwork = NormalScore 7
    , sociability = NormalScore 5
    , morality = NormalScore 10
    , quote = "_*“Little man is so f#&% cute holy shit”* \\*John turning red\\* *“Haha, he gets even cuter when embarrassed!” - An Arcadian student.*_"
    , description = "John is a very uncommon male witch... And rarer still: A Changeling. He can’t remember much of his past so doesn’t know how old he really is, but he tries to look as old as he can, He must have been a soldier or knight pre-awakening, because he has a firmly rooted sense of chivalry, justice, and mercy, and great reflexes and is a natural with weapons He’s eager to learn all he can and studies his lessons closely."
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


sayaKurosawa : Details
sayaKurosawa =
    { name = SayaKurosawa
    , class = ClassOne Academic
    , races = [ Neutral ]
    , hasPerk = False
    , cost = Just 8
    , power = NormalScore 8
    , teamwork = NormalScore 5
    , sociability = NormalScore 8
    , morality = NormalScore 6
    , quote = "_*“Don’t worry, Saya may be new but she knows what she’s doing, she’s good. Just keep that photo of Elaine handy.” - Arcadian faculty*_"
    , description = "Saya is an experienced but still new witch that was motivated with the help of Elaina to improve herself, and ever since has been driven with focus, mostly just to try to get the attention of Elaina. She has a huge one-sided crush that she isn’t shy about sharing, but they remain friends. She works with faculty as a problem solver, and runner."
    , positives =
        [ "+ Very energetic and lively"
        , "+ Remarkably witty under the surface."
        ]
    , negatives =
        [ "- Perhaps too lively worked up."
        , "- A _bit_ manipulative and may resort to questionable actions"
        ]
    , mixed =
        [ "+/- Perhaps you could redirect her feelings from Elaina with some time and effort, if you wanted?"
        ]
    , has = "Saya has Runes 1, Wands 3, Witchery 4, Digicasting 4, Broom Beast (4p), _Mana Core, & a Witch Pistol_"
    }


francescaAstrenichtys : Details
francescaAstrenichtys =
    { name = FrancescaAstrenichtys
    , class = ClassOne Sorceress
    , races = [ Neutral ]
    , hasPerk = False
    , cost = Just 12
    , power = NormalScore 5
    , teamwork = NormalScore 7
    , sociability = NormalScore 8
    , morality = NormalScore 6
    , quote = "_*“Have you seen Francesca duel Avurelliea over who had to chaperone the dance? It was insane. We have a lot to learn.” - Some students*_"
    , description = "Fran is a retired Hawthorne professor who has taken to Arcadia, where she was a private tutor to Elaina as a favor to her mother, who was her own teacher when she was younger. Fran at first glance is a lighthearted whimsical character with a pleasant smile, but within is a very sharp and thoughtful tactician that can at times be rather cold hearted with razor precision in cutting to the heart of issues."
    , positives =
        [ "+ Very easy going but serious about the important things"
        ]
    , negatives =
        [ "- The reason she’s so efficient is because she’s fairly lazy, to get things over with sooner."
        ]
    , mixed =
        [ "+/- Loves butterflies, visually influences her magic"
        ]
    , has = "Fran has Digicasting 1, Earthmoving 3, Firecalling 3, Aethernautics 3, Metallurgy 3, Runes 4, Alchemy 5, Wands 5 _& a Mana Core_"
    }


elaineAVictorica : Details
elaineAVictorica =
    { name = ElaineAVictorica
    , class = ClassOne Academic
    , races = [ Neutral ]
    , hasPerk = True
    , cost = Just 10
    , power = NormalScore 10
    , teamwork = NormalScore 7
    , sociability = NormalScore 6
    , morality = NormalScore 5
    , quote = "_*“You met her?! Where? Alfheimr? Earth 84x? Where is she this time? Did she say where she was going next?” - an excited Saya*_"
    , description = "Elaina’s first memories are as a child looking up to skilled witches and started training herself early before studying under Fran. Rough treatment from some witches early on helped temper her personality development, but she does have a high opinion of herself She loves traveling, Arcadia and other worlds alike, and is easily motivated by [K]"
    , positives =
        [ "+ Confident to say the least"
        , "+ Very polite"
        ]
    , negatives =
        [ "- While personable, it can be hard to actually get close"
        ]
    , mixed =
        [ "+/- While “yuri” is normal among witches due to gender disparity, unfortunately for Saya, Elaina is very straight, she doesn’t even think in that way to recognize such options."
        ]
    , has = "Elaina has Digicasting 1, Witchery 4, Potions 4, Wands 5, _Broom Beast_ (6p), _Mana Core, & a Witch Pistol_"
    }


maimonadaMajesteim : Details
maimonadaMajesteim =
    { name = MaimonadaMajesteim
    , class = ClassOne Warlock
    , races = [ Neutral ]
    , hasPerk = False
    , cost = Just 12
    , power = NormalScore 9
    , teamwork = NormalScore 6
    , sociability = NormalScore 7
    , morality = NormalScore 5
    , quote = "_*“Mai! Good grief when was the last time you’ve eaten? A WEEK! You know you’re always welcome to come over for dinner!” - Liz*_"
    , description = "Mai comes from a family of seers with a strong magical bloodline. She’s known for combining her aethernautic studies of stellar motions, and her third eye and general intuition for interpreting her divinations to deliver some of the most predictions around, but she doesn’t believe in using divination for financial gain."
    , positives =
        [ "+ Semi-unique ability"
        , "+ Has a 6th sense for good bargains."
        ]
    , negatives =
        [ "- Has a strong sense of etiquette & propriety, is insulted when others don’t treat her in kind."
        ]
    , mixed =
        [ "+/- She is very frugal and lives lightly, being content with the basics despite having expensive tastes and interests."
        ]
    , has = "Mai has Necromancy 2, Waterworking 3, Portals 4, Witchery 4, Aethernautics 5, Divination 5, _Oracle, Third Eye, Prestidigitation_"
    }


azurellieaAdMadelline : Details
azurellieaAdMadelline =
    { name = AzurellieaAdMadelline
    , class = ClassOne Sorceress
    , races = [ Dwarf, Lilin ]
    , hasPerk = False
    , cost = Just 15
    , power = NormalScore 8
    , teamwork = NormalScore 8
    , sociability = NormalScore 6
    , morality = NormalScore 7
    , quote = "_*“Look, if you really want to improve for that tournament in time, you could try to get Azure to tutor you? Think about it.” - A Student*_"
    , description = "A Dwarf-Lilin girl who’s strive for power took her across the aether and to hell and back as a _Dalililah_. She now takes comfort in teaching in Arcadia as a private tutor as some soul healing. Remote friends with Bethadonna Rossbaum and tutors Evelyn in Waterworking. She’s lived comfortably for a while now but may easily be driven back to adventure, especially for someone they care about"
    , positives =
        [ "+ Very patient with people trying to learn"
        , "+ Mostly Unspent wish(20p)"
        ]
    , negatives =
        [ "- While good in a team and normal social skills, is a loner with trust issues."
        ]
    , mixed =
        [ "+/- Not opposed to choking perverts out with cold water"
        ]
    , has = "Azure has Necromancy 1, Wands 2, Witchery 3, Waterworking 5 & Consortation 5. Wished for Power & Life (4 & 19)"
    }


melissaVincimvitch : Details
melissaVincimvitch =
    { name = MelissaVincimvitch
    , class = ClassOne Academic
    , races = [ Daeva ]
    , hasPerk = False
    , cost = Just 15
    , power = NormalScore 2
    , teamwork = NormalScore 8
    , sociability = NormalScore 10
    , morality = NormalScore 9
    , quote = "_*“I can’t believe that damn big tiddy witch tazed me, how was I supposed to know that cheap ass book couldn’t float” - A rude student*_"
    , description = "Melissa, “Liz” or Lisa to some, is a librarian of the *Vincimvitch* family’s private library which was given a static reference plot in Arcadia, meaning out of the city but static throughout cycles. The library is a growing Death Ward of her grandmother’s who likes to give her grandaughters jobs around the Ward for excuses to keep her darlings near, and this personality rubbed off on Liz, who acts quite motherly."
    , positives =
        [ "+ Prodigious memory."
        , "+ Highly considerate."
        ]
    , negatives =
        [ "- Big Ara Ara energy."
        , "- But strict with expectations"
        ]
    , mixed =
        [ "+/- Teaches Witch History & Alchemy, & tutors runes"
        ]
    , has = "Liz has Portals 1, Witchery 3, Divination 4, Alchemy 5, Runes 5, Windkeeping 5, _Energized, Third Eye, Windsong_. + Keeper x2 _Alchemy & Eromancy_* _{smol Don’t get the wrong idea perv}_*"
    }


hawthorne : List Details
hawthorne =
    [ hannahGrangely, elizabellSinclaire, ashleyLovenko, sylvanneMaeKanzaki, lauraDDevonshire, caroline, suzyTheMiasma, norikoDuNichols ]


hannahGrangely : Details
hannahGrangely =
    { name = HannahGrangely
    , class = ClassOne Academic
    , races = [ Neutral ]
    , hasPerk = False
    , cost = Just 6
    , power = NormalScore 5
    , teamwork = NormalScore 7
    , sociability = NormalScore 6
    , morality = NormalScore 7
    , quote = "_*“I’m about ready to throw myself in a snake pit over this assignment, I need to ask Hannah to tutor me tonight.” - A Hawthorne Student.*_"
    , description = "Hannah is top of her class, an excellent student that has the rules memorized and wholeheartedly embraces Hawthorne methods. She applied for the hardest courses and assigned House Lionfeather. She rarely makes the same mistake twice, and when she’s not practicing magic, she’s reading theory, or helping housemates with problems they’re having trouble with."
    , positives =
        [ "+ Knowledgeable"
        , "+ Good at teaching"
        , "+ Always up to date on policy."
        ]
    , negatives =
        [ "- Unintentionally arrogant"
        , "- Not great at reading people,"
        , "- Doesn’t get out much"
        ]
    , mixed =
        [ "+/- Helps avoid rulebreaking, but obligated to report" ]
    , has = "Hannah has 1 in every core Specialization, and has Waterworking 3, Wands 4 & _Master Wand_."
    }


elizabellSinclaire : Details
elizabellSinclaire =
    { name = ElizabellSinclaire
    , class = ClassOne Warlock
    , races = [ Erinyes ]
    , hasPerk = False
    , cost = Just 10
    , power = NormalScore 2
    , teamwork = NormalScore 7
    , sociability = NormalScore 6
    , morality = NormalScore 5
    , quote = "_*“Miss Grangely! Report to Eliza’s office at once. Leave your things here. Yes, Everything.”*_ _\\*Crack of a ruler\\*_ _*- A Hawthorne Teacher.*_"
    , description = "Elizabell is a Hawthorne veteran at the top of her class and is her house’s assistant disciplinarian, that is among some to employ more... novel, methods of discipline and reward structures, She’s straight up a mistress dom and enjoys her job, fully dedicated to employing the methods of her house rules to get witches under her care to achieve their best She’s firm but very warm and comforting when appropriate."
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
        [ "+/- Isn’t a domme by heart, but a talented switch." ]
    , has = "Eliza has Wands 3, Occultism 3, Witchery 3, Consortation 4, Windkeeping 5, and _Energize_."
    }


ashleyLovenko : Details
ashleyLovenko =
    { name = AshleyLovenko
    , class = ClassOne Sorceress
    , races = [ Neutral ]
    , hasPerk = False
    , cost = Just 6
    , power = NormalScore 3
    , teamwork = NormalScore 7
    , sociability = NormalScore 9
    , morality = NormalScore 8
    , quote = "_*“Where’d Ash go? Oh there she is, taunting the pond fish with fish-sticks again.”*_ _\\*Splash\\*_ _*“There she goes.” - A Hawthorne Student.*_"
    , description = "Ashley has been moved between two house already. She isn’t a delinquent, but... she’s an eccentric airhead with a goldfish-like memory at least when it comes to rules or lessons. She’s very social and likable but often rubs people the wrong way at first with her enthusiasm and talent for messing things up. Her magic seems to have a mind of its own."
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
    , class = ClassOne Sorceress
    , races = [ Luxal ]
    , hasPerk = True
    , cost = Just 8
    , power = NormalScore 10
    , teamwork = NormalScore 6
    , sociability = NormalScore 7
    , morality = NormalScore 8
    , quote = "_*“Mae is so cool.. When I broke my leg on a mission she held me while a gold light slowly fixed me.” - A Hawthorne Student.*_"
    , description = "If Willpower is a shield, Determination is the sword, the active driving force to push forward. Mae is determined to become the best and never fail another again since she lost a student to a band of goblinoids who couldn’t be recovered for 4 days and was never the same. She’s strict and can come across as cold, but would rather die than fail a student again. Uses similar methods to Eliza but much less, ah, _intrusive_."
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


lauraDDevonshire : Details
lauraDDevonshire =
    { name = LauraDDevonshire
    , class = ClassOne Academic
    , races = [ Neutral ]
    , hasPerk = True
    , cost = Just 8
    , power = NormalScore 10
    , teamwork = NormalScore 7
    , sociability = NormalScore 8
    , morality = NormalScore 7
    , quote = "_*“You could cut the tension between Hannah and Laura with a knife and spread it on toast. I bet they would be friends if they’d just talk.”*_"
    , description = "Laura is the only witch born to an Alfheimr noble lineage known for their talented witches. She’s grown up with the pressure of her family name and a deep need to satisfy expectations. She has a natural desire to see others exceed as well, and may seem overly serious or sharp when dealing with others who aren’t living up to their potential, but she just wants them to succeed. Prickly exterior, soft interior."
    , positives =
        [ "+ While confident, she doesn’t boast or go out of her way to show off."
        ]
    , negatives =
        [ "- Under a lot of stress kept beneath the surface"
        , "- Won’t disobey family"
        ]
    , mixed = [ "+/- Spends a lot of her time in study or practice." ]
    , has = "Laura has every core magic at rank 2, Alchemy 3, Hexes 4, Naturalism 4, Wands 5, _Master Wand_"
    }


caroline : Details
caroline =
    { name = Caroline
    , class = ClassOne Warlock
    , races = [ Aurai, Mimi ]
    , hasPerk = False
    , cost = Just 5
    , power = NormalScore 6
    , teamwork = NormalScore 5
    , sociability = NormalScore 7
    , morality = NormalScore 8
    , quote = "_*“Carol? No, she just showed up one day, nobody has seen her parents. Staff has raised her for...20 or so years now.” - Noriko to Ashley*_"
    , description = "Carol is a dangerous combination of Aurai and a chatterbox halfling mimi. To keep her from withering people with her voice, she’s tasked with biting down on a various bits when not sure it’s safe, only around various witches or creatures that don’t age can she let loose and she’ll talk endlessly about all the different things she wanted to say during the day, even if only talking to herself as she drifts to sleep."
    , positives =
        [ "+ Heart melting pro"
        , "+ Does retain information, so she’s still intelligent"
        ]
    , negatives =
        [ "- She has not mentally aged for more than 2 decades."
        , "- Poor impulse control."
        ]
    , mixed = [ "+/- Her draining voice withers 10 years instead of 1." ]
    , has = "Carol has Hexes 2, Familiarity 2, Witchery 3, Windkeeping 3, _Third Eye, Longing Mirror, Silly Goose_"
    }


suzyTheMiasma : Details
suzyTheMiasma =
    { name = SuzyTheMiasma
    , class = ClassOne Academic
    , races = [ Neutral ]
    , hasPerk = False
    , cost = Just 10
    , power = NormalScore 5
    , teamwork = NormalScore 7
    , sociability = NormalScore 3
    , morality = NormalScore 4
    , quote = "_*“Suzy may not kill people these days but she sure doesn’t seem to have any reservations with using others for her experiments.” - A teacher*_"
    , description = "Suzy is a poison specialist alchemist that always has multiple projects ongoing. She’s very numb to traumas such that she can appear sociopathic but it’s not that she doesn’t feel as much as she never learned to care and her emotions have been burned from a very rough first century of life. Now she just focuses on her craft in Hawthorne."
    , positives =
        [ "+ Benefits from anything that would benefit a familiar"
        , "+ Extremely proficient"
        ]
    , negatives =
        [ "- Requires moral guidance but is open to learning"
        , "- Consent? What’s that?"
        ]
    , mixed = [ "+/- Single-minded in poisoncraft. Her poisons are 3x as effective, but other potions are 80% less potent." ]
    , has = "Suzy has Portals 2, Witchery 2, Wands 3, Hexes 3, Alchemy 5, _Poisoner, Witchflame, Alchemist Stash_"
    }


norikoDuNichols : Details
norikoDuNichols =
    { name = NorikoDuNichols
    , class = ClassOne Academic
    , races = [ Neutral ]
    , hasPerk = False
    , cost = Just 15
    , power = NormalScore 3
    , teamwork = NormalScore 7
    , sociability = NormalScore 5
    , morality = NormalScore 6
    , quote = "_*Noriko made lunch boxes for each of her house students on their field trip. All Eliza gave us was fishing rods, lame.” - Students*_"
    , description = "Like most Hawthorne staff, Noriko was once a student herself, a star of her class with something of a wild streak: which many would say is an understatement. Not many people know why, but she withdrew from the public eye and into herself and her studies, adopting a new appearance to turn to teaching under a new name. She’s a house lead of House Mooncrest where she councils with a gentle hand."
    , positives =
        [ "+ Patient and considerate of student’s individual needs & interests"
        ]
    , negatives =
        [ "- Emotionally distanced from everything, a bit of a shell"
        , "- Deep sense of unfulfillment"
        ]
    , mixed = [ "+/- Some people might have blackmail on her past." ]
    , has = "Nori has Consortation 2, Witchery 4, Divination 4, Wands 5, Hexes 5, _Improved Rod (8p in cantrips), Master Wand_"
    }


watchers : List Details
watchers =
    [ francisIsaacGiovanni, ifraAlZahra, sariahJSnow, claireBelmontegra, sylvarraAsDomonina, madellineLPeach, reinaAkatsuki, minnieAndrus ]


francisIsaacGiovanni : Details
francisIsaacGiovanni =
    { name = FrancisIsaacGiovanni
    , class = ClassOne Warlock
    , races = [ Neutral ]
    , hasPerk = False
    , cost = Just 6
    , power = NormalScore 6
    , teamwork = NormalScore 4
    , sociability = NormalScore 2
    , morality = NormalScore 7
    , quote = "_*“He gives me chills. He’s been a Watcher since the Crusades. Which he fought in. I heard he stormed Jerusalem in 1099” - A Watcher.*_"
    , description = "They aren’t wrong, Isaac is a Crusader that participated in most at least seven different crusades. He’s a Warlock with an angry soul that simmers in a fine boil for the rape and murder of his entire family while he was away on campaign, which is what caused his Witch Awakening wherin a Balor emerged out from his rage which was stopped by a Contemplar when it nearly killed him too. Catholic."
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
    , class = ClassOne Warlock
    , races = [ Changeling ]
    , hasPerk = False
    , cost = Just 4
    , power = NormalScore 3
    , teamwork = NormalScore 4
    , sociability = NormalScore 9
    , morality = NormalScore 7
    , quote = "_*“Hey Ifra, where’d these doujins come fro-”* \\*Poof\\* \\*Frog croaking\\* *“Oh hey Ifra, Where’d Kat go? Oh what’s this-”* \\*Poof\\* *-Last Tue*_"
    , description = "Ifra grew up Muslim and managed to live a full life until the old age of 83 when she finally had her awakening -- into a changeling finding herself young again. A little more than she’d have hoped but hey. She believes in Islam though she’s lived long enough to see a lot of changes, that she just adapts to whatever the local custom is, otherwise she’s somewhat casual, but makes sure to pray 5 times a day."
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
    , class = ClassOne Academic
    , races = [ Nymph ]
    , hasPerk = True
    , cost = Just 8
    , power = NormalScore 4
    , teamwork = NormalScore 9
    , sociability = NormalScore 7
    , morality = NormalScore 9
    , quote = "_*“Sariah? She wanders a lot, but when she stops by I can’t think of anyone more pleasant to be around.”- An ORC Agent.*_"
    , description = "Sariah has been a witch for a long time already, and used to have a human husband early on who passed away before she could do something about it. She takes comfort in her Mormon belief in eternal marriages, and lives a life of dedication to doing good and stay worthy to be with him again someday, chats once in a while via necromancy."
    , positives =
        [ "+ Very calm and optimistic"
        , "+ Very true to her faith"
        , "+/- Friendly with the ORC"
        ]
    , negatives =
        [ "- Doesn’t go out on Sundays, unless it’s important"
        , "- Even avoids caffeine entirely"
        ]
    , mixed = [ "+/- Wants a new husband, but always holds her, late husband as her true partner to be reunited eventual" ]
    , has = "Sariah has Necromancy 2, Witchery 3, Divination 4, Waterworking 5, Ministration 5, _Oracle_, _Mythril Armor_ & _Artifact Keeper_"
    }


claireBelmontegra : Details
claireBelmontegra =
    { name = ClaireBelMontegra
    , class = ClassOne Sorceress
    , races = [ Nymph ]
    , hasPerk = True
    , cost = Just 6
    , power = NormalScore 5
    , teamwork = NormalScore 8
    , sociability = NormalScore 10
    , morality = NormalScore 4
    , quote = "_*“I heard Claire competed against a whole team of Hespatian succubi for trade secrets in Russia.. and won.” - Watcher secretary gossip*_"
    , description = "Claire isn’t very religious but appreciates the power structures and views Watcher lore as a meta layer of hidden histories. She’s a full fledged active Watcher agent with eyes and ears everywhere, and greatly enjoys her work in corporate espionage and uses certain Daeva advantages to her fullest. She often quickly finds herself in the offices of some of the most powerful people in the world."
    , positives =
        [ "+ Heartachingly beautiful."
        , "+ Highly tech savvy."
        ]
    , negatives =
        [ "- Deception ≠ Stealth."
        , "- Workaholic"
        ]
    , mixed = [ "+/- Doesn’t really consider anyone a friend until they’ve shared a bed while on mission together more than once" ]
    , has = "Claire has Curses 2, Ministration 3, Wind 4, Witchery 4, Hexes 5 & _False Light_"
    }


sylvarraAsDomonina : Details
sylvarraAsDomonina =
    { name = SylvarraAsDomonina
    , class = ClassOne Sorceress
    , races = [ Elf ]
    , hasPerk = False
    , cost = Just 15
    , power = NormalScore 2
    , teamwork = NormalScore 7
    , sociability = NormalScore 6
    , morality = NormalScore 7
    , quote = "_*“Torture? Maybe, maybe not. You get a special treat, Sylvarra is coming here. Just. for. you. Enjoy <3” - Peach, to a hespatian rogue.*_"
    , description = "Syl is a high ranking Inquisitor that takes a less confrontational approach than dear Peach, but is in many ways the scarier of the two. Syl is an investigation specialist that hunts information and stalks suspects until she has a full picture and delivers a final, rarely changed, judgement or invades your mind for more details or confirmation."
    , positives =
        [ "+ Strong sense of justice with fair judgement."
        ]
    , negatives =
        [ "- Holds law as her highest value"
        , "+/- Definitely a dominatrix"
        ]
    , mixed = [ "+/- Doesn’t need to kill - Can make you forget about witches entirely, while no longer being one yourself." ]
    , has = "Syl has Ministration 3, Witchery 3, Hexes 4, Occultism 4, Covenants 4, Divination 4, Windkeeping 5, Psychotics 5, _Secret Elixir, Mana Core, Violet Lenses, Life Record, Master Key_"
    }


madellineLPeach : Details
madellineLPeach =
    { name = MadellineLPeach
    , class = ClassOne Warlock
    , races = [ Empusa ]
    , hasPerk = True
    , cost = Just 15
    , power = NormalScore 5
    , teamwork = NormalScore 5
    , sociability = NormalScore 4
    , morality = NormalScore 7
    , quote = "_*“Peach can be a little eccentric but she’s harmless if not attacked first or given orders otherwise. She should be in Math now.” - Sariah*_"
    , description = "One of the foremost Watcher Inquisitors, field agents that relentlessly root out evil. They overlook simple demonology, but wherever there is a demon or outsider running amok, or simply an enemy to the Watchers, they send in the Inquisition, and Madelline is one of their heavy hitters. She’s able to use Celestials with her _Necronomicon_."
    , positives =
        [ "+ Cute and wholesome when not on the job, unrecognizable."
        ]
    , negatives =
        [ "- Yandere a/f"
        , "- Still in HS. despite being 80"
        ]
    , mixed = [ "+/- Though lesbians are normal for witches, even among Watchers, Peach is pissed she can’t find a witch husband." ]
    , has = "Peach has Alchemy 2, Runes 2, Ministration 4, Familiarity 4, Necromancy 5, Metallurgy 5, Curses 5, _Blood Witch, Mana Core, Apex, Necronomicon, Pewter Crown, Ritual Inks_"
    }


reinaAkatsuki : Details
reinaAkatsuki =
    { name = ReinaAkatsuki
    , class = ClassOne Warlock
    , races = [ Neutral ]
    , hasPerk = False
    , cost = Just 12
    , power = NormalScore 8
    , teamwork = NormalScore 5
    , sociability = NormalScore 5
    , morality = NormalScore 7
    , quote = "_*“Lay a hand on her again & you’ll lose it. Reina has atoned for her past & now serves. Reflect on your actions.” - Sariah shielding Reina*_"
    , description = "A former member of a hespatian blood cult, it was her Family that would eventually become those that turned the Red Mother, which Reina was present for but would leave not long after. Reina was eventually converted and joined the Watchers by the efforts of _Sariah_ over many encounters over the course of a decade, and now lives a life of making up for her long and heinous past. She does her best."
    , positives =
        [ "+ While not a saint, she tries her best to be moral."
        , "+ Her resolve is as iron."
        ]
    , negatives =
        [ "- Doesn’t resist those who take issue with her past."
        , "- _Too_ self-sacrificing."
        ]
    , mixed = [ "+/- Would not hesitate to sacrifice herself for another" ]
    , has = "Reina has Ministration 2, Curses 3, Necromancy 4, Occultism 4, _Blood Witch, Soul Graft (Vampire), False Light_"
    }


minnieAndrus : Details
minnieAndrus =
    { name = MinnieAndrus
    , class = ClassOne Warlock
    , races = [ Daeva ]
    , hasPerk = True
    , cost = Just 8
    , power = NormalScore 10
    , teamwork = NormalScore 6
    , sociability = NormalScore 8
    , morality = NormalScore 10
    , quote = "_*“Chronography suggests she really is only 8, not under a hex, but spectroscopy suggests an old soul, but not possession? Hm.” - Researcher*_"
    , description = "Minnie is a brand new soul to this aether, she’s reincarnated from another world, a world without magic from an entirely different multiverse as part of a divine exchange. She has the favor of the Eloquincia, goddess of beauty and rebirth who checks in on her every now and then, and personally crafted this new body of hers. She died at age 32 in her last life when she shielded a student from a terrorist bombing."
    , positives =
        [ "+ Despite her apparent age, she is wise & maintained most her past life’s knowledge & skills"
        ]
    , negatives =
        [ "- Her new body does cause her problems & influences her personality somewhat"
        ]
    , mixed = [ "+/- A lot of aspects of reality differ from her old one." ]
    , has = "Minnie has Familiarity 1, Witchery 3, and Runes 4, _+ Pet Break (Phoenix + Luck Magic & Intelligence)_"
    }


hespatians : List Details
hespatians =
    [ lucilleMBright, kingDaemianKain, whisper, redMother, nova, scarlet, huntress, malice ]


lucilleMBright : Details
lucilleMBright =
    { name = LucilleMBright
    , class = ClassOne Academic
    , races = [ Lilin ]
    , hasPerk = False
    , cost = Just 6
    , power = NormalScore 5
    , teamwork = NormalScore 7
    , sociability = NormalScore 8
    , morality = NormalScore 3
    , quote = "_*“Be careful dealing with her, Luke... she caused the American Civil War with her games, playing both sides because she could” - A Watcher.*_"
    , description = "Lucille is an eternal rebel and unquenchable contrarian that strains against all forms of authority or boundary line. If you can take something, it’s yours, they failed to keep it. She admires spark, in any form, and likes to kindle them into embers, into infernos. A strong tree has to start out from a sprout after all. She’s an information and arms broker."
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
    , class = ClassOne Sorceress

    -- TODO: check this
    , races = [ Dravir Fire ]
    , hasPerk = True
    , cost = Just 12
    , power = NormalScore 10
    , teamwork = NormalScore 5
    , sociability = NormalScore 6
    , morality = NormalScore 2
    , quote = "_*“Son, it’s not every day a young man turns 18, time to start helping out with the family business. For now enjoy some catgirl slaves” - Kain*_"
    , description = "When most witches who know their history think of a male sorcerer, Kain likely crosses their mind as an antediluvian warlord that ruled over ancient Siberia as a witch-king with his striking crown-halo of flame and his drake mount familiar, his own skin hardened with scales in places, and his breath like a dragon’s own. Now? a single dad with 3 kids and a blue collar job... while head of 4 Hespatian families"
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
    , class = ClassOne Warlock
    , races = [ Neutral ]
    , hasPerk = False
    , cost = Just 10
    , power = NormalScore 9
    , teamwork = NormalScore 7
    , sociability = NormalScore 8
    , morality = NormalScore 1
    , quote = "_\\*Shadows slither of their own volition as you lay atop a mountain of bodies slick with blood, a red glow illuminating a woman\\* *- A Vision.*_"
    , description = "Whisper is a high ranking Wraith of Hespatia and a prolific serial killer and ritual coordinator leading-a particular network of hespatian families as a pleasure-death cult indulging in the most egregious sins more than can be spoken here that make the murder, rape, and necrophilia seem tame. Every passing month marks between 6 and 60+ new victims between her rituals and her assassinations"
    , positives =
        [ "+ Insane situational awareness."
        , "+ Actually very charming."
        , "+ She’s like, _really_ hot tho."
        ]
    , negatives =
        [ "- Highly manipulative"
        , "- Habit of vanishing,"
        , "- Might flay you if bored."
        ]
    , mixed = [ "+/- Wont trust you if you don’t join in her “parties”." ]
    , has = "Whisper has Witchery 3, Familiarity 5, Occultism 5, Psychotics 5, _Visceramancy 5, Toximancy, Blood Witch & Shroud_"
    }


redMother : Details
redMother =
    { name = RedMother
    , class = ClassOne Academic
    , races = [ Neutral ]
    , hasPerk = False
    , cost = Just 8
    , power = NormalScore 6
    , teamwork = NormalScore 7
    , sociability = NormalScore 7
    , morality = NormalScore 3
    , quote = "_*“M..Mother...”* \\*Suggestive breathing\\* *“May I?.”* \\*Deep rhythmic drums and chanting\\* *-Audio pulled from captive ORC Agent.*_"
    , description = "Was the student Mae lost. Goblin shamanism attempted to turn her into a broodmother and it left deep scars in her mind that have manifested themselves during recovery She runs a Hespatian family where she’s “Mother” to a cult of pleasure and blood, though with less murder than Whisper’s shtick and more emphasis on a creepy incestuous undertone and a religious charade involving sadism."
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
    , mixed = [ "+/- Won’t _kill_ children or parents, but might abduct" ]
    , has = "Mom has Occultism 4, Necromancy 5, Alchemy 5, Consortation 5, _Visceramancy 5, Blood Witch, Family Line, and Immortal Blood_"
    }


nova : Details
nova =
    { name = Nova
    , class = ClassOne Sorceress
    , races = [ Neutral ]
    , hasPerk = False
    , cost = Just 15
    , power = NormalScore 7
    , teamwork = NormalScore 5
    , sociability = NormalScore 5
    , morality = NormalScore 1
    , quote = "_*“What the HELL happened here? Wasn’t there a town here?” “...yes, The Hespatians sent Nova to erase evidence...” - An ORC Agent*_"
    , description = "An evil twin that absorbed life force from her twin sister in the womb and throughout childhood. She feels a compulsive need to inflict destruction even harder than a succubus is compelled to do, well, other things. Like a constant itch, she yearns for causing mayhem combining her Wind and Fire magics to focus on Combustion magics. She’s only restrained by a control collar placed by Lucille that mellows her out."
    , positives =
        [ "+ Extremely adept at wide-area type attacks."
        , "+ Abnormally high mana (3x)"
        ]
    , negatives =
        [ "- Can be seen squirming with a need to destroy."
        , "- Addictive personality dis"
        ]
    , mixed = [ "+/- Do NOT take off her collar or let anyone touch it" ]
    , has = "Nova has Firecalling 5, Windkeeping 5, Curses 5, _Charge\u{00A0}Swap+ (Draviri), Mana Core, Magical Heart (20p worth)_"
    }


scarlet : Details
scarlet =
    { name = Scarlet
    , class = ClassOne Sorceress
    , races = [ Neutral ]
    , hasPerk = False
    , cost = Just 12
    , power = NormalScore 3
    , teamwork = NormalScore 8
    , sociability = NormalScore 9
    , morality = NormalScore 8
    , quote = "_*“A valued cultist of the Dragon, but she saved my little sister’s life from the razing of Port Charlotte. That she caused.” - Alliance Merchant*_"
    , description = "Scarlet was once a simple village girl known for her red hood, until a werewolf stalked her and murdered her grandmother. In the terror of the situation, her awakening triggered naturally as a Dravir, ared dragon. She ate the wolf the way it ate her only relative but it left a human body Scarlet would go on a downward spiral and end up in Hespatia under King Damien Kain’s favored family."
    , positives =
        [ "+ Cheerful and upbeat"
        , "+ Secretly very analytical."
        ]
    , negatives =
        [ "- Impressionable and easily convinced to do things."
        ]
    , mixed = [ "+/- A little duplicitous, she’ll do what’s required of her but undermines evil actions subtly within bounds of orders" ]
    , has = "Scarlet has Witchery 3, Runes 4, Portals 4, Firecalling 5, Metamorphize (Red Dragon), _Windsong, Blood Witch_"
    }


huntress : Details
huntress =
    { name = Huntress
    , class = ClassOne Warlock
    , races = [ Neutral ]
    , hasPerk = False
    , cost = Just 12
    , power = NormalScore 6
    , teamwork = NormalScore 5
    , sociability = NormalScore 7
    , morality = NormalScore 8
    , quote = "_*“Lucille’s pet heroine is crying when she thinks nobody is watching. Again. Is there a reason we haven’t erased her mind yet?” - Whisper*_"
    , description = "Huntress is another Hespatian specialist known as a dead shot marksman, delivering death before anyone even knows there is a threat, her Owl Familiarity granting her a keen eye and silent movement, all the more deadly with her powerful Archer’s Bow punching through walls to strike targets she sees through walls with her Violent Lenses, all from over a mile away. She also serves to cull misbehaving Families."
    , positives =
        [ "+ Never _misses_ her mark"
        , "+ Calculating with a very quick thinking sharp mind"
        ]
    , negatives =
        [ "- Seems to be outright unable to refuse orders delivered from Crowns, or Lucille."
        ]
    , mixed = [ "+/- She has strong morals, strangely unsuited to her job." ]
    , has = "Huntress has Runes 3, Witchery 4, Occultism 4, Familiarity 5, _Windsong, Archer’s Bow, Violent Lenses, Shroud_"
    }


malice : Details
malice =
    { name = Malice
    , class = ClassOne Sorceress
    , races = [ Empusa ]
    , hasPerk = True
    , cost = Just 15
    , power = NormalScore 10
    , teamwork = NormalScore 9
    , sociability = NormalScore 5
    , morality = NormalScore 4
    , quote = "_*“Malice of Hespatia, graceful and elegant, but fights with a berserker fury if her “family” is under any threat. Any.” - an ORC Agent.*_"
    , description = "“Malice” is a high ranking Hespatian wraith,though not a head of a family of her own, she bounces between families considering herself a part of a larger family in the traditional sense, being close with Whisper, Mother, and King for example. She has nothing but disdain for the overwhelming majority of non-Hespatian life forms"
    , positives =
        [ "+ Will do literally anything in her power for “Family”."
        , "+ Extreme durability."
        ]
    , negatives =
        [ "- Can be intrusively clingy with those she likes / family."
        , "- No morals vs “Threats”."
        ]
    , mixed = [ "+/- Strictly black and white in her worldview: You’re either family, or a threat. Anything against a threat is excused." ]
    , has = "Malice has Psychotics 2, Monstrosity 3, Hexes 4, Necromancy 5, _Guardian’s Wall & Maid Hand_"
    }


lunabellans : List Details
lunabellans =
    [ diana, cassandra, kingCulicarius, einodiaKate, persephone, betildaAraiBuckland, nichteYIr, aelfflaedNowDaphne ]


diana : Details
diana =
    { name = Diana
    , class = ClassOne Sorceress
    , races = [ Neutral ]
    , hasPerk = False
    , cost = Just 6
    , power = NormalScore 8
    , teamwork = NormalScore 9
    , sociability = NormalScore 7
    , morality = NormalScore 9
    , quote = "_*“Around here when you hear the wolves howl to the moon, you can feel safer, knowing the huntresses are here.” - ORC agent on a certain grove.*_"
    , description = "Diana isn’t actually a changeling despite her youthful appearance, but settle into maintaining her looks a certain way a long time ago to be with her late changeling husband who she hasn’t seen in over a thousand years. She doesn’t know what happened to him, but has kept her vows ever since, living as an eternal maiden huntress with her wolf, and started a sorority of celebate huntresses to join her."
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
    , has = "Diana has Ministration 2, Familiarity 4 (Owl), Witchery 5, _Improved Familiar (Dire Wolf) & Memorize_"
    }


cassandra : Details
cassandra =
    { name = Cassandra
    , class = ClassOne Sorceress

    -- TODO: check this
    , races = [ Dravir Fire ]
    , hasPerk = False
    , cost = Just 6
    , power = NormalScore 3
    , teamwork = NormalScore 6
    , sociability = NormalScore 5
    , morality = NormalScore 7
    , quote = "_*“I love visiting Cass but I don’t get why she’s so reclusive. She’s obviously not antisocial, I just don’t get it...” - Lunabellan.*_"
    , description = "Used to be a renown oracle and her spat with Culicarius stuck in the rumormill long enough to become fable, back in his asshole phase in Greece, though she wasn’t a saint either. They both got better. She manages her own domain bubble where she emphasizes large tall mountains and cliffs topped with estates where she practices ritual magics."
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
    , class = ClassOne Academic
    , races = [ Daeva ]
    , hasPerk = True
    , cost = Just 15
    , power = NormalScore 4
    , teamwork = NormalScore 9
    , sociability = NormalScore 8
    , morality = NormalScore 8
    , quote = "_*“I want to see Culicarius.. I want to know if I have a future with Mariah but I wonder what else to ask him.” - Random Lunabellans*_"
    , description = "The King of Lunabella and an ancient male Daeva that inspired the myth of Apollo, He can be in a number of places at once and has seen so much, not just in his life but in exploring futures. He’s fair but firm. Harem of 1,013, wives & some husbands because he rarely turns them down."
    , positives =
        [ "+ Literally the body of a greek god."
        , "+ There’s a lot of him."
        ]
    , negatives =
        [ "- If you abuse your relationship with him, you’ll lose it"
        , "- Very jaded, though whimsical"
        ]
    , mixed = [ "+/- You can’t convince him to change Lunabella, for better or worse, without serious evidence that it isn’t working: and, it is, and he’s seen a thousand alternatives to their ends." ]
    , has = "He has Ministration 3, Potions 4, Curses 5, Consortation 5, Firecalling 5, Divination 5, Oracle, Synthetic Hive, Embody Time"
    }


einodiaKate : Details
einodiaKate =
    { name = EinodiaKate
    , class = ClassOne Academic
    , races = [ Daeva ]
    , hasPerk = False
    , cost = Just 15
    , power = NormalScore 1
    , teamwork = NormalScore 4
    , sociability = NormalScore 5
    , morality = NormalScore 6
    , quote = "_*“She’s too talented for her own good, straining against the limits of the masquerade...and higher powers.” - Culicarius to a friend.*_"
    , description = "Also goes by Kate, Kate is the lich founder of Lunabella, but she has no interest in management, she just opened the way and chose her brother Culicarius to rule in her stead. She’s a legendary witch that got entangled in the myth of _Hekate_, to the extent she began the process of mantling, becoming one, but was pulled out of it by her brother. She’s a traveler at heart that’s always yearning for more knowledge."
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


persephone : Details
persephone =
    { name = Persephone
    , class = ClassOne Academic
    , races = [ Daeva ]
    , hasPerk = True
    , cost = Just 12
    , power = NormalScore 4
    , teamwork = NormalScore 6
    , sociability = NormalScore 9
    , morality = NormalScore 8
    , quote = "_*“You should know there’s a good chance she’s watching us right now, and she’s the one who recommended I set up shop here...” - Penelope.*_"
    , description = "Persephone is the younger sister of Penelope, though a few years are nothing compared to their long lives, they’re more like twins. If you chose Lunabella it’s likely Pen called in her as her contact to show you around and from there you decide what to do or to stick with her or not, she’s supportive & even doting if you’re fun."
    , positives =
        [ "+ Easy connection to make"
        , "+ Incredible insight."
        ]
    , negatives =
        [ "- Has already stalked you"
        ]
    , mixed =
        [ "+/-Ara Ara"
        , "+/- Maaay want your babies (F/F is no issue for witches), she’s big into genetics & experiments in lineage for witch offspring."
        ]
    , has = "Seph has Hexes 3, Witchery 3, Potions 4, Aethernautics 4, Familiarity 4 (Cow), Domain 5, Portals 5, Divination 5, _Cornucopia, Life Record, Companion Brick (You)_"
    }


betildaAraiBuckland : Details
betildaAraiBuckland =
    { name = BetildaAraiBuckland
    , class = ClassOne Academic
    , races = [ Doll ]
    , hasPerk = False
    , cost = Just 10
    , power = NormalScore 5
    , teamwork = NormalScore 2
    , sociability = NormalScore 5
    , morality = NormalScore 4
    , quote = "_*“You’re in charge of taking care of this library, and Miss Kate can be our mistress, we have your back, just ask any time,” - Culicarius*_"
    , description = "Betty is a doll that at first glance might be mistaken for a dwarf or changeling, without visual cues to her true type save for easily missed slight lines on her shoulders and hips. Prior to this she was a human girl, taken off the street. Betty has issues where she’s uncomfortable functioning independently, so she began to flourish under the hierarchy system of Lunabella. She spends a lot of time in libraries."
    , positives =
        [ "+ Encyclopedic knowledge. of witch history, alchemy runes, gods, and monsters"
        ]
    , negatives =
        [ "- Has a habit of sticking “I suppose” into sentences."
        , "- Jaded and slow to care"
        ]
    , mixed = [ "+/- If she says she’ll do something, she’d rather die than fail." ]
    , has = "Betty has Necromancy 2, Familiarity 4, Runes 5, Alchemy 5, Domain 5, _Levitation, Alchemist Stash & Stone_"
    }


nichteYIr : Details
nichteYIr =
    { name = NichteYIr
    , class = ClassOne Warlock
    , races = [ Neutral ]
    , hasPerk = False
    , cost = Just 10
    , power = NormalScore 8
    , teamwork = NormalScore 4
    , sociability = NormalScore 3
    , morality = NormalScore 3
    , quote = "_*“Nichte scares the children, well, everybody, but she works hard to go out of her way help anyone as best she can.” - an ORC Agent.*_"
    , description = "Nichte was alifelong slave in an old Hespatian family that likes to play with fire. She was used as a sacrificial test for controlling Outsider corruption. It didn’t go as planned but they still gained a potent slave, until a Lunabellan raid stormed the compound lead by a Culicarius, who subdued and offered her a choice, and control over herself."
    , positives =
        [ "+ Dependable and trustworthy"
        , "+ Contrary to her appearance, she is warm and comforting."
        ]
    , negatives =
        [ "- She’s still corrupted, so any harm she causes still contributes to Far entropy"
        ]
    , mixed = [ "+/- Has a voluntary slave crest that acts as an anchor to avoid losing herself to Outsiders by binding to a master. " ]
    , has = "Nichte has Psychotics 3, Firecalling 3, Aethernautics 3, Domain 4, Necromancy 5, Monstrosity 5, _Memorize, Sun Shard, Cornucopia_"
    }


aelfflaedNowDaphne : Details
aelfflaedNowDaphne =
    { name = AelfflaedNowDaphne
    , class = ClassOne Warlock
    , races = [ Neutral ]
    , hasPerk = False
    , cost = Just 15
    , power = NormalScore 7
    , teamwork = NormalScore 6
    , sociability = NormalScore 7
    , morality = NormalScore 5
    , quote = "_*“She’s on the moon under Culicarius’s care now, she was quite a threat for decades, being bound to some “seed bearer”” - An ORC Agent.*_"
    , description = "Daphne was at one point a teacher at Hawthorne over a thousand years ago, and head of a particularly troublesome house, but she encountered the Pumpkin Moon and disappeared. She’s now returned to a very different world Feeling out of place, she found her way to Lunabella where Culicarius, one of a handful of familiar faces, offered her residence to teach magic on the moon and learn new norms."
    , positives =
        [ "+ 1,000/y of stories of other worlds to share."
        ]
    , negatives =
        [ "- Slow and impatient with technology & modernity "
        ]
    , mixed = [ "+/- Daph has 6 _Pumpkin Boons_: choose 6 _personal perks_ & tweak to something similar and roughly as effective" ]
    , has = "Daph has Witchery 3, Necromancy 3, Domain 4, Familiarity 4, Wands 5, Fire 5+Nature 5 = _Jack’o Lantern magic_"
    }


theOrcs : List Details
theOrcs =
    [ victoriaWatts, richardMaxJohnson, bethadonnaRossbaum, mirandaQuincy, meganMinosine, janeKitAdams, aliciaRedVelvetine, juliaMayCaldwin ]


victoriaWatts : Details
victoriaWatts =
    { name = VictoriaWatts
    , class = ClassNone
    , races = [ Neutral ]
    , hasPerk = False
    , cost = Just 8
    , power = NormalScore 0
    , teamwork = NormalScore 10
    , sociability = NormalScore 8
    , morality = NormalScore 7
    , quote = "_*“TYPE RED TYPE RED, OH SHIT, oh-”* *static* *“Fall back to site\u{00A0}B6. V on route to intercept.” - ORC Radio chatter.*_"
    , description = "V is not a witch, but is nonetheless a top ranking ORC Agent and a master of her job. She’s 87 years old, 88 in a few months, but potions don’t benefit only witches and she’s been in peak prime for the last 6 and a half decades, committed to either dying on the job or for the universe to tell her when it’s time to settle down, but no end in sight."
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
    , class = ClassNone
    , races = [ Neutral ]
    , hasPerk = False
    , cost = Just 5
    , power = NormalScore 0
    , teamwork = NormalScore 7
    , sociability = NormalScore 5
    , morality = NormalScore 6
    , quote = "_*“He came back from a mission one time with a chainsaw tied to the stump of a missing hand, and a shotgun pegleg” - Doc Ginger.*_"
    , description = "The quintessential male action hero made flesh, Max is the love child between Indiana Jones, Clint Eastwood, and MacGyver with a splash of Geralt of Rivia. He gets the job done one way or another and doesn’t shy away from risk or pain, Doesn’t drink because he hates anything that dulls his perception, and alcoholism cost him is first marriage, Avoids showing emotion in front of anyone, including anger (within reason, outside of combat), Has many safehouses memorized."
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
    , class = ClassOne Warlock
    , races = [ Lilin ]
    , hasPerk = True
    , cost = Just 12
    , power = NormalScore 1
    , teamwork = NormalScore 8
    , sociability = NormalScore 9
    , morality = NormalScore 5
    , quote = "_*“I... I heard Bethadonna was a cultist for a long time before joining up... The things she must have seen...and done..” - An ORC Agent.*_"
    , description = "Bethadonna is a domestic goddess that radiates warmth and a motherly presence in contrast to the devilish horns you might miss at a glance from the front, nevermind if your eyes didn’t have somewhere else drawing their attention. She acts as a detective utilizing spirits and communion to work scenes, while being a heavy hitter for when SHTF. She cultivates a private pond of Golden Fish for her rituals."
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
    , class = ClassOne Academic
    , races = [ Erinyes ]
    , hasPerk = True
    , cost = Just 8
    , power = NormalScore 8
    , teamwork = NormalScore 6
    , sociability = NormalScore 7
    , morality = NormalScore 6
    , quote = "_*“Why have doctors when healing potions are a thing?”* \\*bonk\\* *“Nitwit, potions are expensive, for emergencies.” - An ORC Agent.*_"
    , description = "Miranda, also called Gingerdoc or Doc Ginger, is a medical and general life-saving specialist responsible for keeping people fit for duty and enables risk taking in the face of overwhelming odds with her mastery of anatomy and what keeps a body ticking. That, and magic potions, of course She spends a lot of time micromanaging resources to keep everyone in shape and supplied so there’s no excuse for a loss."
    , positives =
        [ "+ Legitimate doctor for real"
        , "+ Values her word / promises"
        ]
    , negatives =
        [ "- Somewhat cold and sharp."
        , "- Brutally honest"
        ]
    , mixed = [ "+/- Doesn’t believe in mercy, for enemies, or for allies; Will save your life no matter the pain, to eventually recover" ]
    , has = "Doc has Gadgetry 2, Hexes 3, Witchery 4, Firecalling 4, Potions 5, and Familiar 5 (Fox), _ORC License_"
    }


meganMinosine : Details
meganMinosine =
    { name = MeganMinosine
    , class = ClassOne Warlock
    , races = [ Neutral ]
    , hasPerk = False
    , cost = Just 8
    , power = NormalScore 6
    , teamwork = NormalScore 7
    , sociability = NormalScore 5
    , morality = NormalScore 6
    , quote = "_*“This tiny thing leveled the Zonguie forest?.” “I said it was an accident! There were zombies! I was scared!” - ORC Agents & Meg.*_"
    , description = "The good twin counterpart to Nova. Her growth was stunted by her sister. She has all the qualities her evil sister lacks, but not exaggerated in the same way the negative qualities were exaggerated. She’s a little timid and a little reluctant to cause collateral damage, but from the link with her evil win she developed the same magic affinities."
    , positives =
        [ "+ Friendly and mindful"
        , "+ Will bake cookies."
        , "+ Patient with people"
        ]
    , negatives =
        [ "- Conflict avoidance"
        , "- Impatient with herself."
        ]
    , mixed =
        [ "+/-Smol"
        , "+/- She can use her magics almost exclusively for explosions, which are 3x more potent & large than usual."
        ]
    , has = "Meg has Firecalling 5, Windkeeping 5, Curses 5, _Charge Swap+ (Draviri), Mana Core, Magical Heart (20p worth)_"
    }


janeKitAdams : Details
janeKitAdams =
    { name = JaneKitAdams
    , class = ClassOne Sorceress
    , races = [ Mimi ]
    , hasPerk = False
    , cost = Just 8
    , power = NormalScore 6
    , teamwork = NormalScore 5
    , sociability = NormalScore 7
    , morality = NormalScore 8
    , quote = "_*“Look, I get she’s cute and all, and recovered, but should we really give her back that super scary dagger? I mean.” - An ORC Agent.*_"
    , description = "Kit is a halfling mimi under an immortality curse since she was young, She’s around 30 years older than she looks, and trained since birth to be a killer by a Hespatian family that specialized in training covert assassin-bodyguards. The branch she was in was raided by the ORC and she was among 8 to survive, and 3 years of psychosurgery later, recovered or reconditioned enough to be sociable and live free."
    , positives =
        [ "+ Absolute loyalty to someone she dedicates herself to by choice"
        ]
    , negatives =
        [ "- Instincts run deep, she still has violent reactions to threats."
        , "- Can’t bring herself to ask for help"
        ]
    , mixed = [ "+/- Trained more in martial and skill, than in magic use." ]
    , has = "Kit has Witchery 2, Psychotics 2, Familiarity 3, Portals 4, _Master Key, Assassin’s Edge, Memorize_"
    }


aliciaRedVelvetine : Details
aliciaRedVelvetine =
    { name = AliciaRedVelvetine
    , class = ClassOne Sorceress
    , races = [ Neutral ]
    , hasPerk = False
    , cost = Just 12
    , power = NormalScore 7
    , teamwork = NormalScore 6
    , sociability = NormalScore 8
    , morality = NormalScore 8
    , quote = "_*Red, I know you already undid your chain, let’s get out already.”* \\*huff\\* *Don’t ruin my fun, they were getting the whip” - Red & May*_"
    , description = "Red grew up high on romance adventure thrillers, with dreams of being swept off her feet. When she Awoke as a witch, she took it in stride. Realizing her affinities, she molded herself a dashing heroine and learned to do the sweeping herself. She wants a man that can outpace her but has yet to find anyone that can while satisfying her ideals, but enjoys herself in the meantime by teasing damsels."
    , positives =
        [ "+ Goes out of her way for others and acts on her values of honor & service."
        ]
    , negatives =
        [ "- Sometimes seems to get herself situations where she can play the damsel, for fun"
        ]
    , mixed = [ "+/- Very into the theatrics and roleplaying scenarios." ]
    , has = "Red has Necromancy 2, Portals 3, Witchery 4, Metallurgy 5, _Memorize, Gunwitch, Hot Swap, Mana Core, Apex_"
    }


juliaMayCaldwin : Details
juliaMayCaldwin =
    { name = JuliaMayCaldwin
    , class = ClassNone
    , races = [ Neutral ]
    , hasPerk = False
    , cost = Just 12
    , power = NormalScore 0
    , teamwork = NormalScore 10
    , sociability = NormalScore 8
    , morality = NormalScore 10
    , quote = "_*“Damn tricky human and her damn toys. After training under Lalahon she’s become an even greater threat” - A fleeing Hespatian.*_"
    , description = "May is a Division Leader of the ORC that was one of the first children born within the ORC itself, growing up in the human pocket world of Deco City run by the international ORC. She loves her job as division lead for the USA branch expeditionary force. While not a witch she makes very good use of the artifacts and tricks she’s picked up on the job."
    , positives =
        [ "+ Always pushes her limits"
        , "+ Great at bringing out the full potential of her allies."
        ]
    , negatives =
        [ "- Can never feel settled when someone is in danger"
        , "- Which is all the time."
        ]
    , mixed = [ "+/- May pushes the limits on what it means to be human in a world of magic and gods, she’s more human than human" ]
    , has = "May has _Soul Warrior (Scepter), Soul Graft (Fountain, Stay Human), Magic Talisman (Portals), Collection (Runes)_"
    }


alphazonians : List Details
alphazonians =
    [ samanthaNatPonds, jenniferKYoung, agent7Y, agent9s, twinsSaraAndKara, vesuvianelleLalahon, amberOgdenVix, xINDollmaker ]


samanthaNatPonds : Details
samanthaNatPonds =
    { name = SamanthaNatPonds
    , class = ClassNone
    , races = [ Neutral ]
    , hasPerk = False
    , cost = Just 8
    , power = NormalScore 0
    , teamwork = NormalScore 6
    , sociability = NormalScore 10
    , morality = NormalScore 5
    , quote = "_*“Seventh quarterly is in, 12% average across the board. Asset 4z13-Samantha Ponds, continues to excel above expectations” - An Exec.*_"
    , description = "Sam is a high level Alphazon agent known for cooperation with the ORC. She has a preferred appearance as shown, but makes full use of her skinsuit and full body replacements and remote operations to make more use of changing forms, and whole bodies, more than even most expert Hex witches She takes it a step further by having trained her personality to become highly adaptive and instinctively suit any given situation. She doesn’t really know who she is anymore."
    , positives =
        [ "+ Very low maintenance."
        , "+ Incredibly adaptive."
        ]
    , negatives =
        [ "- Literal maintenance; Synth"
        , "- No self-drive, craves purpose."
        ]
    , mixed = [ "+/- Not naive, but doesn’t ask questions that aren’t directly relevant to a given task" ]
    , has = "Sam has Divination 3, Gadgetry 4, Integration 5"
    }


jenniferKYoung : Details
jenniferKYoung =
    { name = JenniferKYoung
    , class = ClassNone
    , races = [ Neutral ]
    , hasPerk = False
    , cost = Just 5
    , power = NormalScore 0
    , teamwork = NormalScore 4
    , sociability = NormalScore 8
    , morality = NormalScore 4
    , quote = "_*“Lucky bastard in O5 was assigned to monitor Asset f9E21’s observer, and cohost.”* \\*Sigh\\* *“Could have been me...” - an Operator.*_"
    , description = "Jen... is not a typical agent or employee of Alphazon. She’s a daughter of a high level executive that grew up never knowing the feeling of being unable to get something she wanted and was a designer baby derived from daeva genetics. This didn’t make her a witch, but the results do speak for themselves. She’s somewhat naive and spoiled, but in a minimally toxic way, understanding her position and being   grateful for it, though actively indulges in vices."
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
    , class = ClassNone
    , races = [ Neutral ]
    , hasPerk = False
    , cost = Just 12
    , power = NormalScore 0
    , teamwork = NormalScore 7
    , sociability = NormalScore 7
    , morality = NormalScore 1
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
    , has = "Sev has Integration 5 (many reserve bodies), DIvination 5 & _Gold Card_, (+2 Spysats, with observers)"
    }


agent9s : Details
agent9s =
    { name = Agent9s
    , class = ClassOne Warlock
    , races = [ Aurai ]
    , hasPerk = False
    , cost = Just 10
    , power = NormalScore 8
    , teamwork = NormalScore 4
    , sociability = NormalScore 6
    , morality = NormalScore 2
    , quote = "_*“I reviewed the Observer case logs of A-98. Twelve vampire ninjas. Twelve. With a hairpin and two missing limbs.” - An Operator.*_"
    , description = "“Nines” was a common human employee before awakening and finding a niche with potions and ritualcasting for some time before coming into her greater potential; As a corporate hitman with her spider familiar. She has a legitimate business front as a dayjob with Alphazon that sees her traveling a lot “for business”, does love to relax though."
    , positives =
        [ "+ Remarkably chill and homey in private"
        , "+ As tenacious as Max"
        ]
    , negatives =
        [ "- May “disappear” people that annoyed her"
        , "- Used to working alone."
        ]
    , mixed = [ "+/- A semi-lethal rivalry with the ORC, Richard Max in particular, a little Mr. and Ms. Smith style" ]
    , has = "Nines has Integration 2, Witchery 3, Potions 3, Occultism 4, Psychotics 4, Familiarity 5 & _Gold Card_"
    }


twinsSaraAndKara : Details
twinsSaraAndKara =
    { name = TwinsSaraKara
    , class = ClassOne Sorceress
    , races = [ Dwarf ]
    , hasPerk = True
    , cost = Just 8
    , power = NormalScore 10
    , teamwork = NormalScore 10
    , sociability = NormalScore 3
    , morality = NormalScore 4
    , quote = "_*“Underestimate them and you’re dead. They look young but they’re dwarves, and have experience worth triple their age.” - An Agent.*_"
    , description = "The Sara and Kara are dwarven twins who have been together through unbearable circumstance ever since they were little, growing closer from their life challenges and rely on one another as the only ones they currently trust. They play off each other’s strengths and weaknesses with perfect synchronicity not as mirrors, but as a balanced harmony, extending to their personality traits and magic."
    , positives =
        [ "+ Kara is highly observant."
        , "+ Sara is highly intuitive"
        ]
    , negatives =
        [ "- Kara is always on edge."
        , "- Sara can’t let questions go."
        ]
    , mixed = [ "+/- Two for the price of one? They’re an inseparable pair," ]
    , has = "Twins both have _Magic Friendship (twin)_ & Witchery 4; Sara\u{00A0}has\u{00A0}Ministration\u{00A0}4, Domain 4, Covenants 4, Divination 5; Kara\u{00A0}has\u{00A0}Consortation\u{00A0}4, Portals 4, Occultism 4, Aethernautics\u{00A0}5"
    }


vesuvianelleLalahon : Details
vesuvianelleLalahon =
    { name = VesuvianelleLalahon
    , class = ClassOne Sorceress
    , races = [ Mimi ]
    , hasPerk = True
    , cost = Just 10
    , power = NormalScore 8
    , teamwork = NormalScore 6
    , sociability = NormalScore 7
    , morality = NormalScore 6
    , quote = "_*“Vesuviannelle was one of the goddesses to resist the masquerade and was struck down from her realm by the victors.” - An ORC scholar*_"
    , description = "Suvi is an Amazonian mimi (Rabbit) well known in some parts for her extensive use of volcanic magics utilizing Earth-Fire elementalism. There’s a myth that she created an island chain by hopping across the sea with volcanos erupting from her footsteps. Fact or fiction? She just laughs it off. She makes appearances when her islands are threatened by major events, otherwise being elusive."
    , positives =
        [ "+ Protective of innocents."
        , "+ One of the strongest martial witches out there"
        ]
    , negatives =
        [ "- Harsh towards offense"
        , "- As a fallen goddess, she has powerful enemies"
        ]
    , mixed = [ "+/- Used to be worshipped, some sects remain" ]
    , has = "Suvi has Potions 3, Runes 4, Familiarity 5, Earthmoving 5, Firecalling 5, _Soul Warrior (Gladius), Soul Graft (Jackalope)_"
    }


amberOgdenVix : Details
amberOgdenVix =
    { name = AmberOgdenVix
    , class = ClassOne Warlock
    , races = [ Empusa ]
    , hasPerk = False
    , cost = Just 12
    , power = NormalScore 7
    , teamwork = NormalScore 4
    , sociability = NormalScore 6
    , morality = NormalScore 3
    , quote = "_*“May be the best driver in known witchdom, you’ll be in good hands. Nines will cover you on your way out, 3...2..1--” - An Alphazon Op.*_"
    , description = "Vix was always a speed demon, a wild child that started racing tricycles at six and couldn’t get enough, becoming a street racer by her late teens. On her 21st birthday she met with a strange crew that dripped with mystery and she had to dig deeper. The ringleader saw something in her and pushed her unreasonably hard, until it paid off, and she had a witch awakening, & stepped into a whole new world."
    , positives =
        [ "+ Extreme determination"
        , "+ Laser focus on problems."
        , "+ Potion energy drinks"
        ]
    , negatives =
        [ "- Sees “No” as a challenge"
        , "- Adrenaline junkie."
        , "- Can’t sit still"
        ]
    , mixed = [ "+/- Pro use of alchemy and runes to enhance rides" ]
    , has = "Vix has Consortation 1, Necromancy 2, Wands 3, Runes 3, Metallurgy 5, Alchemy 5, Witchery 5, _Hellrider, Broom Beast 6, Witch Pistol_"
    }


xINDollmaker : Details
xINDollmaker =
    { name = XINDollmaker
    , class = ClassOne Academic
    , races = [ Taura ]
    , hasPerk = False
    , cost = Just 12
    , power = NormalScore 6
    , teamwork = NormalScore 5
    , sociability = NormalScore 7
    , morality = NormalScore 3
    , quote = "_*“X.I.N.? She gives me chills, Admin gives her way too much freedom, she gets away with anything... Did you hear something?” ~ Recording*_"
    , description = "XIN, Dollmaker was the original lead researcher Alphazon employed to study the research and development of new forms of dolls, based on Xin’s own past experience with as one of the first witches to dabble in the art of dollmaking. Allegedly she turned her own daughter into a doll, the only name on file was _Chen_, who, or it’s, since been lost. Arachne (Spider Taura), though usually keeps shifted to human legs."
    , positives =
        [ "+ Can raise the dead as dolls"
        , "+ Turns humans into witches"
        , "+ Knows unspeakable secrets"
        ]
    , negatives =
        [ "- Ruthless pursuit of goals she won’t or can’t speak of"
        ]
    , mixed = [ "+/- Her body is a combination of flesh and ball-joint doll" ]
    , has = "Xin has Runes 3, Hexes 4, Necromancy 5, Windkeeping 5, Arachnescence 5, _Jester Mask, Ritual Inks, Dollmaker Kit, Servant Dolls_"
    }


independents : List Details
independents =
    [ alexKHalls, isabellaMableOaks, evangelinaRosaCostaval, penelope, mandyHunts, eskhanderMahabadi, experiment627, augustRoseOBare, opheliaReisha, estherReisha, custom, erisJulianariStonefallen, xiaoLiena, jin, saraStar, redBetty ]


alexKHalls : Details
alexKHalls =
    { name = AlexKHalls
    , class = ClassOne Sorceress
    , races = [ Neutral ]
    , hasPerk = False
    , cost = Just 2
    , power = NormalScore 10
    , teamwork = NormalScore 7
    , sociability = NormalScore 5
    , morality = NormalScore 8
    , quote = "_*“I sensed this one awaken not long ago. Poor thing was lost and confused while her human family didn’t notice a change.” - Penelope.*_"
    , description = "Alex is a brand new witch that just awakened a few days ago... and is still a little slow to adjust to losing her... trunk, having been a man this time last Tuesday. She’s an adorable fish out of water that doesn’t know much of anything, she visits Penelope’s shop when she has the free time to learn while trying to keep her previous life going as best she can, most humans adjust as though she always was a woman, but it’s more complicated than that. Wanna help her out?"
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
    , has = "Alex has Familiarity 1 (Cat) & _Jack-of-All_"
    }


isabellaMableOaks : Details
isabellaMableOaks =
    { name = IsabellaMableOaks
    , class = ClassAny
    , races = []
    , hasPerk = False
    , cost = Just 15
    , power = SpecialEffect { better = 10, worse = Nothing }
    , teamwork = NormalScore 6
    , sociability = NormalScore 9
    , morality = NormalScore 8
    , quote = "_*“I detected this one earlier before you showed up. She hasn’t awakened yet, but should naturally within the week.” - Penelope.*_"
    , description = "Isabella is for now a human going about her life attending her first year of college while working part-time at a convenience store, spending most her free time volunteering for events and charities, or service projects. When she’s not at class, work, or volunteering, she’s cooking or cleaning. if not for herself then for her wheelchair-bound mom, missing both legs from an accident."
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
    , class = ClassAny
    , races = [ Neutral ]
    , hasPerk = False
    , cost = Just 12
    , power = SpecialEffect { better = 10, worse = Nothing }
    , teamwork = NormalScore 7
    , sociability = NormalScore 8
    , morality = NormalScore 5
    , quote = "_*“Rosa is in town, she’s very capable and perhaps you two could come to... a mutually beneficial partnership.” - Penelope.*_"
    , description = "Rosa is an old vampire _(As per second-hand Immortal Blood)_ with an unnatural ability to copy magical abilities temporarily, and with her many years of life, she’s learned to make good use of most powers out there. For a time she was a member of the Hespatian Coven, but has largely distanced herself from them in recent years, acting on her own. She doesn’t know if her master is still alive or not, and the thought of his return fills her with dread."
    , positives =
        [ "+ Charming and pleasant"
        , "+ Very knowledgeable."
        ]
    , negatives =
        [ "- Can’t disobey the vampire that turned her, her master,"
        ]
    , mixed = [ "+/- Is stronger when well fed, weakens with hunger" ]
    , has = "Rosa is a vampire + copies the (magical) power & perks of the last person she fed on"
    }


penelope : Details
penelope =
    { name = Penelope
    , class = ClassOne Sorceress
    , races = [ Daeva ]
    , hasPerk = False
    , cost = Just 10
    , power = NormalScore 4
    , teamwork = NormalScore 6
    , sociability = NormalScore 8
    , morality = NormalScore 6
    , quote = "_*\"Oh hey, would you look at that. Looks like I might be in your future myself, You can crash here if you want.\" - Penelope.*_"
    , description = "I’m technically a Lunabellan at heart, but I’ve carefully positioned myself as a neutral party as a Guidance Witch that can induce early magical awakenings in witches, so I live hidden among human society with eyes open for new witches, or those not yet awake. None of the big factions bother me as a result, so long as I do my best not to play favorites."
    , positives =
        [ "+ Is this where I say an Office quote?"
        , "+ I’ve got a great rack?"
        ]
    , negatives =
        [ "- None, clearly"
        , "- I must stay neutral"
        ]
    , mixed = [ "+/- \"Sometimes, I just care too much, I love to travel and take long walks on the beach. I’m not like other girls\" /s" ]
    , has = "Pen has Potions 3, Hexes 3, Domain 3, Familiar 4 (Cow), Witchery 4, Necromancy 5, Divination 5, _Cornucopia, Prestidigitation, Third Eye & Yaga Root_"
    }


mandyHunts : Details
mandyHunts =
    { name = MandyHunts
    , class = ClassNone
    , races = [ Neutral ]
    , hasPerk = False
    , cost = Just 1
    , power = NormalScore 0
    , teamwork = NormalScore 8
    , sociability = NormalScore 5
    , morality = NormalScore 6
    , quote = "_*“Followed up on a report of class 3 spirit beast stalking a human and both disappearing into the shadows, here’s the file” - ORC Agent.*_"
    , description = "Mandy is a human Medium, she can see through the veil to see spirits, monsters, and magic in general. Due to a botched Occultism ritual, Mandy is left with the ability to both enter the spirit world like a Sylph, but enter the Shadow world as with Occultism 4 She can unreliably provoke the transition herself, but it’s usually involuntary. She’s currently terrified for her life in the shadow world hiding from a spirit beast."
    , positives =
        [ "+ Very knowledgeable in occult."
        , "+ Doesn’t take life for granted, and enjoys the little things"
        ]
    , negatives =
        [ "- But from a human level."
        , "- Somewhat emotionally scarred from her life"
        ]
    , mixed = [ "+/- By witch standards, defenseless against the monsters that can now see her as easily as she sees them" ]
    , has = "Mandy is a human, no faction relation"
    }


eskhanderMahabadi : Details
eskhanderMahabadi =
    { name = EskhanderMahabadi
    , class = ClassOne Sorceress
    , races = [ Oread ]
    , hasPerk = False
    , cost = Just 10
    , power = NormalScore 7
    , teamwork = NormalScore 8
    , sociability = NormalScore 5
    , morality = NormalScore 4
    , quote = "_*“Eskhander Mahabadi... That’s the awakened Tiger right? Strange that, never heard of non-humanoids awakening.” - An ORC Scholar*_"
    , description = "No, not the girl -- The tiger. Eskhander is a literal tiger that received a witch awakening when poisoned by a Witchblood Elixir. Like a human witch becoming a non-human, Khan awakened as an Oread, but has Transformation Sequence, retaining his natural male tiger body and a separate witch form. He has a familiar cat that spends most her time in human form and learned to do a lot of the talking."
    , positives =
        [ "+ Curious & inquisitive"
        , "+ Deferential but confident"
        ]
    , negatives =
        [ "- _Might_ eat your face if you provoke him."
        ]
    , mixed = [ "+/- Understands the wider world he woke up to is different than what he knew, and is patient in learning new norms." ]
    , has = "Khan has Runes 2, Earthmoving 3, Witchery 5, Familiarity 5, Metamorphize 5 (Zooarch), _Transform Sequence, and Menagerie_"
    }


experiment627 : Details
experiment627 =
    { name = Experiment627
    , class = ClassOne Sorceress
    , races = [ Doll ]
    , hasPerk = False
    , cost = Just 8
    , power = NormalScore 10
    , teamwork = NormalScore 10
    , sociability = NormalScore 4
    , morality = NormalScore 4
    , quote = "_*“When we found her she was stuck in a loop putting books back on a shelf as a poltergeist kept knocking them over.” - ORC Agent.*_"
    , description = "627 is an advanced Doll experiment trying to recreate Doll witches, but without pesky things like free will getting in the way, but more intelligent and lifelike than the mindless serving dolls. 627 is a real person and a Doll witch, her past life heavily suppressed and faded away. She struggles hard to think for herself as though physically painful. She can bond a Master by occupying their Familiar spot"
    , positives =
        [ "+ Benefits from anything that would benefit a familiar"
        , "+ Extremely proficient"
        ]
    , negatives =
        [ "- Can only be independent within bounds of an order"
        , "- Slow, at first, to adapt."
        ]
    , mixed = [ "+/- Her body is a combination of flesh and ball-joint doll." ]
    , has = "627 has Necromancy 3, Windcalling 3, Hexes 4, Witchery 4, _Hot Swap, Maid Hand, and Levitation_"
    }


augustRoseOBare : Details
augustRoseOBare =
    { name = AugustRoseOBare
    , class = ClassNone
    , races = [ Neutral ]
    , hasPerk = False
    , cost = Just 10
    , power = NormalScore 0
    , teamwork = NormalScore 0
    , sociability = NormalScore 0
    , morality = NormalScore 0
    , quote = "_*“I haven’t seen this happen before, are you seeing the same thing? This vision is just static to me.” - Penelope.*_"
    , description = "Rose is a quiet witch, speaking seems to fatigue her, but shelll reply or say things when important. The Veil hides her from everyone else, as a monster would be hidden from a mortal, she is hidden from even witches. Even physically moving things to try to convey a message is obscured or dismissed in some way: If you select her as a Companion you will be the only one who can sense her, and can be seen in the background of casual daily life as though she’s just watching the life go by of everyone else, vanishing and appearing suddenly. She’ll write in a small notebook, the pages look like static to you. Sometimes she’ll help you with single written words or pointing. She’s very passive and not very helpful, but your life seems to become more interesting. *You can take any 1 extra Quest.*"
    , positives = []
    , negatives = []
    , mixed = []
    , has = ""
    }


opheliaReisha : Details
opheliaReisha =
    { name = OpheliaReisha
    , class = ClassOne Sorceress
    , races = [ Hannya, Lilin ]
    , hasPerk = False
    , cost = Just 10
    , power = NormalScore 5
    , teamwork = NormalScore 9
    , sociability = SpecialEffect { better = 8, worse = Just 4 }
    , morality = NormalScore 8
    , quote = "_*“Eli? She’s the best, she’s so smart and works hard, I can’t work that hard if I tried, not with all those words and stuff.” - Esther.*_"
    , description = "Twin sister to Esther, Ophelia is older by 5 minutes. She’s the more intellectual of the two and very focused on acquiring knowledge and learning more, expanding her education. She’s currently kept up some nights by trying to research the disappearance of their mother, Gwendoline, who vanished, the only clue being an abundance of pumpkins."
    , positives =
        [ "+ Sharp and intelligent"
        , "+ Mind for puzzles"
        , "+ Yearns for knowledge"
        ]
    , negatives =
        [ "- A bit of a wallflower if not encouraged by her sis, Ess"
        , "- Afraid of isolation"
        ]
    , mixed = [ "+/- Without Ess, Eli is a single minded workaholic to her detriment. With Ess, she’ll lighten up and be playful." ]
    , has = "Eli has Portals 2, Consortation 3, Hexes 3, Witchery 4, Wands 4, Alchemy 4, Domain 5, Firecalling 5 _& Prestidigitation_"
    }


estherReisha : Details
estherReisha =
    { name = EstherReisha
    , class = ClassOne Sorceress
    , races = [ Hannya, Lilin ]
    , hasPerk = False
    , cost = Just 10
    , power = NormalScore 5
    , teamwork = NormalScore 9
    , sociability = NormalScore 8
    , morality = SpecialEffect { better = 9, worse = Just 5 }
    , quote = "_*“Ess? What can I say, I love my sister, she’s always there to help me remember to actually live and when to take a break.” - Ophelia.*_"
    , description = "Twin sister to Ophelia, Esther is younger by 5 minutes. She’s the more physically active athlete type of the two that always seems to have the energy to keep going, but really it’s just a front to try to support Eli, who was major depressive in their childhood, she doesn’t want her sister to end up like that again and while she’s at it she may as well help others."
    , positives =
        [ "+ Uplifting to be around."
        , "+ Athletic & energetic"
        , "+ Loves to cook for people."
        ]
    , negatives =
        [ "- A would-be wanton hedonist delinquent if not for Eli"
        , "- Afraid of isolation"
        ]
    , mixed = [ "+/- Without Eli, Ess loses focus and starts to care less about others, feeling disconnected from the world." ]
    , has = "Ess has Ministration 1, Hexes 3, Necromancy 3, Witchery 4, Familiarity 5, Firecalling 5 _& Prestidigitation, Memorize, Windsong_"
    }


custom : Details
custom =
    { name = Custom
    , class = ClassAny
    , races = []
    , hasPerk = False
    , cost = Nothing
    , power = SpecialEffect { better = 1, worse = Just 0 }
    , teamwork = SpecialEffect { better = 1, worse = Just 0 }
    , sociability = SpecialEffect { better = 1, worse = Just 0 }
    , morality = SpecialEffect { better = 1, worse = Just 0 }
    , quote = "_*“What’s a quote from a third party that references the character” - Source*_"
    , description = """
        You can take only 1 Custom companion. Custom companions can’t have notable special features that aren’t represented by Magic, Perks, or Relics at the end. Increase Custom’s p/Rp cost by +1 per 5 added ranks in Stats above, or +1 per 2 in the Power stat

        Custom companions cannot have more than 8 magics/features, and cannot be Sword type."""
    , positives =
        [ "+ Positive quality"
        , "+ Positive quality"
        ]
    , negatives =
        [ "- Negative quality"
        , "- Negative quality"
        ]
    , mixed = [ "+/- A quality that could go either way based on player opinion, or has it’s ups and downs," ]
    , has = "What stuff do they have? For every 1p companion cost, use 4 Power for their build costs."
    }


erisJulianariStonefallen : Details
erisJulianariStonefallen =
    { name = ErisJulianariStonefallen
    , class = ClassOne Sorceress
    , races = [ Nymph ]
    , hasPerk = True
    , cost = Just 15
    , power = NormalScore 5
    , teamwork = NormalScore 8
    , sociability = NormalScore 9
    , morality = NormalScore 7
    , quote = "_*“All I can say is that any agent from ANY faction that gets too close is never seen again, leave it be for now.” - An ORC Agent.*_"
    , description = "Eris grew up isolated yet in the spotlight, attending only the best schools her entire life never even looking upon culture that was less than upper middle class. This isn’t for lack of intent on her part, but her mysterious father she hasn’t even met sets her life up for her from a distance. Anywhere she goes, unseeable guardians shield her every movement equivalent to 6 undetectable Solarchs that operate subtly."
    , positives =
        [ "+ Very insightful, (knows something is weird but also not to ask questions)"
        ]
    , negatives =
        [ "- Chronically lonely feeling disconnected from others."
        , "- A little naive to the world."
        ]
    , mixed = [ "+/- Incredibly innocent despite what most might expect" ]
    , has = "Eris has Potions 3, Witchery 3, Occultism 3, Waterworking 3, Familiarity 5 (*_black unicorn_*), _Shroud, Gold Card_"
    }


xiaoLiena : Details
xiaoLiena =
    { name = XiaoLiena
    , class = ClassOne Academic
    , races = [ Neutral ]
    , hasPerk = False
    , cost = Just 8
    , power = NormalScore 10
    , teamwork = NormalScore 7
    , sociability = NormalScore 8
    , morality = NormalScore 9
    , quote = "_*Where have I seen her before... Ah! Of course, I recognize her from the Chinese ORC division, I heard she retired.” - Miranda to May.*_"
    , description = "The girl you helped gives her name as Xiao Liena, and in giving her one of the Flowers of Life, her body will be healed and she’ll gain the ability to use Lifeweaving. She used to be a member of the Chinese ORC, but retired from the job to pursue her grandmother’s stories of the Flower."
    , positives =
        [ "+ Li is a gentle but adventurous soul that travels and helps all along the way"
        ]
    , negatives =
        [ "- Overly hesitant to resort to violence to her detriment, but wont hold it against you"
        ]
    , mixed = [ "+/- The Chinese ORC has no relation to the PRC since the Mao regime, and is based out of Taiwan now. In short, Liena is not complicit in Chinese actions since pre-Mao." ]
    , has = "Li has Lifeweaving 1, Familiarity 2, Ministration 3, Windcalling 3, Witchery 4, Divination 4, Alchemy 5, _Alchemist Stash_"
    }


jin : Details
jin =
    { name = JinChooseAName
    , class = ClassSpecial

    -- TODO allow the user to choose the affinity
    , races = [ Genie Life ]
    , hasPerk = False
    , cost = Just 20
    , power = NormalScore 10
    , teamwork = NormalScore 6
    , sociability = NormalScore 6
    , morality = NormalScore 10
    , quote = "_*“Who?” - An ORC Records analyst.*_"
    , description = "“Jin” is a genie who had been trapped in her vessel for who knows how long until you found it and got that key. She’ll look like the image when you first see her but can change to whatever you’d like. She wont give her name even if commanded, requesting you give her one. Should you see inside her vessel you’ll see her room and a whole modern world out the window, with a computer displaying an image editor, and a reddit tab open on one monitor."
    , positives =
        [ "+ Doesn’t try to trick or twist wishes around. Does her best."
        ]
    , negatives =
        [ "- May often only do what is asked, then do her own thing."
        ]
    , mixed = [ "+/- Can refuse orders within her vessel or about her past." ]
    , has = "Jin has Digicasting 4, Divination 4, Witchery 5, Wishcasting 5, Psychotics 5, _Soul Graft (Jinn), Witch hut? (Inside Vessel), Keeper x3, Comfy Pocket (TSCZ), Magical Heart (20p)_"
    }


saraStar : Details
saraStar =
    { name = SaraStar
    , class = ClassSpecial
    , races = [ Cyborg ]
    , hasPerk = False
    , cost = Just 15
    , power = NormalScore 8
    , teamwork = NormalScore 6
    , sociability = NormalScore 7
    , morality = NormalScore 10
    , quote = "_*“She’s stubborn, she seems to only want to speak with you. Be careful, anything could happen.” - A cautious First Contact responder.*_"
    , description = "Goes by Sara Star, a witch from what she calls _Tera Prima_ located in NGC 6960. She says she’s the only survivor she knows of from an alien invasion by a race of Outsider beings that crawled out from the void during an arcanotech experiment in interplanar travel, resulting in a _Zero Point Break_ in the local aether. erasing her entire home system."
    , positives =
        [ "+ Honest and hardworking."
        , "+ Her form of Integration has no Alphazon connection."
        ]
    , negatives =
        [ "- Relies on a translator for now"
        , "- While intelligent, she can be comically oblivious to norms."
        ]
    , mixed = [ "+/- Being a homeless witch from a lost world, she’s in the market for a new home and new connections, maybe love." ]
    , has = "Sara has Runes 2, Alchemy 2, Aethernautics 2, Windcalling 3, Hexes 3, Dominion 4, Divination 4, Integration 5 _Her ship: Red Betty_"
    }


redBetty : Details
redBetty =
    { name = RedBetty
    , class = ClassNone
    , races = [ Neutral ]
    , hasPerk = False
    , cost = Nothing
    , power = NormalScore 0
    , teamwork = NormalScore 10
    , sociability = NormalScore 10
    , morality = NormalScore 0
    , quote = "_*This is Red Betty, my best. well, I guess my only friend now, you know, with my home planet deleted and all.” - Sara Star.*_"
    , description = "Nicknamed Red Betty by Sara Star, this is her ship. The inside is far larger than the outside would suggest, only being around the size of a fighter aircraft externally. It would have heavy damage when you first see it, but Sara will be able to fix it after the conflict is resolved. Should you take Sara as a companion and grow close enough, she may let you tag along... or may even build a ship for you via 15rp/(epic)"
    , positives = []
    , negatives = []
    , mixed = []
    , has = "Red Betty is made with Comfy Spaceship: [https://imgur.com/a/OGMd0JN]. *_Human Space Yacht, Wood Paneling | Plant Garden 1, Fabricator 2, Cloning Tank 2, Medical Lab 3, AR Room 1, Bathing Amenities 1, Expanded Cabin 1, Training Complex 1, Recreational Activities 1 & 2 | A.I. Companion, Synthetic Android 3, Powered Armor 1, Holo Map 1, EVA Suit 1, Bridge Windows 1. | Wonders irrelevant_*"
    }


outsiders : List Details
outsiders =
    [ theCaretaker, lostQueen, giftFromBeyond, agent9sOriginal ]


theCaretaker : Details
theCaretaker =
    { name = TheCaretaker
    , class = ClassOne Warlock
    , races = [ Lamia ]
    , hasPerk = True
    , cost = Just 8
    , power = NormalScore 7
    , teamwork = NormalScore 4
    , sociability = NormalScore 3
    , morality = NormalScore 1
    , quote = "_*“Death is preferable to being caught by the Caretaker, do not hesitate to kill yourself before captured. Do not forget.” - An ORC Stormtrooper.*_"
    , description = "The Caretaker is an especially active Outsider cultist worshiping an abyssal entity. She’s on the verge of completely abandoning humanity with a heavily twisted form to be closer to her Far God. She is known for going out of her way to take captives and use them to breed large swarms of aberrant monsters with parasite implants that corrupt the very soul and twist the mind beyond recovery."
    , positives =
        [ "+ Actually very close with allies, will netflix and chill."
        ]
    , negatives =
        [ "- With such a twisted form, she can’t walk in public."
        ]
    , mixed = [ "+/- Frequently mutters in a far-speech, receiving inspiration and visions of what to do, making her seem super lucky" ]
    , has = "The Caretaker has Necromancy 3, Occultism 4, Aethernautics 4, Waterworking 5, Monstrosity 5, _Aberration & Crystallize_"
    }


lostQueen : Details
lostQueen =
    { name = LostQueen
    , class = ClassOne Sorceress
    , races = [ Sprite ]
    , hasPerk = True
    , cost = Just 15
    , power = NormalScore 6
    , teamwork = NormalScore 10
    , sociability = NormalScore 7
    , morality = NormalScore 1
    , quote = "_*“Do you hear something?”* \\*Increasing shill metallic buzzing\\* *“ARGH,\u{00A0}OUT OUT O-”* \\*gurgle\\* * - An ORC audio log.*_"
    , description = "A once renown Fairy Queen in Alfheimr that succumbed to a major Outsider incursion within a minor realmspace known as the Everglen, now the once great sprite is twisted by a technophage class Far God, her body a blend of flesh and metal, carried on razor wings. Effectively a new being, but old instincts continue to yearn for the familiar. She constructs gardens of living metal with her iron daughters."
    , positives =
        [ "+ Cunning and intuitive"
        , "+ Loves to tinker and create."
        ]
    , negatives =
        [ "- Increasingly unstable in natural environs."
        ]
    , mixed = [ "+/- Arabella is uniquely able to use Hexes to replicate the effects of Gadgetry and Integration combined." ]
    , has = "Arabella has Alchemy 1, Necromancy 2, Monstrosity 3, Windcalling 4, Curses 4, Hexes 5, and Covenants 5"
    }


giftFromBeyond : Details
giftFromBeyond =
    { name = GiftFromBeyond
    , class = ClassAny
    , races = []
    , hasPerk = False
    , cost = Just 12
    , power = NormalScore 10
    , teamwork = NormalScore 10
    , sociability = NormalScore 4
    , morality = NormalScore 1
    , quote = "_*“Meagota fabf ya gof’n ng bugnab shuggogg pb nghut ya yaor mghftnab cabf hw’nafbnab abornab ab’fnab Hoigshogg ot yogfn’ll”*_"
    , description = "You receive the egg of an eldritch child of the Far God of your choice (Affecting the aesthetic). This egg needs to be incubated, either in a living womb (yours or another’s), or in a sufficiently advanced or occult chamber. Once the egg has been fed the blood of 100 different souls one way or another, it hatches into a beast of your design up to the size of beasts shown in Metamorphosis, and is a witch of a chosen type. The monster will have a personality reflecting its Far God but will be loyal to you, more so if you bore it yourself to the point of going against its own nature with a telepathic bond. It can also benefit from any options to improve your familiar, and has its own Power and witchcrafts. It cannot bond to non-Outsiders unless they bore it themselves."
    , positives = []
    , negatives = []
    , mixed = []
    , has = "Egg has 3x Power to spend & Any 2 Affinities"
    }


agent9sOriginal : Details
agent9sOriginal =
    { name = Agent9sOriginal
    , class = ClassOne Warlock
    , races = [ Aurai ]
    , hasPerk = True
    , cost = Just 12
    , power = NormalScore 10
    , teamwork = NormalScore 3
    , sociability = NormalScore 5
    , morality = NormalScore 2
    , quote = "_*“NINES! *\\*Explosion\\** Nines, what’s gotten into you! Stop! NO!”* \\*Sound of chains clanking\\* *- An Alphazon Informant.*_"
    , description = "The original body of 9s. During a mission, Nines infiltrated an unexpected Outsider nest where she was overwhelmed and taken. Alphazon resorted to a hard reset, her soul safe in a datacrypt for housing in a new body. But the original is still out there- Still remembering everything, still feeling just as herself, and twisted into believing in the White Serpent god who imbued her with a blank soul of her own."
    , positives =
        [ "+ Still chill, if more melancholic"
        ]
    , negatives =
        [ "- No regard for the lives of others, very jaded."
        ]
    , mixed = [ "+/- Her only purpose in life is to satisfy the White Serpent, who promises her peace and...a simple family life, someday" ]
    , has = "9s has Alchemy 3, Runes 4, Occult 4, Psychotics 5, Familiar 5 (Snake), Monstrosity 5, _Aberration_"
    }


alliance : List Details
alliance =
    [ princessDaelEzraOfCharis, anaphalonGreenwield, briarGracehollow, duchessSaelAstraOfOdalle ]


princessDaelEzraOfCharis : Details
princessDaelEzraOfCharis =
    { name = PrincessDaelEzraOfCharis
    , class = ClassOne Academic
    , races = [ Elf ]
    , hasPerk = True
    , cost = Just 8
    , power = NormalScore 2
    , teamwork = NormalScore 6
    , sociability = NormalScore 9
    , morality = NormalScore 9
    , quote = "_*““What’s Daeal’ezra deal? I don’t get why she works so hard for the court instead of just marrying in. I know she has suitors” - A courtier.*_"
    , description = "The second oldest of 12 daughters of an Alfheimr queendom the size of Poland, her mother holds a position at the high table of the Summer Court as the local queen but her father is a duke of the Winter Court, a scandal that almost cost her her crown, but with political maneuvering, the illegitimate Dael’ezra is simply not permitted to inherit titles and must earn a position in either court the hard way."
    , positives =
        [ "+ Literal princess"
        , "+ Competent warrior"
        ]
    , negatives =
        [ "- Slightly vindictive"
        , "- Constant need to prove herself"
        ]
    , mixed = [ "+/- Somewhat lofty and larger than life, everything around her feels more serious and important." ]
    , has = "Ezra has Witchery 2, Necromancy 2, Portals 3, Wands 3, Ministration 4, Divination 4, Covenants 5 _& Fae Step_"
    }


anaphalonGreenwield : Details
anaphalonGreenwield =
    { name = AnaphalonGreenwield
    , class = ClassOne Academic
    , races = [ Siren ]
    , hasPerk = False
    , cost = Just 12
    , power = NormalScore 4
    , teamwork = NormalScore 9
    , sociability = NormalScore 10
    , morality = NormalScore 8
    , quote = "_*You’d be hard pressed to find a more loyal and dedicated knight, but he’s a little single-minded in his quest, within reason.” - A Courtier.*_"
    , description = "Born into a wealthy noble clan governing a small fiefdom in the Albion region in Alfheimr, “Talon” lived a happy life until an Outsider plot put him and his town through 12 nights of hell. In the aftermath, his eldest brother inherited their father’s title, while he became a Questor -- A wandering knight seeking justice on behalf of his family and civilians He pursues Outsider events, while serving the Courts."
    , positives =
        [ "+ Social savant with quick wit"
        , "+ Strategic master swordsman."
        ]
    , negatives =
        [ "- Fairly arrogant"
        , "- Class based worldview."
        ]
    , mixed = [ "+/- Defers to authority above him, the same as he expects authority to be deferred to him by those below." ]
    , has = "Talon has Dominion 3, Windcalling 3, Covenants 5, _Soul Warrior_ (Design a sword) _& Fae Step_"
    }


briarGracehollow : Details
briarGracehollow =
    { name = BriarGracehollow
    , class = ClassOne Warlock
    , races = [ Elf ]
    , hasPerk = False
    , cost = Just 6
    , power = NormalScore 7
    , teamwork = NormalScore 9
    , sociability = NormalScore 2
    , morality = NormalScore 4
    , quote = "_*“Her? Don’t mind her, it’s only been a few years since she was living in the wilds, she hasn’t gotten the hang of clothing yet.” - A Pixie*_"
    , description = "As a child, little elven girl was offered to appease the terms of a covenant made by the mother many years prior, given to wild spirits of the Faewild. These white elk beings raised her in harmony with the spirit of the Wolf, On her 100th birthday: comparable to a human’s 18th, she completed her terms of service and was released to the Alliance."
    , positives =
        [ "+ Highly loyal to her “pack”"
        , "+ Effective survivalist"
        , "+ Adaptive to most things, but"
        ]
    , negatives =
        [ "- Dismissive of those who are not part of that “pack”"
        , "- Intimidated by technology."
        ]
    , mixed =
        [ "+/- Will obey all orders of the “alpha”, if no one becomes “alpha” then she will take on this role."
        ]
    , has = "Briar has Covenants 2, (_Improved_) Familiar 2 (Dire Wolf), Naturalism 4, and _Poisoner_"
    }


duchessSaelAstraOfOdalle : Details
duchessSaelAstraOfOdalle =
    { name = DuchessSaelAstraOfOdalle
    , class = ClassOne Sorceress
    , races = [ Vanir, Sylph ]
    , hasPerk = False
    , cost = Just 12
    , power = NormalScore 5
    , teamwork = NormalScore 7
    , sociability = NormalScore 5
    , morality = NormalScore 8
    , quote = "_*“Sael’astra must be having a bad week, it’s been constant overcast” - new guy “I wouldn’t worry about it, happens every month.” - a local*_"
    , description = "A well liked public figure of the Winter Court. She isn’t high in the ranks, but she’s a minor duchess of a city-state in the far north of the Albion region, but she isn’t around a lot, regulating a lot of the politics and management to her sister, who lacks political weight but is good at logistics and keeping people happy, while Sael’astra can deal with the Courts, and protect the city with her magical talent."
    , positives =
        [ "+ A natural at using storm magic"
        , "+ Heavy emphasis on ice magic."
        ]
    , negatives =
        [ "- Bad mood... bad weather"
        , "- Control can be unstable."
        ]
    , mixed = [ "+/- Especially potent magic, approximately 78% as potent as would be expected of a witch of her ranking." ]
    , has = "Astra has Runes 2, Witchery 3, Aethernautics 4, Windcalling 5, Waterworking 5 (Ice), _Fae Step, Hybrid, & Mood Weather_"
    }
