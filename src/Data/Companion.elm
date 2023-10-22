module Data.Companion exposing (Details, all, intro)

import Generated.Types exposing (Class(..), Companion(..), Race(..))


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
    It’s dangerous to go alone, take this! A Companions section. {choice You have one free companion from your chosen faction, and one free that shares the same witch type} as you (Either Race or Class). Companions have 4 main statistics giving a _rough idea_ of their general impact in a group dynamic, except the {choice POWER stat, which primarily serves to give free floating Power points to spend to _customize_ your companion's abilities} on top of the default abilities they come with. Beyond that, interactions are inferred in how you interpret their personality and how different stats play out in relation to it and others. {choice You can buy additional companions using Reward Points}, listed to the left of their name. A {choice +} indicates they have their Type Perk.

    You can spend your own Power on behalf of a member if you so choose.

    **Really, don't stress the details too much and feel free to fill in the blanks. I do what I can to provide a reasonable summary of an array of characters, but it's up to you to interpret it as you will.**

    Take your pick:
    """


all : List ( String, List Details )
all =
    [ ( "The Arcadians", arcadians ) ]


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
