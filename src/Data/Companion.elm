module Data.Companion exposing (Details, all, intro)

import Generated.Types exposing (Class(..), Companion(..), Magic(..), Perk(..), Race(..))
import Types exposing (RankedMagic)


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
    , mixed : List String
    , negatives : List String
    , magics : List RankedMagic
    , perks : List Perk
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
    [ rachelPool, anneLaurenchi ]


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
    , quote =
        """“Has anyone seen Rachel around? We have a double date in 20 minutes and she totally vanished.” - an Arcadian student."""
    , description =
        """Rachel is very introverted and socially awkward, she grew up largely alone without a lot of interaction, so other people may as well be aliens for her and she freaks out about saying the wrong thing, but once she makes real friends she cares for them a lot and can find her stride. She's very fond of animals and spends a lot of time with her cat, and going on walks alone as to not bother anybody."""
    , positives =
        [ "Unexpectedly athletic/nimble."
        , "Goes out of her way for friends."
        , "Talks to animals."
        , "Will at least _try_ things beyond her comfort zone."
        ]
    , mixed = []
    , negatives =
        [ "Slow to adapt."
        , "Very nervous."
        , "... more than people."
        ]
    , magics =
        [ RankedMagic Alchemy 2
        , RankedMagic Witchery 1
        , RankedMagic Hexes 2
        , RankedMagic Familiarity 3
        , RankedMagic Naturalism 3
        ]
    , perks = []
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
    , quote = "“Is..Is that Anne? How on earth can she sleep like that. Is that safe? feel like I should help her out” - An Arcadian student"
    , description = "Anne is a bit of an oddball. Rather than simply being introverted, she's pretty much outright antisocial seeing no need or reason to bother and actively prefers keeping a smaller friendgroup, but she tries to do right by those that do make that cut. She's known for sleeping most her days away and not just in her bed, but anywhere she goes, some think she has narcolepsy; but she just enjoys dreams a ton."
    , positives =
        [ "Endless imagination"
        , "Powerful when she wants to be..."
        , "Highly skilled digital artist."
        , "Likes to show off dreamworlds with friends"
        ]
    , mixed = []
    , negatives =
        [ "Ungrounded in reality."
        , "... Would rather nap."
        , "Slow even when awake"
        ]
    , magics =
        [ RankedMagic Familiarity 1
        , RankedMagic Aethernautics 2
        , RankedMagic Portals 3
        , RankedMagic Psychotics 3
        , RankedMagic Digicasting 4
        ]
    , perks = [ BeautySleep ]
    }
