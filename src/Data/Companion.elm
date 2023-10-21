module Data.Companion exposing (Details, all, intro)

import Generated.Types exposing (Class(..), Companion(..), Magic(..), Perk, Race(..))
import Types exposing (RankedMagic)


type alias Details =
    { name : Companion
    , class : Class
    , race : Race
    , cost : Int
    , power : Int
    , teamwork : Int
    , sociability : Int
    , morality : Int
    , quote : String
    , description : String
    , positives : List String
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


all : List Details
all =
    [ rachelPool ]


rachelPool : Details
rachelPool =
    { name = RachelPool
    , class = Academic
    , race = Neutral
    , cost = 4
    , power = 5
    , teamwork = 8
    , sociability = 4
    , morality = 9
    , quote =
        """"Has anyone seen Rachel around? We have a double date in 20 minutes and she totally vanished.” - an Arcadian student."""
    , description =
        """Rachel is very introverted and socially awkward, she grew up largely alone without a lot of interaction, so other people may as well be aliens for her and she freaks out about saying the wrong thing, but once she makes real friends she cares for them a lot and can find her stride. She's very fond of animals and spends a lot of time with her cat, and going on walks alone as to not bother anybody."""
    , positives =
        [ "Unexpectedly athletic/nimble."
        , "Goes out of her way for friends."
        , "Talks to animals."
        , "Will at least *try* things beyond her comfort zone."
        ]
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
