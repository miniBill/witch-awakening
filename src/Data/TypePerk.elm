module Data.TypePerk exposing (Details, all, title)

import Data.Race
import Generated.Types exposing (Affinity(..), Race(..))


type alias Details =
    { race : Race
    , cost : Int
    , content : String
    , dlc : Maybe String
    }


all : List Details
all =
    [ genie, gemini ]


genie : Details
genie =
    { race = Genie All
    , cost = -5
    , dlc = (Data.Race.genie []).dlc
    , content = """
        All genies require this type perk. Genies have a Vessel, an invulnerable artifact between the size of a baseball, up to the size of an urn, it can change its appearance. It must appear opulent and valuable. If the Genie has Witchery, the pocketspace exists within this Vessel and with or without Witchery, the pocketspace is where she appears when banished to or simply returning to her lamp with a visible effect. As the vessel IS the pocket space, it cannot be stored within itself or in any other form of extradimensional space.

        A Genie cannot be more than 300m away from their vessel The last non-witch to touch the vessel becomes the Genie’s Master, who cannot be disobeyed (But can be argued with for a few seconds), the genie cannot cause the master harm. The master can choose the genie’s appearance and race type, when outside her vessel.
        """
    }


gemini : Details
gemini =
    { race = Gemini All
    , cost = 4
    , dlc = (Data.Race.gemini []).dlc
    , content = """
        Geminai with this type perk have even stronger bonds, capable of trading their consciousness and their half of their soul to trade places between their Egos (however minimal or great they may be divergent), or they can swap entirely as though teleporting. Gemini share a mental bond to feel the other’s emotional state, senses, & intention such that they can act as a whole, or they can fully communicate telepathically, each hearing the mental dialogue of the other if they “raise their voice”, or both parties can tighten the connection to hear their passive internal dialogue entirely as though speaking out loud to oneself near the other.

        Nothing can sever this connection, short of death.
        """
    }


title : String
title =
    "# Type Perks"
