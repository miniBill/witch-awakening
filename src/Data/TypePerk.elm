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
    [ dravir, genie, gemini ]


dravir : Details
dravir =
    { race = Dravir All
    , cost = 5
    , dlc = (Data.Race.dravir []).dlc
    , content = """
        Unlike most type perks, it’s rare to see a Dravir without this: The ability to assume the form of a dragonbeast, a form of humanoid dragon-type monster, being a bipedal dragonoid with 5 digit hands with full functionality of normal human arms, tipped with retractile razor claws, and they gain wings that can fly. May or may not have drastic sexual dimorphism depending on your lineage. Which is to say, female dragonbeasts may still look far more human with changes centered mostly around the back and limbs. This form empowers the Draviri’s physical attributes by 150%, and empowers their breath weapon by 300%. Dravir with this perk do not age past prime. Can partially transform, such as just growing wings.
        """
    }


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
