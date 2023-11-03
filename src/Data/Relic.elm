module Data.Relic exposing (Content(..), Details, all, intro)

import Generated.Types exposing (Class(..), Relic(..))


type alias Details =
    { name : Relic
    , class : Class
    , content : Content
    }


type Content
    = Single Int String
    | WithChoices String (List Int)


intro : String
intro =
    """
    # Relics

    {center} Relics are like perks, but are external boons in the form of magical artifacts, as a rest they aren't things inherent to yourself, but are things you can acquire over time.

    {center} "Let's see if we can detect any Relics in your future, they sometimes show up in these tests..."

    {center} {choice *Relics cost REWARD points obtained from Quests, as shown, however you can buy them with POWER instead, if you so choose.*}
    """


all : List Details
all =
    [ hexVPN ]


hexVPN : Details
hexVPN =
    { name = HexVPN
    , class = Academic
    , content = Single 2 """
        The ultimate in secure internet access. HexVPN routes your internet traffic through a literal blackbox. The ominous hovering black cube with fractal engravings completely masks your connection in ways mortal VPNs can't; Your traffic won't even be seen or register as a request, and it allows you to access any public website that would otherwise have a paywall. No more sharing netflix accounts, and no region locks, rocking 666 Tbps up and down.
        """
    }
