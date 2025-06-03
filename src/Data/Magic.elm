module Data.Magic exposing (Affinities(..), Details, elementalismIntro, intro, slotDescription, title)

import Generated.Types exposing (Affinity(..), Class(..), Magic(..))


type alias Details =
    { name : Magic
    , hasRankZero : Bool
    , class : Maybe Class
    , affinities : Affinities
    , isElementalism : Bool
    , description : String
    , ranks : List String
    , dlc : Maybe String
    }


type Affinities
    = Regular (List Affinity)
    | Alternative (List (List Affinity))


title : String
title =
    "# The Magic"


intro : String
intro =
    """
    "Time for the fun part. We’ve isolated your true form and primed it for emergence, but that’s only a small part of what you can actually do. The magic varies from witch to witch, we’ll run through the possibilities so I can get a sense of what’s resonating with you. It’s a pretty reliable method of detecting what you’ll be capable of in the future as you explore your abilities and grow your talents."

    Note that these are the possibilities isolated for you, not all witches would have the same opportunities you do. You are exceptional and have more options, and to a higher ceiling than most. Rank 3 in one or two specializations would be considered a capable witch. There are whispers of witches with rank 6 magic, while 7+ are the domain of gods, who are very real.

    {choice Each rank in a magic specialization below costs power equal to its rank, in sequential order}. le; Rank 5 magic costs 15 points in total, rank 3 would be a total of 6p. {choice All Specializations have associated Affinities tagged}. If you have one of these affinities, the magic costs half the power, {choice *rounding up*}. [All] are universally discounted to all affinities.

    For every Rank 5 magic, you must have at least one other magic at Rank 4. For every rank 4, you need 1 magic of rank 3 or less. This does not apply to the either Slot game mode changes, which behave in isolation. Slots stand on their own. This applies to the player, but non-player characters need not adhere to player mechanics and can be presumed to have various less notable magical traits not listed.

    {choice [star] next to the name show that a magic specialization has a universal “Rank 0” effect, which IS available to nearly every witch}, though this does not imply any innate skill with the magic specialization that was built on top of some aspect of that magic.

    If you have at least 1 rank in a Magic Specialization, you can spend _Focus_, _Might_, or _Favor_ to temporarily “power up” to use the higher rank of magic, equal to what the Power cost would be to unlock that rank (e.g.: 5 power = 5 Focus). You can use it for 10 minutes, or extend it for an additional 10 minutes by pushing past your limits resulting in unconsciousness when time runs out.

    {center} {choice Don’t like math? Have a reference table.}
    """


slotDescription : String
slotDescription =
    "If playing in a Slot Mode (Skill Tree or Constellation), Magic instead costs a Slot as shown. Folk slots can buy rank 2 magics. You can have white “Free” slots granting rank 1 magic as granted by your Class discount on options that would cost 0."


elementalismIntro : String
elementalismIntro =
    """
    The magic presented here in this section is referred to as Elementalism. These single-affinity or monotype magics operate just a little differently from other schools of magic, in that {choice any witch other than sorceresses are incapable of learning more than one elementalist magic _that they do not have the affinity for_}.

    Each magic will show an OR clause. {choice You can qualify for Affinity discounts if you possess both component affinities}, including taking it without the primary monotype affinity.

    If you have more than one elementalist magic, you can combine effects to create combination magic. Such as Firecalling and Earthmoving for lava effects, Windkeeping and Waterworking for storm effects. This is beyond the scope of what the cyoa can detail and up to reasonable interpretation with wiggle room for creative liberties. {choice You can also perform _Harmony_ magic, combining elements of cooperating witches, twice as potent as normal}.

    Witches with an elemental magic at rank 3 are often called _Hazards_. Rank 4 are _Disasters_, and Rank 5 can be labeled as _Calamities_. This designation can influence the weight of consequences placed on you for reckless behavior, people don’t trust living weapons with a track record of being careless when many lives can be at stake if you prove unstable, or outright malevolent. Witches with _Curses_ also receive this designation, as well as others on a case by case basis, such as some grenadier alchemists.

    {choice Elementalist magics cannot be taken for the _Restriction_} complication unless you have the affinity for it.
    """
