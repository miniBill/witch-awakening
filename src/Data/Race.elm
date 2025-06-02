module Data.Race exposing (Details, all, intro, title)

import Generated.Types exposing (Affinity(..), Race(..), Size(..))
import List.Extra


type alias Details =
    { name : Race
    , affinities : List Affinity
    , tank : Size
    , charge : Size
    , content : String
    , dlc : Maybe String
    }


all : List Race -> List Details
all races =
    [ dravir races, genie races, gemini races ]


withVariantAffinity :
    (Race -> Maybe Affinity)
    ->
        { name : Affinity -> Race
        , tank : Size
        , affinities : List Affinity
        , charge : Size
        , dlc : Maybe String
        , content : String
        }
    -> List Race
    -> Details
withVariantAffinity match details races =
    let
        affinity : Affinity
        affinity =
            List.Extra.findMap
                match
                races
                |> Maybe.withDefault All
    in
    { name = details.name affinity
    , tank = details.tank
    , affinities = details.affinities ++ [ affinity ]
    , charge = details.charge
    , dlc = details.dlc
    , content = details.content
    }


dravir : List Race -> Details
dravir =
    { name = Dravir
    , tank = Low
    , affinities = [ Beast ]
    , charge = Medium
    , dlc = Nothing
    , content = """
        Draviri are what happens when those princesses end up sacrificed to the dragon with no hero(ine) to save the day. Leaving the details aside, Draviri have the horns, tail, and scales of a dragon, typically having full claws on their hands and feet. Draviri pick their secondary elemental affinity, which is manifest in a breath weapon that affects a 15ft cone or 30ft line of flame [Fire], lightning [Wind], cold and ice [Water], stone/metal shrapnel [Earth] [Metal], or poison gas / thorns [Nature] For every 10 years of life, this area increases by 5ft. Their dense draconic muscle gives them strength and stamina like a Daeva Draviri age like Elves, and if slain an egg can be found within their body that will hatch the reborn dravir within a year in the right conditions, until hatched or destroyed.

        __Dravir draw Mana__ from Destruction, when they undo the work and labor that went into producing something of value based on its value and purpose to someone else. This includes the taking of life, particularly human or witch life, which briefly provides a High charge rate.
        """
    }
        |> withVariantAffinity
            (\r ->
                case r of
                    Dravir aff ->
                        Just aff

                    _ ->
                        Nothing
            )


genie : List Race -> Details
genie =
    { name = Genie
    , tank = High
    , affinities = [ All ]
    , charge = Low
    , dlc = Just "Loose Assets"
    , content = """
        Genies are avatars of raw magic. They have the [???] type, meaning [???] types are double discounted and rounded down. All Genies then can pick any one affinity as their secondary type which heavily influences their appearance in spirit form. Their spirit form looking like a humanoid elemental of their chosen type from 1 inch to 30f tall. All genies then have a physical form based on any other witch race, as though using Hybridize to acquire it, but they can’t gain the type perks or their form of cheating death. Genies all have rank 2 in every core & faction magic, and Prestidigitation & Conjuration free, used personally, not via Mammon, & costs nothing if used to satisfy a Master’s wish Genies do not age. A slain genie returns to her Vessel, see type perk.

        __Genies draw Mana__ from Wishes, whenever any person says "I wish", and the genie is capable of satisfying that wish with her available abilities, the genie gains a low mana charge for the next hour. If her master is the one to wish, she gains her full mana capacity for meeting the wish.
        """
    }
        |> withVariantAffinity
            (\r ->
                case r of
                    Genie aff ->
                        Just aff

                    _ ->
                        Nothing
            )


gemini : List Race -> Details
gemini =
    { name = Gemini
    , tank = High
    , affinities = [ Earth ]
    , charge = Low
    , dlc = Just "Loose Assets"
    , content = """
        Gemini are split soul beings that inhabit two bodies as one synchronous whole. The bodies are always nearly identical. Geminai have bodies composed of an associated gem associated with an Affinity, ruby fire affinity for example. Each of the pair have a different gem. This gem influences the coloration of parts of their body besides their skin, though they can have gem protrusions through their skin, which is still soft like flesh on the surface. Each half of the Gemini spends Power separately on its own effects, evenly splitting their power total between them. Gemini are born looking around 8 and age 1 year per 10 for around 80-120 years before they stop aging entirely.

        A dead Gemini half will fade into stardust, but will reappear when the surviving half sleeps, waking in the other half’s arms.

        __Geminai draw Mana__ from Pairing, the more in synch the two halves are in mind, intent, and appearance, the more mana they generate proportional to distance to each other. They feel this charge rate and are uncomfortable when it is weakened.
        """
    }
        |> withVariantAffinity
            (\r ->
                case r of
                    Gemini aff ->
                        Just aff

                    _ ->
                        Nothing
            )


title : String
title =
    "# True Form - Race"


intro : String
intro =
    """
    "This is my favorite part, so don’t zone out on me: Even if most people prefer to be a neutral it’s fun to see what other option or two a person might have. Let’s see... oh, oh. ooh" The witch shudders with quite the enthusiasm, and continues "_This is ten times_ the most options I’ve seen in another witch. _*You have so many possibilities*_! Let’s explore them... I haven’t even seen some of these before, in person _or_ in an awakening ritual. I’m a fan of #7 in particular, they’re so cute." This is the physical expression of your True Form, what you would revert to if any effect were to reveal the true nature of a shapechanger for example. Your old body was just a temporary vessel for this true form. In your change, {choice you can totally remake your body within human limits}. However, witches are female by default, but you can take the perk _Elephant Trunk_ to be male if you wish, or the _Transformation Sequence_ perk to transform into your past human vessel and back. Note with the options presented you can become another non-human species as a Witch of that species, ie; A witch can be a naiad but not all naiads are witches. Every different race of witch has a different source of Mana as well as some unique benefits and base abilities. All witches except Neutral and partially Daeva share some degree of _inherent_ life extension, though there are many other methods any witch can learn or obtain later on regardless of their race. {choice *The blue potion icon represents Mana, the yellow bolt icon represents Charge rate. _This uses a Relative system and not absolute value, don’t overthink it_*}, it’s a representation of what you can expect in comparing your witch race to an average of other witches _of your same general power level_. High mana represents high storage, & charge represents the time it takes your method to fill your mana stores though it can often scale with overexposure, and stack.

    This is very simply just to give you a suggestion for perspective on how much magic you could expect to use. High Mana means a bigger tank, but charge is how fast it is filled. So high tank might be good for going all in while a low tank but high charge might have the endurance to go for longer. This is roughly balanced by how easy the charge source is to come by and how well it can stack up on itself.
    """
