module Data.Race exposing (Details, intro, title, withVariantAffinity1, withVariantAffinity2)

import Generated.Types exposing (Affinity(..), Race, Size)
import List.Extra


type alias Details =
    { name : Race
    , affinities : List Affinity
    , tank : Size
    , charge : Size
    , content : String
    , dlc : Maybe String
    }


withVariantAffinity1 :
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
withVariantAffinity1 match details races =
    let
        affinity : Affinity
        affinity =
            List.Extra.findMap match races
                |> Maybe.withDefault All
    in
    { name = details.name affinity
    , tank = details.tank
    , affinities = details.affinities ++ [ affinity ]
    , charge = details.charge
    , dlc = details.dlc
    , content = details.content
    }


withVariantAffinity2 :
    (Race -> Maybe ( Affinity, Affinity ))
    ->
        { name : Affinity -> Affinity -> Race
        , tank : Size
        , affinities : List Affinity
        , charge : Size
        , dlc : Maybe String
        , content : String
        }
    -> List Race
    -> Details
withVariantAffinity2 match details races =
    let
        ( aff1, aff2 ) =
            List.Extra.findMap match races
                |> Maybe.withDefault ( All, All )
    in
    { name = details.name aff1 aff2
    , tank = details.tank
    , affinities = [ aff1, aff2 ]
    , charge = details.charge
    , dlc = details.dlc
    , content = details.content
    }


title : String
title =
    "# True Form - Race"


intro : String
intro =
    """
    "This is my favorite part, so don’t zone out on me: Even if most people prefer to be a neutral it’s fun to see what other option or two a person might have. Let’s see... oh, oh. ooh" The witch shudders with quite the enthusiasm, and continues "_This is ten times_ the most options I’ve seen in another witch. _*You have so many possibilities*_! Let’s explore them... I haven’t even seen some of these before, in person _or_ in an awakening ritual. I’m a fan of #7 in particular, they’re so cute." This is the physical expression of your True Form, what you would revert to if any effect were to reveal the true nature of a shapechanger for example. Your old body was just a temporary vessel for this true form. In your change, {choice you can totally remake your body within human limits}. However, witches are female by default, but you can take the perk _Elephant Trunk_ to be male if you wish, or the _Transformation Sequence_ perk to transform into your past human vessel and back. Note with the options presented you can become another non-human species as a Witch of that species, ie; A witch can be a naiad but not all naiads are witches. Every different race of witch has a different source of Mana as well as some unique benefits and base abilities. All witches except Neutral and partially Daeva share some degree of _inherent_ life extension, though there are many other methods any witch can learn or obtain later on regardless of their race. {choice *The blue potion icon represents Mana, the yellow bolt icon represents Charge rate. _This uses a Relative system and not absolute value, don’t overthink it_*}, it’s a representation of what you can expect in comparing your witch race to an average of other witches _of your same general power level_. High mana represents high storage, & charge represents the time it takes your method to fill your mana stores though it can often scale with overexposure, and stack.

    This is very simply just to give you a suggestion for perspective on how much magic you could expect to use. High Mana means a bigger tank, but charge is how fast it is filled. So high tank might be good for going all in while a low tank but high charge might have the endurance to go for longer. This is roughly balanced by how easy the charge source is to come by and how well it can stack up on itself.
    """
