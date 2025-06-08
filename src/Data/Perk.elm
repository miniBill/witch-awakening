module Data.Perk exposing (Content(..), Details, all, hybridizeCost, intro)

import Generated.Types exposing (Affinity(..), Class(..), Perk(..), Race(..))
import List.Extra
import Types exposing (RankedPerk)


type alias Details =
    { name : Perk
    , class : Class
    , affinity : Affinity
    , isMeta : Bool
    , content : Content
    , dlc : Maybe String
    }


type Content
    = Single Int String
    | WithChoices String (List ( String, Int )) String
    | WithCosts (List Int) String
    | WithChoicesHybridize String (List ( String, Int ))
    | WithChoicesChargeSwap String (List ( String, Int ))


intro : String
intro =
    """
    Perks are an assortment of bonus traits ranging from added benefits of your new witch true form, to special magical abilities you learn or manifest in your transformation that differ from anything already shown. {choice *Perks cost POWER as shown in the corner. Like Magic, Perks have half the cost if you have the Affinity shown in the other corner. [???] is universally halved (Not pre-factored).*}
    """


all : List RankedPerk -> List Details
all perks =
    [ hybridize, chargeSwap perks ]


hybridize : Details
hybridize =
    { name = Hybridize
    , class = Sorceress
    , affinity = Beast
    , isMeta = False
    , dlc = Nothing
    , content =
        WithChoicesHybridize """
        Choose a second racial type for your true form, and reasonably combine the physical look of both races, take the higher Mana capacity, and maintain both methods of charge (each of which charge as fast as they normally would) and any features mentioned in their description, such as the breath of the Draviri or the honey of the Sprite. You can take both type perks of either race. You do not gain affinity of the second type. See _Cosmic Pearl_. You can take this twice for up to 3 aspects.

        *This is added/removed automatically when you select your race[s]*
        """
            [ ( "Taken once - 2 races (_hybrid_)", hybridizeCost )
            , ( "Taken twice - 3 races (_chimera_)", hybridizeCost * 2 )
            ]
    }


hybridizeCost : number
hybridizeCost =
    6


chargeSwap : List RankedPerk -> Details
chargeSwap perks =
    let
        race : Race
        race =
            List.Extra.findMap
                (\{ name } ->
                    case name of
                        ChargeSwap r ->
                            Just r

                        _ ->
                            Nothing
                )
                perks
                |> Maybe.withDefault Neutral
    in
    { name = ChargeSwap race
    , class = Warlock
    , affinity = Soul
    , isMeta = False
    , dlc = Nothing
    , content =
        WithChoicesChargeSwap
            """
            Replace your Charge method with the method of another race, unless the chosen charge type would not be possible without an integral aspect of that race. ie; Can’t take the Aurai charge type if you don’t have the Aurai paradox voice, or a Gorgon’s charge without their petrify. This changes your charge rate to match, as it’s inherent to the method, not the type.
            """
            [ ( "This is the basic version and costs 4 power", 4 )
            , ( "_For an extra 6 power_, you instead _gain_ the desired charge method as an _additional_ charge method. They can both be providing mana gain at the same time, at their individual rates", 10 )
            ]
    }
