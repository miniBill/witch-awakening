module Data.Perk exposing (Content(..), Details, hybridizeCost, weird)

import Generated.Types exposing (Affinity(..), Class(..), Perk(..), Race(..))
import List.Extra
import Types exposing (RankedPerk)


type alias Details =
    { name : Perk
    , class : Class
    , requires : Maybe String
    , affinity : Affinity
    , isMeta : Bool
    , content : Content
    , dlc : Maybe String
    }


type Content
    = Single Int String
    | WithChoices String (List ( String, Int )) String
    | WithCosts (List Int) String
    | WithChoicesChargeSwap String (List ( String, Int ))


weird : List RankedPerk -> List Details
weird perks =
    [ chargeSwap perks ]


hybridizeCost : number
hybridizeCost =
    6


chargeSwap : List RankedPerk -> Details
chargeSwap perks =
    let
        name : Perk
        name =
            List.Extra.findMap
                (\rp ->
                    case rp.name of
                        PerkChargeSwap _ ->
                            Just rp.name

                        _ ->
                            Nothing
                )
                perks
                |> Maybe.withDefault (PerkChargeSwap RaceNeutral)
    in
    { name = name
    , class = ClassWarlock
    , requires = Nothing
    , affinity = AffinitySoul
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
