module Data.Costs.Perks exposing (perkValue, value)

import Data.Affinity as Affinity
import Data.Costs.Monad as Monad exposing (Monad)
import Data.Costs.Utils as Utils exposing (Points)
import Generated.Perk
import Generated.Types as Types exposing (Affinity, Class, Perk(..), Race(..))
import Types exposing (CosmicPearlData, RankedPerk)


value :
    { a
        | races : List Race
        , mainRace : Maybe Race
        , cosmicPearl : CosmicPearlData
        , typePerks : List Race
        , perks : List RankedPerk
        , class : Maybe Class
    }
    -> Monad Points
value model =
    model.perks
        |> Monad.mapAndSum (perkValue model)
        |> Monad.map Utils.powerToPoints


perkValue :
    { a
        | class : Maybe Class
        , races : List Race
        , mainRace : Maybe Race
        , cosmicPearl : CosmicPearlData
        , typePerks : List Race
        , perks : List RankedPerk
    }
    -> RankedPerk
    -> Monad Int
perkValue ({ class } as model) { name, cost } =
    Utils.find "Perk" .name name (Generated.Perk.all model.perks) Types.perkToString
        |> Monad.map
            (\perk ->
                let
                    affinities : List Affinity
                    affinities =
                        Affinity.fromModel model

                    isClass : Bool
                    isClass =
                        Just perk.class == class

                    isInAffinity : Bool
                    isInAffinity =
                        List.member perk.affinity affinities

                    changelingDiff : Int
                    changelingDiff =
                        case name of
                            ChargeSwap _ ->
                                if List.member Changeling model.races then
                                    -3

                                else
                                    0

                            _ ->
                                0

                    apexDiff : Int
                    apexDiff =
                        case name of
                            Apex ->
                                if List.any (\p -> p.name == Hybridize) model.perks then
                                    3 * (List.length model.races - 1)

                                else
                                    0

                            _ ->
                                0

                    finalCost : Int
                    finalCost =
                        (cost + changelingDiff + apexDiff)
                            |> Utils.applyClassBonusIf isClass
                            |> Utils.halveIfPositiveAnd isInAffinity
                in
                -finalCost
            )
        |> Monad.withPowerInfo (Types.perkToString name)
