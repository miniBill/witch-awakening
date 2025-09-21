module Data.Costs.Perks exposing (perkValue, value)

import Data.Affinity as Affinity exposing (AffinityList, InAffinity)
import Data.Costs.Monad as Monad exposing (Monad)
import Data.Costs.Utils as Utils exposing (Points)
import Data.Magic as Magic
import Data.Perk as Perk
import Generated.Perk
import Generated.Types as Types exposing (Class, Perk(..), Race(..))
import List.Extra
import Types exposing (CosmicPearlData, RankedPerk)
import View.Perk


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
        |> Monad.combineMap (perkValue model)
        |> Monad.andThen
            (\pointsList ->
                let
                    free : Maybe String
                    free =
                        if List.any (\p -> p.name == PerkJackOfAll) model.perks then
                            pointsList
                                |> List.filter (\{ staticCost } -> staticCost)
                                |> List.Extra.minimumBy .points
                                |> Maybe.map .name

                        else
                            Nothing
                in
                pointsList
                    |> Monad.mapAndSum
                        (\{ name, points } ->
                            if Just name == free then
                                0
                                    |> Monad.succeed
                                    |> Monad.withInfo
                                        { label = name
                                        , anchor = Just name
                                        , value = Monad.FreeBecause "[Jack-of-All]"
                                        }

                            else
                                points
                                    |> Monad.succeed
                                    |> Monad.withPowerInfo name
                        )
            )
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
    -> Monad { name : String, points : Int, staticCost : Bool }
perkValue ({ class } as model) { name, cost } =
    Utils.find "Perk" .name name (Generated.Perk.all model.perks) View.Perk.perkToShortString
        |> Monad.map
            (\perk ->
                let
                    affinities : AffinityList
                    affinities =
                        Affinity.fromModel model

                    isClass : Bool
                    isClass =
                        Just perk.class == class

                    isInAffinity : InAffinity
                    isInAffinity =
                        Affinity.isInAffinity (Magic.Regular [ perk.affinity ]) affinities

                    changelingDiff : Int
                    changelingDiff =
                        case name of
                            PerkChargeSwap _ ->
                                if List.member RaceChangeling model.races then
                                    -3

                                else
                                    0

                            _ ->
                                0

                    apexDiff : Int
                    apexDiff =
                        case name of
                            PerkApex ->
                                if List.any (\p -> p.name == PerkHybridize) model.perks then
                                    3 * (List.length model.races - 1)

                                else
                                    0

                            _ ->
                                0

                    finalCost : Int
                    finalCost =
                        (cost + changelingDiff + apexDiff)
                            |> Utils.applyClassBonusIf isClass
                            |> Utils.affinityDiscountIf isInAffinity
                in
                { name = Types.perkToString name
                , points = -finalCost
                , staticCost =
                    case perk.content of
                        Perk.Single _ _ ->
                            True

                        _ ->
                            False
                }
            )
