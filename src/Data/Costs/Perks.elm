module Data.Costs.Perks exposing (perkValue, value)

import Data.Affinity as Affinity exposing (AffinityList, InAffinity)
import Data.Costs.Monad as Monad exposing (Monad)
import Data.Costs.Utils as Utils exposing (Points)
import Data.Magic as Magic
import Data.Perk as Perk
import Data.Race as Race
import Generated.Magic as Magic
import Generated.Perk as Perk
import Generated.Types as Types exposing (Class, Magic(..), Perk(..), Quest, Race(..))
import List.Extra
import Types exposing (IdKind(..), RankedMagic, RankedPerk, RankedRelic)
import View.Perk


value :
    { a
        | races : List Race
        , mainRace : Maybe Race
        , relics : List RankedRelic
        , typePerks : List Race
        , perks : List RankedPerk
        , class : Maybe Class
        , magic : List RankedMagic
        , quests : List Quest
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
                                |> List.Extra.maximumBy
                                    (\{ points } ->
                                        case points of
                                            Monad.Power p ->
                                                p

                                            Monad.RewardPoints p ->
                                                p

                                            Monad.FreeBecause _ ->
                                                -1
                                    )
                                |> Maybe.map .name

                        else
                            Nothing
                in
                pointsList
                    |> Monad.combineMap
                        (\{ name, points } ->
                            let
                                ( v, raw ) =
                                    if Just name == free then
                                        ( Monad.FreeBecause "[Jack-of-All]", Utils.powerToPoints 0 )

                                    else
                                        case points of
                                            Monad.Power p ->
                                                ( points, Utils.powerToPoints p )

                                            Monad.RewardPoints p ->
                                                ( points, Utils.rewardPointsToPoints p )

                                            Monad.FreeBecause _ ->
                                                ( points, Utils.powerToPoints 0 )
                            in
                            raw
                                |> Monad.succeed
                                |> Monad.withInfo
                                    { label = name
                                    , kind = IdKindPerk
                                    , anchor = Just name
                                    , value = v
                                    }
                        )
                    |> Monad.map Utils.sumPoints
            )


perkValue :
    { a
        | class : Maybe Class
        , races : List Race
        , mainRace : Maybe Race
        , relics : List RankedRelic
        , typePerks : List Race
        , perks : List RankedPerk
        , magic : List RankedMagic
        , quests : List Quest
    }
    -> RankedPerk
    -> Monad { name : String, points : Monad.Value, staticCost : Bool }
perkValue model ranked =
    let
        isGenie : Maybe String
        isGenie =
            if List.any Race.isGenie model.races then
                Just "[Genie]"

            else
                Nothing
    in
    Utils.find "Perk" .name ranked.name (Perk.all model.perks) View.Perk.perkToShortString
        |> Monad.andThen
            (\perk ->
                let
                    freeIfHasMagicAtRank : Types.Magic -> Int -> Maybe String
                    freeIfHasMagicAtRank magic rank =
                        if Utils.hasMagicAtRank model magic rank then
                            Just ("[" ++ Magic.toString magic ++ "]")

                        else
                            Nothing

                    isFree : Maybe String
                    isFree =
                        case ranked.name of
                            PerkPrestidigitation ->
                                isGenie

                            PerkConjuration ->
                                isGenie

                            PerkFullSteamAhead ->
                                freeIfHasMagicAtRank MagicWaterworking 3

                            _ ->
                                Nothing

                    finalCost : Monad.Value
                    finalCost =
                        case isFree of
                            Just reason ->
                                Monad.FreeBecause reason

                            Nothing ->
                                Monad.Power -(innerPerkCost model ranked perk)

                    res : { name : String, points : Monad.Value, staticCost : Bool }
                    res =
                        { name = Perk.toString ranked.name
                        , points = finalCost
                        , staticCost =
                            case perk.content of
                                Perk.Single _ _ ->
                                    True

                                _ ->
                                    False
                        }
                in
                Utils.checkRequirements perk (Perk.toString ranked.name) model res
            )


innerPerkCost :
    { a
        | class : Maybe Class
        , races : List Race
        , mainRace : Maybe Race
        , relics : List RankedRelic
        , typePerks : List Race
        , perks : List RankedPerk
    }
    -> { name : Perk, cost : Int }
    -> Perk.Details
    -> Int
innerPerkCost ({ class } as model) { name, cost } perk =
    let
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
    in
    (cost + changelingDiff + apexDiff)
        |> Utils.applyClassBonusIf isClass
        |> Utils.affinityDiscountIf isInAffinity
