module Data.Costs.Perks exposing (perkValue, value)

import Data.Affinity as Affinity exposing (AffinityList, InAffinity)
import Data.Costs.Monad as Monad exposing (Monad)
import Data.Costs.Points exposing (Points)
import Data.Costs.Utils as Utils
import Data.Costs.Value as Value exposing (Value)
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
                    freeFromJackOfAll : Maybe String
                    freeFromJackOfAll =
                        if List.any (\p -> p.name == PerkJackOfAll) model.perks then
                            pointsList
                                |> List.filter (\{ staticCost } -> staticCost)
                                |> List.Extra.maximumBy
                                    (\item ->
                                        case item.perkValue of
                                            Value.PowerAndRewardPoints p ->
                                                p.power + p.rewardPoints

                                            Value.FreeBecause _ ->
                                                -1
                                    )
                                |> Maybe.map .name

                        else
                            Nothing
                in
                pointsList
                    |> Monad.combineMapAndSum
                        (\item ->
                            let
                                maybeFree : Value
                                maybeFree =
                                    if Just item.name == freeFromJackOfAll then
                                        Value.FreeBecause "[Jack-of-All]"

                                    else
                                        item.perkValue
                            in
                            maybeFree
                                |> Value.toPoints
                                |> Monad.succeed
                                |> Monad.withInfo
                                    { label = item.name
                                    , kind = IdKindPerk
                                    , anchor = Just item.name
                                    , value = maybeFree
                                    }
                        )
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
    -> Monad { name : String, perkValue : Value, staticCost : Bool }
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

                    finalValue : Value
                    finalValue =
                        case isFree of
                            Just reason ->
                                Value.FreeBecause reason

                            Nothing ->
                                Value.fromPower (innerPerkValue model ranked perk)

                    res : { name : String, perkValue : Value, staticCost : Bool }
                    res =
                        { name = View.Perk.perkToShortString ranked.name
                        , perkValue = finalValue
                        , staticCost =
                            case perk.content of
                                Perk.Single _ _ ->
                                    True

                                _ ->
                                    False
                        }
                in
                Utils.checkRequirements perk (View.Perk.perkToShortString ranked.name) model res
            )


innerPerkValue :
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
innerPerkValue ({ class } as model) { name, cost } perk =
    let
        apexDiff : Int
        apexDiff =
            case name of
                PerkApex ->
                    if List.any (\p -> p.name == PerkHybridize) model.perks then
                        -3 * (List.length model.races - 1)

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

        perkAffinities : Magic.Affinities
        perkAffinities =
            if perk.isMeta then
                Magic.Regular (Types.AffinityMeta :: perk.affinity)

            else
                Magic.Regular perk.affinity

        isInAffinity : InAffinity
        isInAffinity =
            Affinity.isInAffinity perkAffinities affinities

        changelingDiff : Int
        changelingDiff =
            case name of
                PerkChargeSwap _ ->
                    if List.member RaceChangeling model.races then
                        3

                    else
                        0

                _ ->
                    0
    in
    (-cost + changelingDiff + apexDiff)
        |> Utils.applyClassBonusIf isClass
        |> Utils.affinityDiscountIf isInAffinity
