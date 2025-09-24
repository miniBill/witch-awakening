module Data.Costs.Perks exposing (perkValue, value)

import Data.Affinity as Affinity exposing (AffinityList, InAffinity)
import Data.Costs.Monad as Monad exposing (Monad)
import Data.Costs.Utils as Utils exposing (Points)
import Data.Magic as Magic
import Data.Perk as Perk
import Generated.Magic
import Generated.Perk
import Generated.Types as Types exposing (Class, Magic, Perk(..), Race(..))
import List.Extra
import Parser exposing ((|.), (|=), Parser)
import Types exposing (CosmicPearlData, RankedMagic, RankedPerk)
import View.Perk


value :
    { a
        | races : List Race
        , mainRace : Maybe Race
        , cosmicPearl : CosmicPearlData
        , typePerks : List Race
        , perks : List RankedPerk
        , class : Maybe Class
        , magic : List RankedMagic
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
        , magic : List RankedMagic
    }
    -> RankedPerk
    -> Monad { name : String, points : Int, staticCost : Bool }
perkValue model ranked =
    let
        isGenie : Bool
        isGenie =
            List.any
                (\race ->
                    case race of
                        RaceGenie _ ->
                            True

                        _ ->
                            False
                )
                model.races
    in
    Utils.find "Perk" .name ranked.name (Generated.Perk.all model.perks) View.Perk.perkToShortString
        |> Monad.andThen
            (\perk ->
                let
                    finalCost : Int
                    finalCost =
                        if isGenie && (ranked.name == PerkPrestidigitation || ranked.name == PerkConjuration) then
                            0

                        else
                            innerPerkValue model ranked perk

                    res : { name : String, points : Int, staticCost : Bool }
                    res =
                        { name = Types.perkToString ranked.name
                        , points = -finalCost
                        , staticCost =
                            case perk.content of
                                Perk.Single _ _ ->
                                    True

                                _ ->
                                    False
                        }
                in
                case perk.requires of
                    Nothing ->
                        Monad.succeed res

                    Just req ->
                        case Parser.run (requisiteParser |. Parser.end) req of
                            Err _ ->
                                Monad.succeed res |> Monad.withWarning ("Failed to parse requisite: " ++ req)

                            Ok (RequiresMagic requiredName requiredRank) ->
                                if
                                    List.any
                                        (\rankedMagic ->
                                            rankedMagic.name == requiredName && rankedMagic.rank >= requiredRank
                                        )
                                        model.magic
                                then
                                    Monad.succeed res

                                else
                                    Monad.succeed res
                                        |> Monad.withWarning
                                            ("Missing requisite for "
                                                ++ Types.perkToString ranked.name
                                                ++ ": "
                                                ++ req
                                            )
            )


requisiteParser : Parser Requisite
requisiteParser =
    let
        name : Parser Magic
        name =
            Generated.Magic.all
                |> List.map (\m -> Parser.succeed m.name |. Parser.keyword (Types.magicToString m.name))
                |> Parser.oneOf
    in
    Parser.succeed RequiresMagic
        |= name
        |. Parser.spaces
        |= Parser.int
        |. Parser.oneOf
            [ Parser.succeed () |. Parser.symbol "+"
            , Parser.succeed ()
            ]


type Requisite
    = RequiresMagic Magic Int


innerPerkValue :
    { a
        | class : Maybe Class
        , races : List Race
        , mainRace : Maybe Race
        , cosmicPearl : CosmicPearlData
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
