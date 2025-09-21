module Data.Costs.Magic exposing (value)

import Data.Affinity as Affinity exposing (AffinityList, InAffinity(..))
import Data.Costs.Monad as Monad exposing (Monad)
import Data.Costs.Utils as Utils exposing (Points, affinityDiscountIf)
import Data.Magic as Magic
import Dict exposing (Dict)
import Generated.Magic
import Generated.Types as Types exposing (Class(..), Faction, Magic(..), Perk(..), Race(..))
import List.Extra
import Types exposing (CosmicPearlData, RankedMagic, RankedPerk)


value :
    { ignoreSorceressBonus : Bool }
    ->
        { a
            | cosmicPearl : CosmicPearlData
            , mainRace : Maybe Race
            , races : List Race
            , perks : List RankedPerk
            , faction : Maybe ( Faction, Bool )
            , class : Maybe Class
            , typePerks : List Race
            , magic : List RankedMagic
            , capBuild : Bool
        }
    -> Monad Points
value { ignoreSorceressBonus } model =
    let
        affinities : AffinityList
        affinities =
            Affinity.fromModel model
    in
    Generated.Magic.all
        |> List.sortBy
            (\magic ->
                if magic.faction /= Nothing then
                    2

                else if magic.isElementalism then
                    1

                else
                    0
            )
        |> List.filterMap (magicValue model affinities)
        |> Monad.combine
        |> Monad.andThen
            (\pointsList ->
                let
                    free : Dict String String
                    free =
                        case ( model.class, ignoreSorceressBonus, model.capBuild ) of
                            ( Just ClassSorceress, False, _ ) ->
                                case
                                    pointsList
                                        |> List.filter (\{ isElementalism, inAffinity } -> isElementalism && inAffinity /= OffAffinity)
                                        |> List.Extra.minimumBy .points
                                of
                                    Just magic ->
                                        Dict.singleton magic.name "[Sorceress]"

                                    Nothing ->
                                        Dict.empty

                            ( Just ClassAcademic, _, True ) ->
                                pointsList
                                    |> List.sortBy .points
                                    |> List.take 2
                                    |> List.map (\magic -> ( magic.name, "[Academic]" ))
                                    |> Dict.fromList

                            _ ->
                                Dict.empty

                    jackOfAllWarning : Maybe String
                    jackOfAllWarning =
                        if List.any (\p -> p.name == PerkJackOfAll) model.perks then
                            case
                                List.filterMap
                                    (\m ->
                                        if m.rank == 5 then
                                            Just (Types.magicToString m.name)

                                        else
                                            Nothing
                                    )
                                    model.magic
                            of
                                [] ->
                                    Nothing

                                forbidden ->
                                    Just ("If you have Jack-of-All you can't have rank 5 magic - you have selected " ++ String.join ", " forbidden)

                        else
                            Nothing

                    offAffinityWarning : Maybe String
                    offAffinityWarning =
                        if model.class == Just ClassSorceress then
                            Nothing

                        else
                            case List.filter (\magic -> magic.isElementalism && magic.inAffinity == OffAffinity) pointsList of
                                [] ->
                                    Nothing

                                [ _ ] ->
                                    Nothing

                                list ->
                                    Just ("Multiple off-affinity elementalism magics are only allowed for Sorceresses. Found: " ++ String.join ", " (List.map .name list))
                in
                pointsList
                    |> Monad.mapAndSum
                        (\{ name, rank, points } ->
                            let
                                label : String
                                label =
                                    name ++ " " ++ String.fromInt rank
                            in
                            case Dict.get name free of
                                Just reason ->
                                    0
                                        |> Monad.succeed
                                        |> Monad.withInfo
                                            { label = label
                                            , anchor = Just name
                                            , value = Monad.FreeBecause reason
                                            }

                                Nothing ->
                                    points
                                        |> Monad.succeed
                                        |> Monad.withInfo
                                            { label = label
                                            , anchor = Just name
                                            , value = Monad.Power points
                                            }
                        )
                    |> Monad.map Utils.powerToPoints
                    |> Monad.withWarningMaybe offAffinityWarning
                    |> Monad.withWarningMaybe jackOfAllWarning
            )


magicValue :
    { a
        | faction : Maybe ( Faction, Bool )
        , class : Maybe Class
        , typePerks : List Race
        , magic : List RankedMagic
        , races : List Race
    }
    -> Affinity.AffinityList
    -> Magic.Details
    ->
        Maybe
            (Monad
                { name : String
                , rank : Int
                , points : Int
                , isElementalism : Bool
                , inAffinity : InAffinity
                }
            )
magicValue model affinities magicDetails =
    model.magic
        |> List.Extra.find (\rankedMagic -> magicDetails.name == rankedMagic.name)
        |> Maybe.map
            (\rankedMagic ->
                let
                    name : String
                    name =
                        Types.magicToString rankedMagic.name

                    inAffinity : Affinity.InAffinity
                    inAffinity =
                        Affinity.isInAffinity magicDetails.affinities affinities

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
                (-- Genies all have rank 2 in every core & faction magic, and Prestidigitation & Conjuration free
                 if isGenie && magicDetails.dlc == Nothing && rankedMagic.rank <= 2 then
                    Monad.succeed 0

                 else
                    let
                        inFaction : InFaction
                        inFaction =
                            isInFaction model magicDetails

                        inClass : Bool
                        inClass =
                            (magicDetails.class == model.class)
                                && (model.class /= Nothing)

                        raw : Int
                        raw =
                            List.range 1 rankedMagic.rank
                                |> List.map
                                    (\rank ->
                                        rank
                                            |> factionDiscountIf inFaction
                                            |> affinityDiscountIf inAffinity
                                    )
                                |> List.sum
                                |> classDiscountIf inClass
                    in
                    if isGenie && magicDetails.dlc == Nothing then
                        Monad.succeed raw
                            |> Monad.withWarning "Free rank 2 magic for Genie half-implemented"

                    else
                        Monad.succeed raw
                )
                    |> Monad.map
                        (\finalCost ->
                            { name = name
                            , rank = rankedMagic.rank
                            , points = -finalCost
                            , isElementalism = magicDetails.isElementalism
                            , inAffinity = inAffinity
                            }
                        )
            )


type InFaction
    = InFactionPerk
    | InFactionNoPerk
    | OutOfFaction
    | Nonfactional


isInFaction :
    { a
        | faction : Maybe ( Faction, Bool )
        , typePerks : List Race
    }
    -> Magic.Details
    -> InFaction
isInFaction { faction, typePerks } magicDetails =
    if
        (List.member RaceSpider typePerks && magicDetails.name == MagicArachnescence)
            || (List.member RaceCyborg typePerks && magicDetails.name == MagicGadgetry)
            || (List.member RaceCyborg typePerks && magicDetails.name == MagicIntegration)
    then
        InFactionPerk

    else
        case magicDetails.faction of
            Just magicFaction ->
                if Just ( magicFaction, True ) == faction then
                    InFactionPerk

                else if Just ( magicFaction, False ) == faction then
                    InFactionNoPerk

                else
                    OutOfFaction

            Nothing ->
                Nonfactional


classDiscountIf : Bool -> Int -> Int
classDiscountIf inClass cost =
    if inClass && cost > 0 then
        cost - 2

    else
        cost


factionDiscountIf : InFaction -> Int -> Int
factionDiscountIf factionality cost =
    case factionality of
        OutOfFaction ->
            if cost > 0 then
                cost * 2

            else
                cost

        Nonfactional ->
            cost

        InFactionNoPerk ->
            cost

        InFactionPerk ->
            if cost > 0 then
                (cost + 1) // 2

            else
                cost
