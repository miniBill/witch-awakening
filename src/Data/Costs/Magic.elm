module Data.Costs.Magic exposing (value)

import Data.Affinity as Affinity
import Data.Costs.Monad as Monad exposing (Monad)
import Data.Costs.Utils as Utils exposing (Points)
import Data.Magic as Magic
import Dict exposing (Dict)
import Generated.Magic
import Generated.Types as Types exposing (Affinity, Class(..), Faction, Magic(..), Perk(..), Race(..))
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
        affinities : List Affinity
        affinities =
            Affinity.fromModel model

        pointsList :
            List
                { name : String
                , rank : Int
                , points : Int
                , isElementalism : Bool
                , isOffAffinity : Bool
                }
        pointsList =
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

        free : Dict String String
        free =
            case ( model.class, ignoreSorceressBonus, model.capBuild ) of
                ( Just ClassSorceress, False, _ ) ->
                    case
                        pointsList
                            |> List.filter (\{ isElementalism, isOffAffinity } -> isElementalism && not isOffAffinity)
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
                case List.filter (\magic -> magic.isElementalism && magic.isOffAffinity) pointsList of
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


magicValue :
    { a
        | faction : Maybe ( Faction, Bool )
        , class : Maybe Class
        , typePerks : List Race
        , magic : List RankedMagic
    }
    -> List Affinity
    -> Magic.Details
    ->
        Maybe
            { name : String
            , rank : Int
            , points : Int
            , isElementalism : Bool
            , isOffAffinity : Bool
            }
magicValue model affinities magicDetails =
    model.magic
        |> List.Extra.find (\rankedMagic -> magicDetails.name == rankedMagic.name)
        |> Maybe.map
            (\rankedMagic ->
                let
                    inAffinity : Bool
                    inAffinity =
                        isInAffinity magicDetails affinities

                    inFaction : InFaction
                    inFaction =
                        isInFaction model magicDetails

                    inClass : Bool
                    inClass =
                        (magicDetails.class == model.class)
                            && (model.class /= Nothing)

                    finalCost : Int
                    finalCost =
                        List.range 1 rankedMagic.rank
                            |> List.map
                                (\rank ->
                                    rank
                                        |> factionDiscountIf inFaction
                                        |> affinityDiscountIf inAffinity
                                )
                            |> List.sum
                            |> classDiscountIf inClass

                    name : String
                    name =
                        Types.magicToString rankedMagic.name
                in
                { name = name
                , rank = rankedMagic.rank
                , points = -finalCost
                , isElementalism = magicDetails.isElementalism
                , isOffAffinity = not (isInAffinity magicDetails affinities)
                }
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


affinityDiscountIf : Bool -> Int -> Int
affinityDiscountIf inAffinity cost =
    if inAffinity && cost > 0 then
        (cost + 1) // 2

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


isInAffinity : Magic.Details -> List Affinity -> Bool
isInAffinity magic affinities =
    case magic.affinities of
        Magic.Regular regular ->
            List.any
                (\affinity -> List.member affinity affinities)
                regular

        Magic.Alternative alternatives ->
            alternatives
                |> List.any
                    (\alternative ->
                        List.all
                            (\affinity -> List.member affinity affinities)
                            alternative
                    )
