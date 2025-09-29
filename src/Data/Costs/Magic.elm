module Data.Costs.Magic exposing (value)

import Data.Affinity as Affinity exposing (AffinityList, InAffinity(..))
import Data.Costs.Monad as Monad exposing (Monad)
import Data.Costs.Utils as Utils exposing (Points, affinityDiscountIf)
import Data.Magic as Magic
import Data.Race as Race
import Dict exposing (Dict)
import Generated.Magic
import Generated.TypePerk
import Generated.Types as Types exposing (Class(..), Faction(..), Magic(..), Perk(..), Race(..))
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
                    freeFromClass : Dict String String
                    freeFromClass =
                        case model.class of
                            Just ClassSorceress ->
                                if ignoreSorceressBonus then
                                    Dict.empty

                                else
                                    case
                                        pointsList
                                            |> List.filter (\{ isElementalism, inAffinity } -> isElementalism && inAffinity /= OffAffinity)
                                            |> List.Extra.minimumBy .power
                                    of
                                        Just magic ->
                                            Dict.singleton magic.name "[Sorceress]"

                                        Nothing ->
                                            Dict.empty

                            Just ClassAcademic ->
                                if model.capBuild then
                                    pointsList
                                        |> List.sortBy .power
                                        |> List.take 2
                                        |> List.map (\magic -> ( magic.name, "[Academic]" ))
                                        |> Dict.fromList

                                else
                                    Dict.empty

                            _ ->
                                Dict.empty

                    freeFromRace : Dict String String
                    freeFromRace =
                        pointsList
                            |> List.filterMap
                                (\magic ->
                                    magic.freeRankFromRace
                                        |> Maybe.andThen
                                            (\( freeRank, freeRace ) ->
                                                if magic.rank <= freeRank then
                                                    Just ( magic.name, "[" ++ Types.raceToString freeRace ++ "]" )

                                                else
                                                    Nothing
                                            )
                                )
                            |> Dict.fromList

                    free : Dict String String
                    free =
                        Dict.union freeFromClass freeFromRace

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
                                    Just ("If you have Jack-of-All you can’t have rank 5 magic - you have selected " ++ String.join ", " forbidden)

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
                    |> List.map
                        (\{ name, rank, power, rewardPoints } ->
                            let
                                label : String
                                label =
                                    name ++ " " ++ String.fromInt rank
                            in
                            case Dict.get name free of
                                Just reason ->
                                    { power = 0
                                    , rewardPoints = rewardPoints
                                    }
                                        |> Monad.succeed
                                        |> Monad.withInfo
                                            { label = label
                                            , anchor = Just name
                                            , value = Monad.FreeBecause reason
                                            }

                                Nothing ->
                                    { power = power
                                    , rewardPoints = rewardPoints
                                    }
                                        |> Monad.succeed
                                        |> Monad.withInfo
                                            { label = label
                                            , anchor = Just name
                                            , value = Monad.Power power
                                            }
                        )
                    |> Utils.combineAndSum
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
                , freeRankFromRace : Maybe ( Int, Race )
                , power : Int
                , rewardPoints : Int
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
                        if
                            (magicDetails.name == MagicTheHallowingEcho)
                                && (Maybe.map Tuple.first model.faction /= Just FactionTheOutsiders)
                        then
                            InAffinity

                        else
                            Affinity.isInAffinity magicDetails.affinities affinities

                    inFaction : InFaction
                    inFaction =
                        isInFaction model magicDetails

                    inClass : Bool
                    inClass =
                        case magicDetails.class of
                            Magic.ClassSpecial ->
                                magicDetails.name == MagicWishcasting && List.any Race.isGenie model.races

                            Magic.ClassOne c ->
                                model.class == Just c

                            Magic.ClassNone ->
                                False

                    freeRankFromRace : Maybe ( Int, Race )
                    freeRankFromRace =
                        freeRankFromRaceOrTypePerk model magicDetails rankedMagic

                    ( finalCost, rewardPoints ) =
                        List.range
                            (case freeRankFromRace of
                                Nothing ->
                                    if magicDetails.name == MagicAdvancedGolemancy then
                                        let
                                            ranksIn : Magic -> Int
                                            ranksIn magic =
                                                model.magic
                                                    |> List.Extra.findMap
                                                        (\r ->
                                                            if r.name == magic then
                                                                Just r.rank

                                                            else
                                                                Nothing
                                                        )
                                                    |> Maybe.withDefault 0

                                            ranksInHexes : Int
                                            ranksInHexes =
                                                ranksIn MagicHexes

                                            ranksInRunes : Int
                                            ranksInRunes =
                                                ranksIn MagicRunes

                                            freeRanks : Int
                                            freeRanks =
                                                (ranksInHexes + ranksInRunes) // 3
                                        in
                                        1 + freeRanks

                                    else
                                        1

                                Just ( r, _ ) ->
                                    r + 1
                            )
                            rankedMagic.rank
                            |> List.map
                                (\rank ->
                                    ( rank
                                        |> factionDiscountIf inFaction
                                        |> affinityDiscountIf inAffinity
                                    , if rankedMagic.name == MagicBodyRefinement && rank >= 3 then
                                        5

                                      else
                                        0
                                    )
                                )
                            |> List.unzip
                            |> Tuple.mapBoth
                                (\ps ->
                                    ps
                                        |> List.sum
                                        |> classDiscountIf inClass
                                )
                                List.sum
                in
                { name = name
                , rank = rankedMagic.rank
                , freeRankFromRace = freeRankFromRace
                , power = -finalCost
                , rewardPoints = rewardPoints
                , isElementalism = magicDetails.isElementalism
                , inAffinity = inAffinity
                }
                    |> Monad.succeed
                    |> (if magicDetails.name == MagicWishcasting && not (List.any Race.isGenie model.races) then
                            Monad.withWarning "Only Genies can access Wishcasting"

                        else
                            identity
                       )
            )


freeRankFromRaceOrTypePerk :
    { a
        | faction : Maybe ( Faction, Bool )
        , class : Maybe Class
        , typePerks : List Race
        , magic : List RankedMagic
        , races : List Race
    }
    -> Magic.Details
    -> { name : Magic, rank : Int }
    -> Maybe ( Int, Race )
freeRankFromRaceOrTypePerk model magicDetails rankedMagic =
    let
        asGenie : Maybe Race
        asGenie =
            List.Extra.find Race.isGenie model.races
    in
    case
        asGenie
    of
        Just race ->
            if
                (magicDetails.dlc == Nothing)
                    || (magicDetails.faction /= Nothing && magicDetails.class /= Magic.ClassNone)
            then
                -- Genies all have rank 2 in every core & faction magic, and Prestidigitation & Conjuration free
                Just ( 2, race )

            else
                fromTypePerk model rankedMagic.name

        Nothing ->
            fromTypePerk model rankedMagic.name


fromTypePerk : { a | typePerks : List Race } -> Magic -> Maybe ( Int, Race )
fromTypePerk model magic =
    model.typePerks
        |> List.Extra.findMap
            (\race ->
                List.Extra.findMap
                    (\typePerk ->
                        if typePerk.race == race then
                            List.Extra.findMap
                                (\ranked ->
                                    if ranked.name == magic then
                                        Just ( ranked.rank, race )

                                    else
                                        Nothing
                                )
                                typePerk.gain

                        else
                            Nothing
                    )
                    Generated.TypePerk.all
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
