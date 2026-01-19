module Data.Costs.Magic exposing (InFaction, value)

import Data.Affinity as Affinity exposing (AffinityList, InAffinity(..))
import Data.Costs.Monad as Monad exposing (Monad)
import Data.Costs.Points exposing (Points)
import Data.Costs.Utils as Utils exposing (affinityValueDiscountIf)
import Data.Costs.Value as Value
import Data.Magic as Magic
import Data.Race as Race
import Dict exposing (Dict)
import Generated.Magic as Magic
import Generated.Race as Race
import Generated.TypePerk as TypePerk
import Generated.Types exposing (Class(..), Faction(..), Magic(..), Perk(..), Quest, Race(..))
import List.Extra
import Types exposing (IdKind(..), RankedMagic, RankedPerk, RankedRelic)


value :
    { ignoreSorceressBonus : Bool }
    ->
        { a
            | mainRace : Maybe Race
            , races : List Race
            , perks : List RankedPerk
            , factions : List Faction
            , factionPerks : List Faction
            , class : Maybe Class
            , typePerks : List Race
            , magic : List RankedMagic
            , capBuild : Bool
            , quests : List Quest
            , relics : List RankedRelic
        }
    -> Monad Points
value { ignoreSorceressBonus } model =
    let
        affinities : AffinityList
        affinities =
            Affinity.fromModel model
    in
    Magic.all
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
                                                    Just ( magic.name, "[" ++ Race.toString freeRace ++ "]" )

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
                                            Just (Magic.toString m.name)

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
                    |> Monad.combineMapAndSum
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
                                            , kind = IdKindMagic
                                            , anchor = Just name
                                            , value = Value.FreeBecause reason
                                            }

                                Nothing ->
                                    let
                                        points : Points
                                        points =
                                            { power = power
                                            , rewardPoints = rewardPoints
                                            }
                                    in
                                    points
                                        |> Monad.succeed
                                        |> Monad.withInfo
                                            { label = label
                                            , kind = IdKindMagic
                                            , anchor = Just name
                                            , value = Value.PowerAndRewardPoints points
                                            }
                        )
                    |> Monad.withWarningMaybe offAffinityWarning
                    |> Monad.withWarningMaybe jackOfAllWarning
            )


magicValue :
    { a
        | factions : List Faction
        , factionPerks : List Faction
        , class : Maybe Class
        , typePerks : List Race
        , magic : List RankedMagic
        , races : List Race
        , quests : List Quest
        , perks : List RankedPerk
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
                        Magic.toString rankedMagic.name

                    inAffinity : Affinity.InAffinity
                    inAffinity =
                        if
                            (magicDetails.name == MagicTheHallowingEcho)
                                && not (List.member FactionTheOutsiders model.factions)
                        then
                            InAffinity

                        else
                            Affinity.isInAffinity magicDetails.affinities affinities

                    inFaction : InFaction
                    inFaction =
                        isInFaction model magicDetails

                    freeRankFromRace : Maybe ( Int, Race )
                    freeRankFromRace =
                        freeRankFromRaceOrTypePerk model magicDetails rankedMagic

                    minRank : Int
                    minRank =
                        case freeRankFromRace of
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

                    ( finalValue, rewardPoints ) =
                        List.range minRank rankedMagic.rank
                            |> List.map
                                (\rank ->
                                    ( -rank
                                        |> factionValueDiscountIf inFaction
                                        |> affinityValueDiscountIf inAffinity
                                    , if rankedMagic.name == MagicBodyRefinement && rank >= 3 then
                                        5

                                      else
                                        0
                                    )
                                )
                            |> List.unzip
                            |> Tuple.mapBoth
                                (\ps ->
                                    let
                                        inClass : Bool
                                        inClass =
                                            case magicDetails.class of
                                                Magic.ClassSpecial ->
                                                    magicDetails.name == MagicWishcasting && List.any Race.isGenie model.races

                                                Magic.ClassOne c ->
                                                    model.class == Just c

                                                Magic.ClassNone ->
                                                    False
                                    in
                                    ps
                                        |> List.sum
                                        |> classValueDiscountIf inClass
                                )
                                List.sum
                in
                { name = name
                , rank = rankedMagic.rank
                , freeRankFromRace = freeRankFromRace
                , power = finalValue
                , rewardPoints = rewardPoints
                , isElementalism = magicDetails.isElementalism
                , inAffinity = inAffinity
                }
                    |> (case freeRankFromRace of
                            Nothing ->
                                -- If you get it free from your race it usually
                                -- doesn't require the normal prerequisites
                                Utils.checkRequirements magicDetails name model

                            Just _ ->
                                Monad.succeed
                       )
                    |> (if magicDetails.name == MagicWishcasting && not (List.any Race.isGenie model.races) then
                            Monad.withWarning "Only Genies can access Wishcasting"

                        else
                            identity
                       )
            )


freeRankFromRaceOrTypePerk :
    { a
        | factions : List Faction
        , factionPerks : List Faction
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
                    TypePerk.all
            )


type InFaction
    = InFactionPerk
    | InFactionNoPerk
    | OutOfFaction
    | Nonfactional


isInFaction :
    { a
        | factions : List Faction
        , factionPerks : List Faction
        , typePerks : List Race
    }
    -> Magic.Details
    -> InFaction
isInFaction { factions, factionPerks, typePerks } magicDetails =
    if
        (List.member RaceSpider typePerks && magicDetails.name == MagicArachnescence)
            || (List.member RaceCyborg typePerks && magicDetails.name == MagicGadgetry)
            || (List.member RaceCyborg typePerks && magicDetails.name == MagicIntegration)
    then
        InFactionPerk

    else
        case magicDetails.faction of
            Just magicFaction ->
                if List.member magicFaction factions then
                    if List.member magicFaction factionPerks then
                        InFactionPerk

                    else
                        InFactionNoPerk

                else
                    OutOfFaction

            Nothing ->
                Nonfactional


classValueDiscountIf : Bool -> Int -> Int
classValueDiscountIf inClass v =
    if inClass && v < 0 then
        v + 2

    else
        v


factionValueDiscountIf : InFaction -> Int -> Int
factionValueDiscountIf factionality v =
    case factionality of
        OutOfFaction ->
            if v < 0 then
                v * 2

            else
                v

        Nonfactional ->
            v

        InFactionNoPerk ->
            v

        InFactionPerk ->
            if v < 0 then
                (v - 1) // 2

            else
                v
