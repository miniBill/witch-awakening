module Data.Costs.Magic exposing (value)

import Data.Affinity as Affinity
import Data.Costs.Monad as Monad exposing (Monad)
import Data.Costs.Utils as Utils exposing (Points)
import Data.Magic as Magic
import Generated.Magic
import Generated.Types as Types exposing (Affinity, Class(..), Faction, Magic(..), Race(..))
import List.Extra
import Maybe.Extra
import Types exposing (CosmicPearlData, RankedMagic)


value :
    { ignoreSorceressBonus : Bool }
    ->
        { a
            | cosmicPearl : CosmicPearlData
            , mainRace : Maybe Race
            , races : List Race
            , faction : Maybe ( Faction, Bool )
            , class : Maybe Class
            , typePerks : List Race
            , magic : List RankedMagic
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
                |> List.filterMap (magicValue model affinities)

        free : Maybe String
        free =
            if model.class == Just Sorceress && not ignoreSorceressBonus then
                pointsList
                    |> List.filter (\{ isElementalism, isOffAffinity } -> isElementalism && not isOffAffinity)
                    |> List.Extra.minimumBy .points
                    |> Maybe.map .name

            else
                Nothing

        offAffinityWarning : Maybe String
        offAffinityWarning =
            if model.class == Just Sorceress then
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
                if Just name == free then
                    0
                        |> Monad.succeed
                        |> Monad.withInfo
                            { label = label
                            , anchor = Just name
                            , value = Monad.FreeBecause "[Sorceress]"
                            }

                else
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
magicValue ({ faction, class, typePerks } as model) affinities magicDetails =
    model.magic
        |> List.Extra.findMap
            (\rankedMagic ->
                if magicDetails.name == rankedMagic.name then
                    basicMagicValue affinities class rankedMagic.rank magicDetails
                        |> Maybe.map
                            (\basicValue -> ( rankedMagic, basicValue ))

                else
                    Nothing
            )
        |> Maybe.map
            (\( rankedMagic, basicValue ) ->
                let
                    doubleIfNegative : Int -> Int
                    doubleIfNegative c =
                        if c > 0 then
                            c

                        else
                            c * 2

                    hasFactionDiscount : Bool
                    hasFactionDiscount =
                        (List.member Spider typePerks && magicDetails.name == Arachnescence)
                            || (List.member Cyborg typePerks && magicDetails.name == Gadgetry)
                            || (List.member Cyborg typePerks && magicDetails.name == Integration)
                            || (case magicDetails.faction of
                                    Just magicFaction ->
                                        Just ( magicFaction, True ) == faction

                                    Nothing ->
                                        False
                               )

                    finalValue : Int
                    finalValue =
                        if hasFactionDiscount then
                            factionDiscount basicValue

                        else
                            case magicDetails.faction of
                                Just magicFaction ->
                                    if Just ( magicFaction, False ) == faction then
                                        List.sum basicValue

                                    else
                                        doubleIfNegative (List.sum basicValue)

                                Nothing ->
                                    List.sum basicValue

                    name : String
                    name =
                        Types.magicToString rankedMagic.name
                in
                { name = name
                , rank = rankedMagic.rank
                , points = finalValue
                , isElementalism = magicDetails.isElementalism
                , isOffAffinity = not (isInAffinity magicDetails affinities)
                }
            )


factionDiscount : List Int -> Int
factionDiscount l =
    l
        |> List.map
            (\c ->
                if c > 0 then
                    c

                else
                    (c - 1) // 2
            )
        |> List.sum


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


basicMagicValue :
    List Affinity
    -> Maybe Class
    -> Int
    -> Magic.Details
    -> Maybe (List Int)
basicMagicValue affinities class rank magic =
    let
        isClass : Bool
        isClass =
            (magic.class == class)
                && (class /= Nothing)

        cases : Int -> Int -> Int
        cases basicValue inAffinityValue =
            if isInAffinity magic affinities then
                inAffinityValue

            else
                basicValue

        rank1 : number
        rank1 =
            if isClass then
                1

            else
                -1

        inner : Int -> Maybe Int
        inner r =
            case r of
                1 ->
                    Just rank1

                2 ->
                    Just <| cases -2 -1

                3 ->
                    Just <| cases -3 -2

                4 ->
                    Just <| cases -4 -2

                5 ->
                    Just <| cases -5 -3

                _ ->
                    Nothing
    in
    List.range 1 rank
        |> Maybe.Extra.combineMap inner
