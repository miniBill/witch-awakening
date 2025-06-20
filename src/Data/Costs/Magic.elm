module Data.Costs.Magic exposing (value)

import Data.Affinity as Affinity
import Data.Costs.Monad as Monad exposing (Monad)
import Data.Costs.Utils as Utils exposing (Points)
import Data.Magic as Magic
import Generated.Magic
import Generated.Types as Types exposing (Affinity, Class(..), Faction, Magic(..), Race(..))
import List.Extra
import Types exposing (CosmicPearlData, RankedMagic)


value :
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
value model =
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
            if model.class == Just Sorceress then
                pointsList
                    |> List.filter .isElementalism
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

                    _ ->
                        Just "Multiple off-affinity elementalism magics are only allowed for Sorceresses."
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
                                        basicValue

                                    else
                                        doubleIfNegative basicValue

                                Nothing ->
                                    basicValue

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


factionDiscount : Int -> Int
factionDiscount c =
    if c > 0 then
        c * 2

    else
        (c - 1) // 2


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
    -> Maybe Int
basicMagicValue affinities class rank magic =
    let
        isClass : Bool
        isClass =
            (magic.class == class)
                && (class /= Nothing)

        cases : Int -> Int -> Int -> Int -> Int
        cases basicValue inAffinityValue inClassValue inBothValue =
            if isClass then
                if isInAffinity magic affinities then
                    inBothValue

                else
                    inClassValue

            else if isInAffinity magic affinities then
                inAffinityValue

            else
                basicValue
    in
    case rank of
        1 ->
            Just <| cases -1 -1 1 1

        2 ->
            Just <| cases -3 -2 -1 0

        3 ->
            Just <| cases -6 -4 -4 -2

        4 ->
            Just <| cases -10 -6 -8 -4

        5 ->
            Just <| cases -15 -9 -13 -7

        _ ->
            Nothing
