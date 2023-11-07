module Data.Costs exposing (classPower, companionsValue, complicationsValue, factionValue, initialPower, magicsValue, perksValue, powerCap, relicsValue, typePerksValue)

import Data.Companion as Companion
import Data.Complication as Complication
import Data.FactionalMagic as FactionalMagic
import Data.Magic as Magic
import Data.Perk as Perk
import Data.Relic as Relic
import Data.TypePerk as TypePerk
import Generated.Types exposing (Affinity, Class(..), Companion, Faction, GameMode(..), Race, Relic(..))
import List.Extra
import Maybe.Extra
import Types exposing (ComplicationKind(..), CosmicPearlData, Model, RankedMagic, RankedPerk, RankedRelic)



-- Class --


classPower : Model -> Maybe Int
classPower model =
    Maybe.map
        (\class ->
            case class of
                Warlock ->
                    20

                Academic ->
                    0

                Sorceress ->
                    0
        )
        model.class



-- Power cap --


powerCap : Model -> Maybe Int
powerCap model =
    case model.gameMode of
        Nothing ->
            Just <| 100 + model.towardsCap

        Just StoryArc ->
            Maybe.map ((+) 150) <| complicationsValue model

        Just EarlyBird ->
            Just <| 75 + model.towardsCap

        Just SkillTree ->
            Nothing

        Just Constellation ->
            Nothing



-- Initial power / complications --


initialPower : Model -> Maybe Int
initialPower model =
    case ( model.gameMode, complicationsValue model ) of
        ( Nothing, Just complications ) ->
            Just <| 30 + complications - model.towardsCap

        ( Just StoryArc, _ ) ->
            Just 10

        ( Just EarlyBird, Just complications ) ->
            Just <| 75 + complications - model.towardsCap

        _ ->
            Nothing


complicationsValue : Model -> Maybe Int
complicationsValue model =
    maybeSum (complicationValue model) model.complications


complicationValue : Model -> Types.RankedComplication -> Maybe Int
complicationValue model complication =
    let
        get : Int -> List ( a, Int ) -> Maybe Int
        get tier list =
            List.Extra.getAt (tier - 1) list
                |> Maybe.map Tuple.second
    in
    Complication.all
        |> List.Extra.find (\{ name } -> name == complication.name)
        |> Maybe.andThen
            (\details ->
                let
                    raw : Maybe Int
                    raw =
                        case ( details.content, complication.kind ) of
                            ( Complication.Single value _, _ ) ->
                                Just value

                            ( Complication.WithTiers _ tiers _, Tiered tier ) ->
                                get tier tiers

                            ( Complication.WithChoices _ choices _, Tiered tier ) ->
                                get tier choices

                            ( Complication.WithCosts _ costs, Tiered tier ) ->
                                List.Extra.getAt (tier - 1) costs

                            ( _, Nontiered ) ->
                                Nothing
                in
                Maybe.map
                    (\r ->
                        let
                            bonus : Int
                            bonus =
                                if details.class == model.class && details.class /= Nothing then
                                    2

                                else
                                    0
                        in
                        r + bonus
                    )
                    raw
            )



-- Type perks --


typePerksValue : Model -> Maybe Int
typePerksValue model =
    maybeSum typePerkValue model.typePerks


typePerkValue : Race -> Maybe Int
typePerkValue race =
    List.Extra.find (\perk -> perk.race == race) TypePerk.all
        |> Maybe.map (\{ cost } -> -cost)



-- Magics --


magicsValue : Model -> Maybe Int
magicsValue model =
    let
        affinities : List Affinity
        affinities =
            Types.affinities model
    in
    maybeSum (magicValue affinities model) model.magic


magicValue : List Affinity -> Model -> RankedMagic -> Maybe Int
magicValue affinities { faction, class } { name, rank } =
    case
        Magic.all
            |> List.Extra.find (\magic -> magic.name == name)
            |> Maybe.andThen (magicCost affinities class rank)
    of
        Just cost ->
            Just cost

        Nothing ->
            FactionalMagic.all
                |> List.Extra.find (\magic -> magic.name == name)
                |> Maybe.andThen
                    (\magic ->
                        let
                            cost : Maybe Int
                            cost =
                                magicCost affinities class rank magic
                        in
                        if Just ( magic.faction, True ) == faction then
                            Maybe.map (\c -> (c + 1) // 2) cost

                        else
                            Maybe.map ((*) 2) cost
                    )


magicCost :
    List Affinity
    -> Maybe Class
    -> Int
    -> { d | class : Maybe Class, affinities : Magic.Affinities }
    -> Maybe Int
magicCost affinities class rank magic =
    let
        isClass : Bool
        isClass =
            (magic.class == class)
                && (class /= Nothing)

        isAffinity : Bool
        isAffinity =
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

        cases : Int -> Int -> Int -> Int -> Int
        cases basicCost affinityCost classCost bothCost =
            if isClass then
                if isAffinity then
                    bothCost

                else
                    classCost

            else if isAffinity then
                affinityCost

            else
                basicCost
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



-- Perks --


perksValue : Model -> Maybe Int
perksValue model =
    let
        affinities : List Affinity
        affinities =
            Types.affinities model
    in
    maybeSum (perkValue affinities model.class) model.perks


perkValue : List Affinity -> Maybe Class -> RankedPerk -> Maybe Int
perkValue affinities class { name, cost } =
    Perk.all
        |> List.Extra.find (\perk -> perk.name == name)
        |> Maybe.map
            (\perk ->
                let
                    isClass : Bool
                    isClass =
                        Just perk.class == class

                    isAffinity : Bool
                    isAffinity =
                        List.member perk.affinity affinities
                in
                if isClass then
                    if isAffinity then
                        (-cost + 2 - 1) // 2

                    else
                        -cost + 2

                else if isAffinity then
                    -(cost + 1) // 2

                else
                    -cost
            )



-- Faction --


factionValue : Model -> Maybe Int
factionValue model =
    case model.faction of
        Nothing ->
            Just 4

        Just ( _, False ) ->
            Just 2

        Just ( _, True ) ->
            Just 0



-- Companions --


companionsValue : Model -> Maybe Int
companionsValue model =
    let
        totalCost : List ( Maybe Faction, Companion.Details ) -> Maybe Int
        totalCost companions =
            companions
                |> Maybe.Extra.traverse (\( _, { cost } ) -> cost)
                |> Maybe.map List.sum

        forFree : List ( Maybe Faction, Companion.Details ) -> Int
        forFree companions =
            let
                byCost : List ( Maybe Faction, Companion.Details )
                byCost =
                    companions
                        |> List.sortBy (\( _, { cost } ) -> -(Maybe.withDefault -1 cost))

                sameFaction : List Companion.Details
                sameFaction =
                    case model.faction of
                        Nothing ->
                            []

                        Just ( f, _ ) ->
                            byCost
                                |> List.filterMap
                                    (\( faction, c ) ->
                                        if faction == Just f then
                                            Just c

                                        else
                                            Nothing
                                    )
                                |> List.take 2

                sameKind : List Companion.Details
                sameKind =
                    byCost
                        |> List.filterMap
                            (\( _, companion ) ->
                                if
                                    sameRace companion model.races
                                        || sameClass companion model.class
                                then
                                    Just companion

                                else
                                    Nothing
                            )
                        |> List.take 2
            in
            if List.isEmpty sameFaction then
                sameKind
                    |> List.head
                    |> Maybe.andThen (\{ cost } -> cost)
                    |> Maybe.withDefault 0

            else if List.isEmpty sameKind then
                sameFaction
                    |> List.head
                    |> Maybe.andThen (\{ cost } -> cost)
                    |> Maybe.withDefault 0

            else
                List.Extra.lift2
                    (\sf sk ->
                        if sf.name == sk.name then
                            sf.cost

                        else
                            Maybe.map2 (+) sf.cost sk.cost
                    )
                    sameFaction
                    sameKind
                    |> List.filterMap identity
                    |> List.maximum
                    |> Maybe.withDefault 0
    in
    model.companions
        |> Maybe.Extra.traverse getCompanion
        |> Maybe.andThen
            (\companions ->
                Maybe.map
                    (\calculatedCost -> forFree companions - calculatedCost)
                    (totalCost companions)
            )


sameClass : Companion.Details -> Maybe Class -> Bool
sameClass companion maybeClass =
    case companion.class of
        Companion.ClassOne class_ ->
            Just class_ == maybeClass

        Companion.ClassAny ->
            True

        Companion.ClassNone ->
            False

        Companion.ClassSpecial ->
            False


sameRace : Companion.Details -> List Race -> Bool
sameRace companion races =
    List.isEmpty companion.races
        || List.any (\companionRace -> List.member companionRace races) companion.races


getCompanion : Companion -> Maybe ( Maybe Faction, Companion.Details )
getCompanion companion =
    List.Extra.findMap
        (\( _, faction, group ) ->
            List.Extra.findMap
                (\({ name } as c) ->
                    if name == companion then
                        Just ( faction, c )

                    else
                        Nothing
                )
                group
        )
        Companion.all



-- Relics --


relicsValue : Model -> Maybe Int
relicsValue model =
    maybeSum (relicValue model.class model.cosmicPearl) model.relics


relicValue : Maybe Class -> CosmicPearlData -> RankedRelic -> Maybe Int
relicValue class pearl { name, cost } =
    Relic.all
        |> List.Extra.find (\relic -> relic.name == name)
        |> Maybe.map
            (\relic ->
                let
                    isClass : Bool
                    isClass =
                        Just relic.class == class

                    baseCost : Int
                    baseCost =
                        if isClass then
                            -cost + 2

                        else
                            -cost

                    multiplier : Int
                    multiplier =
                        if name == CosmicPearl then
                            max 1 <| List.length pearl.add + List.length pearl.change

                        else
                            1
                in
                baseCost * multiplier
            )



-- Utils --


maybeSum : (item -> Maybe Int) -> List item -> Maybe Int
maybeSum toValue list =
    list
        |> Maybe.Extra.traverse toValue
        |> Maybe.map List.sum
