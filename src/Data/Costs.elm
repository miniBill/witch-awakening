module Data.Costs exposing (Points, classValue, companionsValue, complicationsRawValue, complicationsValue, factionValue, magicsValue, negate, perkCost, perksValue, powerCap, powerToPoints, relicsValue, startingValue, totalCost, totalRewards, typePerksValue, zero)

import Data.Affinity as Affinity
import Data.Companion as Companion
import Data.Complication as Complication
import Data.FactionalMagic as FactionalMagic
import Data.Magic as Magic
import Data.Perk as Perk
import Data.Relic as Relic
import Data.TypePerk as TypePerk
import Generated.Types as Types exposing (Affinity, Class(..), Companion, Faction(..), GameMode(..), Magic(..), Perk(..), Race(..), Relic(..))
import List.Extra
import Maybe.Extra
import Result.Extra
import ResultME exposing (ResultME)
import Types exposing (ComplicationKind(..), CosmicPearlData, Model, RankedMagic, RankedPerk, RankedRelic)


type alias Points =
    { power : Int
    , rewardPoints : Int
    , warnings : List String
    }


zero : Points
zero =
    { power = 0
    , rewardPoints = 0
    , warnings = []
    }


slotUnsupported : ResultME String value
slotUnsupported =
    ResultME.error "Slot modes not supported yet"


capWithWarning : Int -> String -> Int -> Points
capWithWarning cap warning value =
    if value > cap then
        { zero
            | power = cap
            , warnings = [ warning ]
        }

    else
        { zero | power = value }


{-| "In Early Bird mode, complications can only increase the starting power/power cap by up to 30 points"
-}
earlyBirdWarning : String
earlyBirdWarning =
    "In Early Bird mode, complications can only increase the starting power/power cap by up to 30 points"


{-| "In Story Arc mode, complications can only increase the power cap by up to 60 points"
-}
storyArcWarning : String
storyArcWarning =
    "In Story Arc mode, complications can only increase the power cap by up to 60 points"


{-| "Complications can only increase the power cap by up to 30 points"
-}
normalCapWarning : String
normalCapWarning =
    "Complications can only increase the power cap by up to 30 points"


{-| "Complications can only increase the starting power by up to 30 points"
-}
normalInitialWarning : String
normalInitialWarning =
    "Complications can only increase the starting power by up to 30 points"



-- Total --


totalCost : Model key -> ResultME String Points
totalCost model =
    [ Result.map negate <| classValue model
    , Result.map negate <| startingValue model
    , Result.map negate <| complicationsValue model
    , Result.map negate <| typePerksValue model
    , Result.map negate <| magicsValue model
    , perksCost model
    , Result.map negate <| factionValue model
    , Result.map negate <| companionsValue model
    , Result.map negate <| relicsValue model
    , Result.map negate <| conversion model
    , Result.map zeroOut <| powerCap model
    ]
        |> resultsSum
        |> Result.map
            (\result ->
                let
                    addIf : Int -> String -> List String -> List String
                    addIf b warning acc =
                        if b > 0 then
                            warning :: acc

                        else
                            acc

                    isGemini : Bool
                    isGemini =
                        List.any
                            (\race ->
                                case race of
                                    Gemini _ ->
                                        True

                                    _ ->
                                        False
                            )
                            model.races
                in
                { power =
                    if isGemini then
                        result.power // 2

                    else
                        result.power
                , rewardPoints =
                    if isGemini then
                        result.rewardPoints // 2

                    else
                        result.rewardPoints
                , warnings =
                    result.warnings
                        |> addIf result.rewardPoints "Not enough reward points! Try converting some power."
                        |> addIf result.power
                            (if model.gameMode == Just StoryArc && not model.capBuild then
                                "Not enough power!"

                             else
                                "Not enough power! Try adding complications."
                            )
                }
            )


totalRewards : Model key -> ResultME String Points
totalRewards model =
    [ classValue model
    , conversion model
    ]
        |> resultsSum



-- Class --


classValue : Model key -> ResultME String Points
classValue model =
    case model.class of
        Just class ->
            case class of
                Warlock ->
                    Ok { zero | rewardPoints = 20 }

                Academic ->
                    Ok zero

                Sorceress ->
                    Ok zero

        Nothing ->
            ResultME.error "You need to select a class"



-- Power cap --


powerCap : Model key -> ResultME String Points
powerCap model =
    case model.gameMode of
        Nothing ->
            model.towardsCap
                |> capWithWarning 30 normalCapWarning
                |> sum { zero | power = 100 }
                |> Ok

        Just StoryArc ->
            complicationsRawValue model
                |> Result.map (capWithWarning 60 storyArcWarning)
                |> Result.map (sum { zero | power = 150 })

        Just EarlyBird ->
            complicationsRawValue model
                |> Result.map (capWithWarning 30 earlyBirdWarning)
                |> Result.map (sum { zero | power = 75 })

        Just SkillTree ->
            slotUnsupported

        Just Constellation ->
            slotUnsupported



-- Starting power  --


startingValue : Model key -> ResultME String Points
startingValue model =
    let
        power : ResultME String Int
        power =
            case model.gameMode of
                Just StoryArc ->
                    if model.capBuild then
                        Ok 150

                    else
                        Ok 10

                Just EarlyBird ->
                    Ok 75

                Just SkillTree ->
                    slotUnsupported

                Just Constellation ->
                    slotUnsupported

                Nothing ->
                    if model.capBuild then
                        Ok 100

                    else
                        Ok 30
    in
    Result.map powerToPoints power



-- Complications --


complicationsValue : Model key -> ResultME String Points
complicationsValue model =
    complicationsRawValue model
        |> Result.andThen
            (\value ->
                case model.gameMode of
                    Just StoryArc ->
                        if model.capBuild then
                            value
                                |> capWithWarning 60 storyArcWarning
                                |> Ok

                        else
                            Ok zero

                    Just EarlyBird ->
                        value
                            |> capWithWarning 30 earlyBirdWarning
                            |> Ok

                    Just SkillTree ->
                        slotUnsupported

                    Just Constellation ->
                        slotUnsupported

                    Nothing ->
                        if model.capBuild then
                            model.towardsCap
                                |> capWithWarning 30 normalInitialWarning
                                |> Ok

                        else
                            (value - model.towardsCap)
                                |> capWithWarning 30 normalInitialWarning
                                |> Ok
            )


complicationsRawValue : Model key -> ResultME String Int
complicationsRawValue model =
    resultSum (complicationValue model) model.complications


complicationValue : Model key -> Types.RankedComplication -> ResultME String Int
complicationValue model complication =
    let
        get : Int -> List a -> ResultME String a
        get tier list =
            case List.Extra.getAt (tier - 1) list of
                Just v ->
                    Ok v

                Nothing ->
                    ResultME.error <| "Could not get tier " ++ String.fromInt tier ++ " for complication " ++ Types.complicationToString complication.name
    in
    case List.Extra.find (\{ name } -> name == complication.name) Complication.all of
        Nothing ->
            ResultME.error <| "Could not find complication " ++ Types.complicationToString complication.name

        Just details ->
            let
                raw : ResultME String Int
                raw =
                    case ( details.content, complication.kind ) of
                        ( Complication.Single value _, _ ) ->
                            Ok value

                        ( Complication.WithTiers _ tiers _, Tiered tier ) ->
                            Result.map Tuple.second <| get tier tiers

                        ( Complication.WithChoices _ choices _, Tiered tier ) ->
                            Result.map Tuple.second <| get tier choices

                        ( Complication.WithGains _ costs, Tiered tier ) ->
                            get tier costs

                        ( _, Nontiered ) ->
                            ResultME.error <| "Need a tier for complication " ++ Types.complicationToString complication.name
            in
            Result.map
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



-- Type perks --


typePerksValue : Model key -> ResultME String Points
typePerksValue model =
    resultSum typePerkValue model.typePerks
        |> Result.map powerToPoints


typePerkValue : Race -> ResultME String Int
typePerkValue race =
    find "Type perk" .race race TypePerk.all Types.raceToString
        |> Result.map (\{ cost } -> -cost)


find : String -> (item -> key) -> key -> List item -> (key -> String) -> ResultME String item
find label toKey value list toString =
    case List.Extra.find (\candidate -> toKey candidate == value) list of
        Nothing ->
            ResultME.error <| label ++ " " ++ toString value ++ " not found"

        Just v ->
            Ok v



-- Magics --


magicsValue :
    { a
        | class : Maybe Class
        , races : List Race
        , mainRace : Maybe Race
        , typePerks : List Race
        , magic : List RankedMagic
        , faction : Maybe ( Faction, Bool )
        , cosmicPearl : CosmicPearlData
    }
    -> ResultME String Points
magicsValue model =
    let
        affinities : List Affinity
        affinities =
            Affinity.fromModel model
    in
    resultSum (magicValue affinities model) model.magic
        |> Result.map powerToPoints


magicValue :
    List Affinity
    ->
        { a
            | faction : Maybe ( Faction, Bool )
            , class : Maybe Class
            , typePerks : List Race
        }
    -> RankedMagic
    -> ResultME String Int
magicValue affinities { faction, class, typePerks } { name, rank } =
    case
        Magic.all
            |> List.Extra.find (\magic -> magic.name == name)
            |> Maybe.andThen
                (\magic ->
                    let
                        cost : Maybe Int
                        cost =
                            magicCost affinities class rank magic
                    in
                    if magic.name == Arachnescence && List.member Spider typePerks then
                        Maybe.map factionDiscount cost

                    else
                        cost
                )
            |> Maybe.Extra.orElseLazy
                (\_ ->
                    FactionalMagic.all
                        |> List.Extra.find (\magic -> magic.name == name)
                        |> Maybe.andThen
                            (\magic ->
                                let
                                    cost : Maybe Int
                                    cost =
                                        magicCost affinities class rank magic
                                in
                                if
                                    (Just ( magic.faction, True ) == faction)
                                        || (List.member Cyborg typePerks && List.member magic.name [ Gadgetry, Integration ])
                                then
                                    Maybe.map
                                        factionDiscount
                                        cost

                                else if Just ( magic.faction, False ) == faction then
                                    cost

                                else
                                    Maybe.map
                                        (\c ->
                                            if c > 0 then
                                                c

                                            else
                                                c * 2
                                        )
                                        cost
                            )
                )
    of
        Just cost ->
            Ok cost

        Nothing ->
            ResultME.error <| "Magic " ++ Types.magicToString name ++ " not found"


factionDiscount : Int -> Int
factionDiscount c =
    if c > 0 then
        c * 2

    else
        (c - 1) // 2


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
        cases basicCost inAffinityCost inClassCost inBothCost =
            if isClass then
                if isAffinity then
                    inBothCost

                else
                    inClassCost

            else if isAffinity then
                inAffinityCost

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


perksValue :
    { a
        | races : List Race
        , mainRace : Maybe Race
        , cosmicPearl : CosmicPearlData
        , typePerks : List Race
        , perks : List RankedPerk
        , class : Maybe Class
    }
    -> ResultME String Points
perksValue model =
    resultSum (perkCost model) model.perks
        |> Result.map powerToPoints
        |> Result.map negate


perksCost :
    { a
        | races : List Race
        , mainRace : Maybe Race
        , cosmicPearl : CosmicPearlData
        , typePerks : List Race
        , perks : List RankedPerk
        , class : Maybe Class
    }
    -> ResultME String Points
perksCost model =
    perksValue model
        |> Result.map negate


perkCost :
    { a
        | class : Maybe Class
        , races : List Race
        , mainRace : Maybe Race
        , cosmicPearl : CosmicPearlData
        , typePerks : List Race
        , perks : List RankedPerk
    }
    -> RankedPerk
    -> ResultME String Int
perkCost ({ class } as model) { name, cost } =
    let
        affinities : List Affinity
        affinities =
            Affinity.fromModel model
    in
    find "Perk" .name name (Perk.all model.perks) Types.perkToString
        |> Result.map
            (\perk ->
                let
                    isClass : Bool
                    isClass =
                        Just perk.class == class

                    isAffinity : Bool
                    isAffinity =
                        List.member perk.affinity affinities

                    changelinged : Int
                    changelinged =
                        case name of
                            ChargeSwap _ ->
                                if List.member Changeling model.races then
                                    cost - 3

                                else
                                    cost

                            _ ->
                                cost
                in
                changelinged
                    |> applyClassBonusIf isClass
                    |> halveIfPositiveAnd isAffinity
            )



-- Faction --


factionValue : Model key -> ResultME String Points
factionValue model =
    case model.faction of
        Nothing ->
            Ok { zero | power = 4 }

        Just ( _, False ) ->
            Ok { zero | power = 2 }

        Just ( _, True ) ->
            Ok zero



-- Companions --


companionsValue : Model key -> ResultME String Points
companionsValue model =
    let
        totalCompanionCost : List ( Maybe Faction, Companion.Details ) -> ResultME String Int
        totalCompanionCost companions =
            companions
                |> Result.Extra.combineMap
                    (\( _, { name, cost } ) ->
                        case cost of
                            Just v ->
                                Ok v

                            Nothing ->
                                ResultME.error <| "Companion " ++ Types.companionToString name ++ " does not have a fixed cost"
                    )
                |> Result.map List.sum

        forFree : List ( Maybe Faction, Companion.Details ) -> Int
        forFree companions =
            let
                treasure : Bool
                treasure =
                    model.faction == Just ( TheCollegeOfArcadia, True )

                byCost : List ( Maybe Faction, Int, Companion.Details )
                byCost =
                    companions
                        |> List.filterMap
                            (\( f, c ) ->
                                c.cost |> Maybe.map (\cost -> ( f, cost, c ))
                            )
                        |> List.sortBy (\( _, cost, _ ) -> -cost)

                sameFaction : List ( Int, Companion.Details )
                sameFaction =
                    case model.faction of
                        Nothing ->
                            []

                        Just ( f, _ ) ->
                            byCost
                                |> List.filterMap
                                    (\( faction, cost, c ) ->
                                        if faction == Just f then
                                            Just ( cost, c )

                                        else
                                            Nothing
                                    )
                                |> List.take
                                    (if treasure then
                                        4

                                     else
                                        2
                                    )

                sameKind : List ( Int, Companion.Details )
                sameKind =
                    byCost
                        |> List.filterMap
                            (\( _, cost, companion ) ->
                                if
                                    sameRace companion model.races
                                        || sameClass companion model.class
                                then
                                    Just ( cost, companion )

                                else
                                    Nothing
                            )
                        |> List.take
                            (if treasure then
                                4

                             else
                                2
                            )
            in
            if List.isEmpty sameFaction then
                sameKind
                    |> List.take
                        (if treasure then
                            2

                         else
                            1
                        )
                    |> List.map (\( cost, _ ) -> cost)
                    |> List.sum

            else if List.isEmpty sameKind then
                sameFaction
                    |> List.take
                        (if treasure then
                            2

                         else
                            1
                        )
                    |> List.map (\( cost, _ ) -> cost)
                    |> List.sum

            else if treasure then
                List.Extra.lift4
                    (\a b c d ->
                        [ a, b, c, d ]
                            |> List.Extra.uniqueBy (\( _, { name } ) -> name)
                            |> List.map Tuple.first
                            |> List.sum
                    )
                    sameFaction
                    sameFaction
                    sameKind
                    (List.map (\( _, cost, c ) -> ( cost, c )) byCost)
                    |> List.maximum
                    |> Maybe.withDefault 0

            else
                List.Extra.lift2
                    (\( sfCost, sf ) ( skCost, sk ) ->
                        if sf.name == sk.name then
                            sfCost

                        else
                            sfCost + skCost
                    )
                    sameFaction
                    sameKind
                    |> List.maximum
                    |> Maybe.withDefault 0
    in
    model.companions
        |> Result.Extra.combineMap getCompanion
        |> Result.andThen
            (\companions ->
                Result.map
                    (\calculatedCost -> powerToPoints <| forFree companions - calculatedCost)
                    (totalCompanionCost companions)
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


getCompanion : Companion -> ResultME String ( Maybe Faction, Companion.Details )
getCompanion companion =
    case
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
    of
        Just p ->
            Ok p

        Nothing ->
            ResultME.error <| "Companion " ++ Types.companionToString companion ++ " not found"



-- Relics --


relicsValue : Model key -> ResultME String Points
relicsValue model =
    resultSum (relicCost model.class model.cosmicPearl) model.relics
        |> Result.map (\cost -> { zero | rewardPoints = -cost })


relicCost : Maybe Class -> CosmicPearlData -> RankedRelic -> ResultME String Int
relicCost class pearl { name, cost } =
    find "Relic" .name name Relic.all Types.relicToString
        |> Result.map
            (\relic ->
                let
                    isClass : Bool
                    isClass =
                        Just relic.class == class

                    baseCost : Int
                    baseCost =
                        if isClass then
                            cost - 2

                        else
                            cost

                    multiplier : Int
                    multiplier =
                        if name == CosmicPearl then
                            max 1 <| List.length pearl.add + List.length pearl.change

                        else
                            1
                in
                baseCost * multiplier
            )


conversion : Model key -> ResultME String Points
conversion model =
    Ok
        { zero
            | power = -model.powerToRewards
            , rewardPoints = model.powerToRewards
        }



-- Utils --


negate : Points -> Points
negate p =
    { p | power = -p.power, rewardPoints = -p.rewardPoints }


sum : Points -> Points -> Points
sum l r =
    { power = l.power + r.power
    , rewardPoints = l.rewardPoints + r.rewardPoints
    , warnings = l.warnings ++ r.warnings
    }


sumPoints : List Points -> Points
sumPoints =
    List.foldl sum zero


resultSum : (item -> ResultME String Int) -> List item -> ResultME String Int
resultSum toValue list =
    list
        |> Result.Extra.combineMap toValue
        |> Result.map List.sum


resultsSum : List (ResultME String Points) -> ResultME String Points
resultsSum list =
    list
        |> Result.Extra.combine
        |> Result.map sumPoints


powerToPoints : Int -> Points
powerToPoints value =
    { zero | power = value }


zeroOut : Points -> Points
zeroOut points =
    { points | power = 0, rewardPoints = 0 }


applyClassBonusIf : Bool -> Int -> Int
applyClassBonusIf isClass cost =
    if isClass then
        cost - 2

    else
        cost


halveIfPositiveAnd : Bool -> Int -> Int
halveIfPositiveAnd condition cost =
    if condition && cost > 0 then
        (cost + 1) // 2

    else
        cost
