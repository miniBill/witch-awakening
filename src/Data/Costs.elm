module Data.Costs exposing (Points, classValue, companionsValue, complicationsRawValue, complicationsValue, factionValue, magicsValue, negate, perkCost, perksValue, powerCap, powerToPoints, relicsValue, startingValue, totalCost, totalRewards, typePerksValue, zero)

import Data.Affinity as Affinity
import Data.Companion as Companion
import Data.Complication as Complication
import Data.FactionalMagic as FactionalMagic
import Data.Magic as Magic
import Data.Perk as Perk
import Data.Relic as Relic
import Data.TypePerk as TypePerk
import Generated.Types as Types exposing (Affinity, Class(..), Companion, Faction, GameMode(..), Perk(..), Race(..), Relic(..))
import List.Extra
import Results exposing (Results(..))
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


slotUnsupported : Results value
slotUnsupported =
    Errs [ "Slot modes not supported yet" ]


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


totalCost : Model key -> Results Points
totalCost model =
    [ Results.map negate <| classValue model
    , Results.map negate <| startingValue model
    , Results.map negate <| complicationsValue model
    , Results.map negate <| typePerksValue model
    , Results.map negate <| magicsValue model
    , perksCost model
    , Results.map negate <| factionValue model
    , Results.map negate <| companionsValue model
    , Results.map negate <| relicsValue model
    , Results.map negate <| conversion model
    , Results.map zeroOut <| powerCap model
    ]
        |> resultsSum
        |> Results.map
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


totalRewards : Model key -> Results Points
totalRewards model =
    [ classValue model
    , conversion model
    ]
        |> resultsSum



-- Class --


classValue : Model key -> Results Points
classValue model =
    case model.class of
        Just class ->
            case class of
                Warlock ->
                    Oks { zero | rewardPoints = 20 }

                Academic ->
                    Oks zero

                Sorceress ->
                    Oks zero

        Nothing ->
            Errs [ "You need to select a class" ]



-- Power cap --


powerCap : Model key -> Results Points
powerCap model =
    case model.gameMode of
        Nothing ->
            model.towardsCap
                |> capWithWarning 30 normalCapWarning
                |> sum { zero | power = 100 }
                |> Oks

        Just StoryArc ->
            complicationsRawValue model
                |> Results.map (capWithWarning 60 storyArcWarning)
                |> Results.map (sum { zero | power = 150 })

        Just EarlyBird ->
            complicationsRawValue model
                |> Results.map (capWithWarning 30 earlyBirdWarning)
                |> Results.map (sum { zero | power = 75 })

        Just SkillTree ->
            slotUnsupported

        Just Constellation ->
            slotUnsupported



-- Starting power  --


startingValue : Model key -> Results Points
startingValue model =
    let
        power : Results Int
        power =
            case model.gameMode of
                Just StoryArc ->
                    if model.capBuild then
                        Oks 150

                    else
                        Oks 10

                Just EarlyBird ->
                    Oks 75

                Just SkillTree ->
                    slotUnsupported

                Just Constellation ->
                    slotUnsupported

                Nothing ->
                    if model.capBuild then
                        Oks 100

                    else
                        Oks 30
    in
    Results.map powerToPoints power



-- Complications --


complicationsValue : Model key -> Results Points
complicationsValue model =
    complicationsRawValue model
        |> Results.andThen
            (\value ->
                case model.gameMode of
                    Just StoryArc ->
                        if model.capBuild then
                            value
                                |> capWithWarning 60 storyArcWarning
                                |> Oks

                        else
                            Oks zero

                    Just EarlyBird ->
                        value
                            |> capWithWarning 30 earlyBirdWarning
                            |> Oks

                    Just SkillTree ->
                        slotUnsupported

                    Just Constellation ->
                        slotUnsupported

                    Nothing ->
                        if model.capBuild then
                            model.towardsCap
                                |> capWithWarning 30 normalInitialWarning
                                |> Oks

                        else
                            (value - model.towardsCap)
                                |> capWithWarning 30 normalInitialWarning
                                |> Oks
            )


complicationsRawValue : Model key -> Results Int
complicationsRawValue model =
    resultSum (complicationValue model) model.complications


complicationValue : Model key -> Types.RankedComplication -> Results Int
complicationValue model complication =
    let
        get : Int -> List a -> Results a
        get tier list =
            case List.Extra.getAt (tier - 1) list of
                Just v ->
                    Oks v

                Nothing ->
                    Errs [ "Could not get tier " ++ String.fromInt tier ++ " for complication " ++ Types.complicationToString complication.name ]
    in
    case List.Extra.find (\{ name } -> name == complication.name) Complication.all of
        Nothing ->
            Errs [ "Could not find complication " ++ Types.complicationToString complication.name ]

        Just details ->
            let
                raw : Results Int
                raw =
                    case ( details.content, complication.kind ) of
                        ( Complication.Single value _, _ ) ->
                            Oks value

                        ( Complication.WithTiers _ tiers _, Tiered tier ) ->
                            Results.map Tuple.second <| get tier tiers

                        ( Complication.WithChoices _ choices _, Tiered tier ) ->
                            Results.map Tuple.second <| get tier choices

                        ( Complication.WithGains _ costs, Tiered tier ) ->
                            get tier costs

                        ( _, Nontiered ) ->
                            Errs [ "Need a tier for complication " ++ Types.complicationToString complication.name ]
            in
            Results.map
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


typePerksValue : Model key -> Results Points
typePerksValue model =
    resultSum typePerkValue model.typePerks
        |> Results.map powerToPoints


typePerkValue : Race -> Results Int
typePerkValue race =
    find "Type perk" .race race TypePerk.all Types.raceToString
        |> Results.map (\{ cost } -> -cost)


find : String -> (item -> key) -> key -> List item -> (key -> String) -> Results item
find label toKey value list toString =
    case List.Extra.find (\candidate -> toKey candidate == value) list of
        Nothing ->
            Errs [ label ++ " " ++ toString value ++ " not found" ]

        Just v ->
            Oks v



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
    -> Results Points
magicsValue model =
    let
        affinities : List Affinity
        affinities =
            Affinity.fromModel model
    in
    resultSum (magicValue affinities model) model.magic
        |> Results.map powerToPoints


magicValue :
    List Affinity
    ->
        { a
            | faction : Maybe ( Faction, Bool )
            , class : Maybe Class
        }
    -> RankedMagic
    -> Results Int
magicValue affinities { faction, class } { name, rank } =
    case
        Magic.all
            |> List.Extra.find (\magic -> magic.name == name)
            |> Maybe.andThen (magicCost affinities class rank)
    of
        Just cost ->
            Oks cost

        Nothing ->
            case
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
                                Maybe.map
                                    (\c ->
                                        if c > 0 then
                                            c * 2

                                        else
                                            (c - 1) // 2
                                    )
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
            of
                Just cost ->
                    Oks cost

                Nothing ->
                    Errs [ "Magic " ++ Types.magicToString name ++ " not found" ]


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
    -> Results Points
perksValue model =
    resultSum (perkCost model) model.perks
        |> Results.map powerToPoints
        |> Results.map negate


perksCost :
    { a
        | races : List Race
        , mainRace : Maybe Race
        , cosmicPearl : CosmicPearlData
        , typePerks : List Race
        , perks : List RankedPerk
        , class : Maybe Class
    }
    -> Results Points
perksCost model =
    perksValue model
        |> Results.map negate


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
    -> Results Int
perkCost ({ class } as model) { name, cost } =
    let
        affinities : List Affinity
        affinities =
            Affinity.fromModel model
    in
    find "Perk" .name name (Perk.all model.perks) Types.perkToString
        |> Results.map
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


factionValue : Model key -> Results Points
factionValue model =
    case model.faction of
        Nothing ->
            Oks { zero | power = 4 }

        Just ( _, False ) ->
            Oks { zero | power = 2 }

        Just ( _, True ) ->
            Oks zero



-- Companions --


companionsValue : Model key -> Results Points
companionsValue model =
    let
        totalCompanionCost : List ( Maybe Faction, Companion.Details ) -> Results Int
        totalCompanionCost companions =
            companions
                |> Results.combineMap
                    (\( _, { name, cost } ) ->
                        case cost of
                            Just v ->
                                Oks v

                            Nothing ->
                                Errs [ "Companion " ++ Types.companionToString name ++ " does not have a fixed cost" ]
                    )
                |> Results.map List.sum

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
        |> Results.combineMap getCompanion
        |> Results.andThen
            (\companions ->
                Results.map
                    (\calculatedCost -> forFree companions - calculatedCost)
                    (totalCompanionCost companions)
            )
        |> Results.map powerToPoints


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


getCompanion : Companion -> Results ( Maybe Faction, Companion.Details )
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
            Oks p

        Nothing ->
            Errs [ "Companion " ++ Types.companionToString companion ++ " not found" ]



-- Relics --


relicsValue : Model key -> Results Points
relicsValue model =
    resultSum (relicCost model.class model.cosmicPearl) model.relics
        |> Results.map (\cost -> { zero | rewardPoints = -cost })


relicCost : Maybe Class -> CosmicPearlData -> RankedRelic -> Results Int
relicCost class pearl { name, cost } =
    find "Relic" .name name Relic.all Types.relicToString
        |> Results.map
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


conversion : Model key -> Results Points
conversion model =
    Oks
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


resultSum : (item -> Results Int) -> List item -> Results Int
resultSum toValue list =
    list
        |> Results.combineMap toValue
        |> Results.map List.sum


resultsSum : List (Results Points) -> Results Points
resultsSum list =
    list
        |> Results.combine
        |> Results.map (List.foldl sum zero)


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
