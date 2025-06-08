module Data.Costs exposing (Points, classValue, companionsValue, complicationsRawValue, complicationsValue, factionValue, magicsValue, negate, perkCost, perksValue, powerCap, powerToPoints, relicsValue, startingValue, totalCost, totalRewards, typePerksValue, zero)

import Data.Affinity as Affinity
import Data.Companion as Companion
import Data.Complication as Complication
import Data.Costs.Monad as Monad exposing (Monad, andThen, combine, map, map2, mapAndSum, succeed, withWarning)
import Data.FactionalMagic as FactionalMagic
import Data.Magic as Magic
import Generated.Companions
import Generated.Complications
import Generated.Magics
import Generated.Perks as Perk
import Generated.Relics
import Generated.TypePerks
import Generated.Types as Types exposing (Affinity, Class(..), Companion, Faction(..), GameMode(..), Magic(..), Perk(..), Race(..), Relic(..), companionToString)
import List.Extra
import Maybe.Extra
import Types exposing (ComplicationKind(..), CosmicPearlData, Model, RankedMagic, RankedPerk, RankedRelic)


type alias Points =
    { power : Int
    , rewardPoints : Int
    }


zero : Points
zero =
    { power = 0
    , rewardPoints = 0
    }


slotUnsupported : Monad value
slotUnsupported =
    Monad.error "Slot modes not supported yet"


capWithWarning : Int -> String -> Int -> Monad Points
capWithWarning cap warning value =
    if value > cap then
        { zero
            | power = cap
        }
            |> succeed
            |> withWarning warning

    else
        { zero | power = value }
            |> succeed


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


totalCost : Model key -> Monad Points
totalCost model =
    [ map negate <| classValue model
    , map negate <| startingValue model
    , map negate <| complicationsValue model
    , map negate <| typePerksValue model
    , map negate <| magicsValue model
    , perksCost model
    , factionValue model |> negate |> succeed
    , map negate <| companionsValue model
    , map negate <| relicsValue model
    , map negate <| conversion model
    , map zeroOut <| powerCap model
    ]
        |> combineAndSum
        |> andThen
            (\result ->
                let
                    warningIf : Int -> String -> Monad q -> Monad q
                    warningIf b warning acc =
                        if b > 0 then
                            acc |> withWarning warning

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
                }
                    |> succeed
                    |> warningIf result.rewardPoints "Not enough reward points! Try converting some power."
                    |> warningIf result.power
                        (if model.gameMode == Just StoryArc && not model.capBuild then
                            "Not enough power!"

                         else
                            "Not enough power! Try adding complications."
                        )
            )


totalRewards : Model key -> Monad Points
totalRewards model =
    [ classValue model
    , conversion model
    ]
        |> combineAndSum



-- Class --


classValue : Model key -> Monad Points
classValue model =
    case model.class of
        Just class ->
            case class of
                Warlock ->
                    succeed { zero | rewardPoints = 20 }

                Academic ->
                    succeed zero

                Sorceress ->
                    succeed zero

        Nothing ->
            Monad.error "You need to select a class"



-- Power cap --


powerCap : Model key -> Monad Points
powerCap model =
    case model.gameMode of
        Nothing ->
            model.towardsCap
                |> capWithWarning 30 normalCapWarning
                |> map (sum { zero | power = 100 })

        Just StoryArc ->
            complicationsRawValue model
                |> andThen (capWithWarning 60 storyArcWarning)
                |> map (sum { zero | power = 150 })

        Just EarlyBird ->
            complicationsRawValue model
                |> andThen (capWithWarning 30 earlyBirdWarning)
                |> map (sum { zero | power = 75 })

        Just SkillTree ->
            slotUnsupported

        Just Constellation ->
            slotUnsupported



-- Starting power  --


startingValue : Model key -> Monad Points
startingValue model =
    let
        power : Monad Int
        power =
            case model.gameMode of
                Just StoryArc ->
                    if model.capBuild then
                        succeed 150
                            |> Monad.withInfo "Story Arc (cap): 150"

                    else
                        succeed 10
                            |> Monad.withInfo "Story Arc: 10"

                Just EarlyBird ->
                    succeed 75
                        |> Monad.withInfo "Early Bird: 75"

                Just SkillTree ->
                    slotUnsupported

                Just Constellation ->
                    slotUnsupported

                Nothing ->
                    if model.capBuild then
                        succeed 100
                            |> Monad.withInfo "Normal game mode (cap): 100"

                    else
                        succeed 30
                            |> Monad.withInfo "Normal game mode: 30"
    in
    map powerToPoints power



-- Complications --


complicationsValue : Model key -> Monad Points
complicationsValue model =
    complicationsRawValue model
        |> andThen
            (\value ->
                case model.gameMode of
                    Just StoryArc ->
                        if model.capBuild then
                            value
                                |> capWithWarning 60 storyArcWarning

                        else
                            succeed zero

                    Just EarlyBird ->
                        value
                            |> capWithWarning 30 earlyBirdWarning

                    Just SkillTree ->
                        slotUnsupported

                    Just Constellation ->
                        slotUnsupported

                    Nothing ->
                        if model.capBuild then
                            model.towardsCap
                                |> capWithWarning 30 normalInitialWarning

                        else
                            (value - model.towardsCap)
                                |> capWithWarning 30 normalInitialWarning
            )


complicationsRawValue : Model key -> Monad Int
complicationsRawValue model =
    model.complications
        |> List.map (complicationValue model)
        |> combine
        |> map List.sum


complicationValue : Model key -> Types.RankedComplication -> Monad Int
complicationValue model complication =
    let
        get : Int -> List a -> Monad a
        get tier list =
            case List.Extra.getAt (tier - 1) list of
                Just v ->
                    succeed v

                Nothing ->
                    Monad.error <| "Could not get tier " ++ String.fromInt tier ++ " for complication " ++ Types.complicationToString complication.name
    in
    case List.Extra.find (\{ name } -> name == complication.name) Generated.Complications.all of
        Nothing ->
            Monad.error <| "Could not find complication " ++ Types.complicationToString complication.name

        Just details ->
            let
                raw : Monad Int
                raw =
                    case ( details.content, complication.kind ) of
                        ( Complication.Single value _, _ ) ->
                            succeed value

                        ( Complication.WithTiers _ tiers _, Tiered tier ) ->
                            map Tuple.second <| get tier tiers

                        ( Complication.WithChoices _ choices _, Tiered tier ) ->
                            map Tuple.second <| get tier choices

                        ( Complication.WithGains costs _, Tiered tier ) ->
                            get tier costs

                        ( _, Nontiered ) ->
                            Monad.error <| "Need a tier for complication " ++ Types.complicationToString complication.name
            in
            raw
                |> map
                    (\r ->
                        let
                            bonus : Int
                            bonus =
                                if details.class /= Nothing && details.class == model.class then
                                    2

                                else
                                    0
                        in
                        r + bonus
                    )
                |> Monad.andThen
                    (\v ->
                        succeed v
                            |> Monad.withInfo
                                (Types.complicationToString details.name ++ ": " ++ String.fromInt v)
                    )



-- Type perks --


typePerksValue : Model key -> Monad Points
typePerksValue model =
    mapAndSum typePerkValue model.typePerks
        |> map powerToPoints


typePerkValue : Race -> Monad Int
typePerkValue race =
    find "Type perk" .race race Generated.TypePerks.all Types.raceToString
        |> Monad.andThen
            (\{ cost } ->
                Monad.succeed -cost
                    |> Monad.withInfo
                        (Types.raceToString race ++ ": " ++ String.fromInt -cost)
            )


find : String -> (item -> key) -> key -> List item -> (key -> String) -> Monad item
find label toKey value list toString =
    case List.Extra.find (\candidate -> toKey candidate == value) list of
        Nothing ->
            Monad.error <| label ++ " " ++ toString value ++ " not found"

        Just v ->
            succeed v



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
    -> Monad Points
magicsValue model =
    let
        affinities : List Affinity
        affinities =
            Affinity.fromModel model
    in
    model.magic
        |> List.map (magicValue affinities model)
        |> combine
        |> map List.sum
        |> map powerToPoints


magicValue :
    List Affinity
    ->
        { a
            | faction : Maybe ( Faction, Bool )
            , class : Maybe Class
            , typePerks : List Race
        }
    -> RankedMagic
    -> Monad Int
magicValue affinities { faction, class, typePerks } { name, rank } =
    case
        Generated.Magics.all
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
            succeed cost
                |> Monad.withInfo (Types.magicToString name ++ ": " ++ String.fromInt cost)

        Nothing ->
            Monad.error <| "Magic " ++ Types.magicToString name ++ " not found"


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
    -> Monad Points
perksValue model =
    mapAndSum (perkCost model) model.perks
        |> map powerToPoints
        |> map negate


perksCost :
    { a
        | races : List Race
        , mainRace : Maybe Race
        , cosmicPearl : CosmicPearlData
        , typePerks : List Race
        , perks : List RankedPerk
        , class : Maybe Class
    }
    -> Monad Points
perksCost model =
    perksValue model
        |> map negate


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
    -> Monad Int
perkCost ({ class } as model) { name, cost } =
    find "Perk" .name name (Perk.all model.perks) Types.perkToString
        |> andThen
            (\perk ->
                let
                    affinities : List Affinity
                    affinities =
                        Affinity.fromModel model

                    isClass : Bool
                    isClass =
                        Just perk.class == class

                    isAffinity : Bool
                    isAffinity =
                        List.member perk.affinity affinities

                    changelingDiff : Int
                    changelingDiff =
                        case name of
                            ChargeSwap _ ->
                                if List.member Changeling model.races then
                                    -3

                                else
                                    0

                            _ ->
                                0

                    finalCost : Int
                    finalCost =
                        (cost + changelingDiff)
                            |> applyClassBonusIf isClass
                            |> halveIfPositiveAnd isAffinity
                in
                finalCost
                    |> succeed
                    |> Monad.withInfo (Types.perkToString name ++ ": " ++ String.fromInt -finalCost)
            )



-- Faction --


factionValue : Model key -> Points
factionValue model =
    case model.faction of
        Nothing ->
            { zero | power = 4 }

        Just ( _, False ) ->
            { zero | power = 2 }

        Just ( _, True ) ->
            zero



-- Companions --


companionsValue : Model key -> Monad Points
companionsValue model =
    let
        totalCompanionCost : List ( Maybe Faction, Companion.Details ) -> Monad Points
        totalCompanionCost companions =
            companions
                |> List.map
                    (\( _, { name, cost } ) ->
                        case cost of
                            Just v ->
                                succeed v

                            Nothing ->
                                Monad.error <| "Companion " ++ Types.companionToString name ++ " does not have a fixed cost"
                    )
                |> combine
                |> map
                    (\points ->
                        points
                            |> List.sum
                            |> powerToPoints
                    )

        forFree : List ( Maybe Faction, Companion.Details ) -> Monad Points
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
                                Maybe.map
                                    (\cost -> ( f, cost, c ))
                                    c.cost
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

                sameKind : List ( String, Int, Companion.Details )
                sameKind =
                    byCost
                        |> List.filterMap
                            (\( _, cost, companion ) ->
                                if sameRace companion model.races then
                                    Just ( "Same race", cost, companion )

                                else if sameClass companion model.class then
                                    Just ( "Same class", cost, companion )

                                else
                                    Nothing
                            )
                        |> List.take
                            (if treasure then
                                4

                             else
                                2
                            )

                withReason :
                    String
                    -> List ( Int, Companion.Details )
                    -> List ( String, Int, Companion.Details )
                withReason label group =
                    List.map
                        (\( cost, details ) -> ( label, cost, details ))
                        group

                tryPick : List (List ( String, Int, Companion.Details )) -> Monad Points
                tryPick lists =
                    lists
                        |> List.Extra.removeWhen List.isEmpty
                        |> List.Extra.cartesianProduct
                        |> List.map
                            (\picked ->
                                let
                                    unique : List ( String, Int, Companion.Details )
                                    unique =
                                        picked
                                            |> List.Extra.uniqueBy (\( _, _, { name } ) -> name)
                                in
                                { value =
                                    { zero
                                        | power =
                                            unique
                                                |> List.map (\( _, cost, _ ) -> cost)
                                                |> List.sum
                                    }
                                , warnings = []
                                , infos =
                                    unique
                                        |> List.map
                                            (\( label, _, { name } ) ->
                                                companionToString name
                                                    ++ " is free ("
                                                    ++ label
                                                    ++ ")"
                                            )
                                }
                            )
                        |> List.Extra.maximumBy (\{ value } -> value.power)
                        |> Maybe.withDefault
                            { value = zero
                            , warnings = []
                            , infos = []
                            }
                        |> Ok
            in
            if treasure then
                let
                    possiblyFriendly : List ( Int, Companion.Details )
                    possiblyFriendly =
                        List.filterMap
                            (\( f, cost, c ) ->
                                if f == Just TheOutsiders || f == Just AlphazonIndustries || f == Just TheCollegeOfArcadia then
                                    Nothing

                                else
                                    Just ( cost, c )
                            )
                            byCost
                in
                tryPick
                    [ withReason "Same faction" sameFaction
                    , withReason "Same faction - True Treasure" sameFaction
                    , sameKind
                    , withReason "Possibly friendly faction - True Treasure" possiblyFriendly
                    ]

            else
                tryPick
                    [ withReason "Same faction" sameFaction
                    , sameKind
                    ]
    in
    model.companions
        |> List.map getCompanion
        |> combine
        |> andThen
            (\companions ->
                map2
                    sum
                    (map negate (totalCompanionCost companions))
                    (forFree companions)
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


getCompanion : Companion -> Monad ( Maybe Faction, Companion.Details )
getCompanion companion =
    case
        List.Extra.findMap
            (\( faction, group ) ->
                List.Extra.findMap
                    (\({ name } as c) ->
                        if name == companion then
                            Just ( faction, c )

                        else
                            Nothing
                    )
                    group
            )
            Generated.Companions.all
    of
        Just p ->
            succeed p

        Nothing ->
            Monad.error <| "Companion " ++ Types.companionToString companion ++ " not found"



-- Relics --


relicsValue : Model key -> Monad Points
relicsValue model =
    mapAndSum (relicCost model.class model.cosmicPearl) model.relics
        |> map (\cost -> { zero | rewardPoints = -cost })


relicCost : Maybe Class -> CosmicPearlData -> RankedRelic -> Monad Int
relicCost class pearl { name, cost } =
    find "Relic" .name name Generated.Relics.all Types.relicToString
        |> map
            (\relic ->
                let
                    isClass : Bool
                    isClass =
                        case class of
                            Nothing ->
                                False

                            Just c ->
                                List.member c relic.classes

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


conversion : Model key -> Monad Points
conversion model =
    { zero
        | power = -model.powerToRewards
        , rewardPoints = model.powerToRewards
    }
        |> succeed



-- Utils --


negate : Points -> Points
negate p =
    { p | power = -p.power, rewardPoints = -p.rewardPoints }


sum : Points -> Points -> Points
sum l r =
    { power = l.power + r.power
    , rewardPoints = l.rewardPoints + r.rewardPoints
    }


sumPoints : List Points -> Points
sumPoints =
    List.foldl sum zero


combineAndSum : List (Monad Points) -> Monad Points
combineAndSum list =
    list
        |> combine
        |> map sumPoints


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
