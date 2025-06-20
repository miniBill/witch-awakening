module Data.Costs exposing (classValue, complicationsRawValue, complicationsValue, factionValue, perkValue, perksValue, powerCap, startingValue, totalCost, totalRewards, typePerksValue)

import Data.Affinity as Affinity
import Data.Complication as Complication
import Data.Costs.Companions
import Data.Costs.Magic
import Data.Costs.Monad as Monad exposing (Monad, succeed)
import Data.Costs.Relics
import Data.Costs.Utils as Utils exposing (Points, zero)
import Generated.Complication
import Generated.Perk
import Generated.TypePerk
import Generated.Types as Types exposing (Affinity, Class(..), GameMode(..), Perk(..), Race(..))
import List.Extra
import Types exposing (ComplicationKind(..), CosmicPearlData, Model, RankedPerk)


slotUnsupported : Monad value
slotUnsupported =
    Monad.error "Slot modes not supported yet"


{-| Cap a value to a maximum. Emit a warning if the maximum is exceeded by the input.
-}
capWithWarning : Int -> String -> Int -> Monad Points
capWithWarning cap warning value =
    if value > cap then
        { zero
            | power = cap
        }
            |> Monad.succeed
            |> Monad.withWarning warning

    else
        { zero | power = value }
            |> Monad.succeed


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
    [ classValue model
    , startingValue model
    , complicationsValue model
    , typePerksValue model
    , Data.Costs.Magic.value { ignoreSorceressBonus = False } model
    , perksValue model
    , factionValue model
    , Data.Costs.Companions.value model
    , Data.Costs.Relics.value model
    , conversion model

    -- Just grab info and warnings
    , Monad.map Utils.zeroOut (powerCap model)
    ]
        |> Utils.combineAndSum
        |> Monad.map Utils.negate
        |> Monad.andThen
            (\result ->
                let
                    warningIf : Int -> String -> Monad q -> Monad q
                    warningIf b warning acc =
                        if b > 0 then
                            acc |> Monad.withWarning warning

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
        |> Utils.combineAndSum



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
                |> Monad.map (Utils.sum { zero | power = 100 })

        Just StoryArc ->
            complicationsRawValue model
                |> Monad.andThen (capWithWarning 60 storyArcWarning)
                |> Monad.map (Utils.sum { zero | power = 150 })

        Just EarlyBird ->
            complicationsRawValue model
                |> Monad.andThen (capWithWarning 30 earlyBirdWarning)
                |> Monad.map (Utils.sum { zero | power = 75 })

        Just SkillTree ->
            slotUnsupported

        Just Constellation ->
            slotUnsupported



-- Starting power  --


startingValue : Model key -> Monad Points
startingValue model =
    let
        withGameModeInfo : String -> Int -> Monad Int
        withGameModeInfo label v =
            succeed v
                |> Monad.withInfo
                    { label = label
                    , anchor = Just "Game mode"
                    , value = Monad.Power v
                    }

        power : Monad Int
        power =
            case model.gameMode of
                Just StoryArc ->
                    if model.capBuild then
                        withGameModeInfo "Story Arc (cap)" 150

                    else
                        withGameModeInfo "Story Arc" 10

                Just EarlyBird ->
                    withGameModeInfo "Early Bird" 75

                Just SkillTree ->
                    slotUnsupported

                Just Constellation ->
                    slotUnsupported

                Nothing ->
                    if model.capBuild then
                        withGameModeInfo "Normal game mode (cap)" 100

                    else
                        withGameModeInfo "Normal game mode" 30
    in
    Monad.map Utils.powerToPoints power



-- Complications --


complicationsValue : Model key -> Monad Points
complicationsValue model =
    complicationsRawValue model
        |> Monad.andThen
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
    Monad.mapAndSum (complicationValue model) model.complications


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
    case List.Extra.find (\{ name } -> name == complication.name) Generated.Complication.all of
        Nothing ->
            Monad.error <| "Could not find complication " ++ Types.complicationToString complication.name

        Just details ->
            let
                raw : Monad Int
                raw =
                    case ( details.content, complication.kind ) of
                        ( Complication.Single value _, _ ) ->
                            Monad.succeed value

                        ( Complication.WithTiers _ tiers _, Tiered tier ) ->
                            Monad.map Tuple.second <| get tier tiers

                        ( Complication.WithChoices _ choices _, Tiered tier ) ->
                            Monad.map Tuple.second <| get tier choices

                        ( Complication.WithGains costs _, Tiered tier ) ->
                            get tier costs

                        ( _, Nontiered ) ->
                            Monad.error <| "Need a tier for complication " ++ Types.complicationToString complication.name
            in
            raw
                |> Monad.map
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
                |> Monad.withPowerInfo
                    (Types.complicationToString details.name)



-- Type perks --


typePerksValue : Model key -> Monad Points
typePerksValue model =
    model.typePerks
        |> Monad.mapAndSum typePerkValue
        |> Monad.map Utils.powerToPoints


typePerkValue : Race -> Monad Int
typePerkValue race =
    Utils.find "Type perk" .race race Generated.TypePerk.all Types.raceToString
        |> Monad.andThen
            (\{ cost } ->
                let
                    value : Int
                    value =
                        -cost
                in
                Monad.succeed value
                    |> Monad.withInfo
                        { label = Types.raceToString race
                        , anchor = Just ("perk-" ++ Types.raceToString race)
                        , value = Monad.Power value
                        }
            )



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
    model.perks
        |> Monad.mapAndSum (perkValue model)
        |> Monad.map Utils.powerToPoints


perkValue :
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
perkValue ({ class } as model) { name, cost } =
    Utils.find "Perk" .name name (Generated.Perk.all model.perks) Types.perkToString
        |> Monad.map
            (\perk ->
                let
                    affinities : List Affinity
                    affinities =
                        Affinity.fromModel model

                    isClass : Bool
                    isClass =
                        Just perk.class == class

                    isInAffinity : Bool
                    isInAffinity =
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
                            |> Utils.applyClassBonusIf isClass
                            |> Utils.halveIfPositiveAnd isInAffinity
                in
                -finalCost
            )
        |> Monad.withPowerInfo (Types.perkToString name)



-- Faction --


factionValue : Model key -> Monad Points
factionValue model =
    case model.faction of
        Nothing ->
            succeed { zero | power = 4 }

        Just ( _, False ) ->
            succeed { zero | power = 2 }

        Just ( _, True ) ->
            succeed zero


conversion : Model key -> Monad Points
conversion model =
    { zero
        | power = -model.powerToRewards
        , rewardPoints = model.powerToRewards
    }
        |> succeed
