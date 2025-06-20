module Data.Costs exposing (classValue, factionValue, startingValue, totalCost, totalRewards, typePerksValue)

import Data.Costs.Companions
import Data.Costs.Complications
import Data.Costs.Magic
import Data.Costs.Monad as Monad exposing (Monad, succeed)
import Data.Costs.Perks
import Data.Costs.Relics
import Data.Costs.Utils as Utils exposing (Points, zero)
import Generated.TypePerk
import Generated.Types as Types exposing (Class(..), GameMode(..), Race(..))
import Types exposing (Model)



-- Total --


totalCost : Model key -> Monad Points
totalCost model =
    [ classValue model
    , startingValue model
    , Data.Costs.Complications.value model
    , typePerksValue model
    , Data.Costs.Magic.value { ignoreSorceressBonus = False } model
    , Data.Costs.Perks.value model
    , factionValue model
    , Data.Costs.Companions.value model
    , Data.Costs.Relics.value model
    , conversion model

    -- Just grab info and warnings from the power cap
    , Monad.map Utils.zeroOut (Data.Costs.Complications.powerCap model)
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
                    Utils.slotUnsupported

                Just Constellation ->
                    Utils.slotUnsupported

                Nothing ->
                    if model.capBuild then
                        withGameModeInfo "Normal game mode (cap)" 100

                    else
                        withGameModeInfo "Normal game mode" 30
    in
    Monad.map Utils.powerToPoints power



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
