module Data.Costs exposing (startingValue, totalCost, totalRewards)

import Data.Costs.Class
import Data.Costs.Companions
import Data.Costs.Complications
import Data.Costs.Factions
import Data.Costs.Magic
import Data.Costs.Monad as Monad exposing (Monad, succeed)
import Data.Costs.Perks
import Data.Costs.Quests
import Data.Costs.Race
import Data.Costs.Relics
import Data.Costs.TypePerks
import Data.Costs.Utils as Utils exposing (Points, zero)
import Generated.Types exposing (GameMode(..), Race(..))
import Types exposing (Model)



-- Total --


totalCost : Model key -> Monad Points
totalCost model =
    [ Data.Costs.Class.value model
    , startingValue model
    , Data.Costs.Race.value model
    , Data.Costs.Complications.value model
    , -model.towardsCap |> Utils.powerToPoints |> Monad.succeed
    , Data.Costs.TypePerks.value model
    , Data.Costs.Magic.value { ignoreSorceressBonus = False } model
    , Data.Costs.Perks.value model
    , Data.Costs.Factions.value model
    , Data.Costs.Companions.value model
    , Data.Costs.Quests.value model
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
    [ Data.Costs.Class.value model
    , conversion model
    ]
        |> Utils.combineAndSum



-- Starting power  --


startingValue : Model key -> Monad Points
startingValue model =
    startingPower model
        |> Monad.map Utils.powerToPoints


startingPower : Model key -> Monad Int
startingPower model =
    let
        withGameModeInfo : String -> Int -> Monad Int
        withGameModeInfo label v =
            succeed v
                |> Monad.withInfo
                    { label = label
                    , anchor = Just "Game mode"
                    , value = Monad.Power v
                    }
    in
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


conversion : Model key -> Monad Points
conversion model =
    { zero
        | power = -model.powerToRewards
        , rewardPoints = model.powerToRewards
    }
        |> succeed
