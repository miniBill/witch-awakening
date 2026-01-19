module Data.Costs exposing (startingValue, totalValue)

import Data.Costs.Class
import Data.Costs.Companions
import Data.Costs.Complications
import Data.Costs.Factions
import Data.Costs.Magic
import Data.Costs.Monad as Monad exposing (Monad, succeed)
import Data.Costs.Perks
import Data.Costs.Points as Points exposing (Points)
import Data.Costs.Quests
import Data.Costs.Race
import Data.Costs.Relics
import Data.Costs.TypePerks
import Data.Costs.Utils as Utils
import Data.Costs.Value as Value
import Generated.Types exposing (GameMode(..))
import Types exposing (IdKind(..), Model)



-- Total --


totalValue : Model key -> Monad Points
totalValue model =
    [ Data.Costs.Class.value model
    , startingValue model
    , Data.Costs.Race.value model
    , Data.Costs.Complications.value model
    , if model.capBuild then
        Monad.succeed Points.zero

      else
        Points.fromPower -model.towardsCap |> Monad.succeed
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
        |> Monad.combineAndSum
        |> Monad.andThen
            (\result ->
                let
                    warningIf : Int -> String -> Monad q -> Monad q
                    warningIf b warning acc =
                        if b < 0 then
                            acc |> Monad.withWarning warning

                        else
                            acc
                in
                succeed result
                    |> warningIf result.rewardPoints "Not enough reward points! Try converting some power."
                    |> warningIf result.power
                        (if model.gameMode == Just GameModeStoryArc && not model.capBuild then
                            "Not enough power!"

                         else
                            "Not enough power! Try adding complications."
                        )
            )



-- Starting power  --


startingValue : Model key -> Monad Points
startingValue model =
    startingPower model
        |> Monad.map Points.fromPower


startingPower : Model key -> Monad Int
startingPower model =
    let
        withGameModeInfo : String -> Int -> Monad Int
        withGameModeInfo label v =
            succeed v
                |> Monad.withInfo
                    { label = label
                    , kind = IdKindGameMode
                    , anchor = Just "Game mode"
                    , value = Value.fromPower v
                    }
    in
    case model.gameMode of
        Just GameModeStoryArc ->
            if model.capBuild then
                withGameModeInfo "Story Arc (cap)" 150

            else
                withGameModeInfo "Story Arc" 10

        Just GameModeEarlyBird ->
            withGameModeInfo "Early Bird" 75

        Just GameModeSkillTree ->
            Utils.slotUnsupported

        Just GameModeConstellation ->
            Utils.slotUnsupported

        Nothing ->
            if model.capBuild then
                withGameModeInfo "Normal game mode (cap)" 100

            else
                withGameModeInfo "Normal game mode" 30


conversion : Model key -> Monad Points
conversion model =
    { power = -model.powerToRewards
    , rewardPoints = model.powerToRewards
    }
        |> succeed
