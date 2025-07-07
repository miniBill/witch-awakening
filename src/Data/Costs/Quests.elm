module Data.Costs.Quests exposing (value)

import Data.Costs.Monad as Monad exposing (Monad)
import Data.Costs.Utils as Utils exposing (Points, zero)
import Generated.Quest
import Generated.Types as Types exposing (Quest)
import Types exposing (Model)


value : Model key -> Monad Points
value model =
    model.quests
        |> Monad.mapAndSum questCost
        |> Monad.map (\rp -> { zero | rewardPoints = rp })
        |> Monad.withWarning "TODO: check max slots for quests"


questCost : Quest -> Monad Int
questCost named =
    Utils.find "Quest" .name named Generated.Quest.all Types.questToString
        |> Monad.map .reward
        |> Monad.withRewardInfo (Types.questToString named)
