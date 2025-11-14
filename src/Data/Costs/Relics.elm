module Data.Costs.Relics exposing (value)

import Data.Costs.Monad as Monad exposing (Monad)
import Data.Costs.Utils as Utils exposing (Points, zero)
import Generated.Relic as Relic
import Generated.Types exposing (Relic(..))
import Types exposing (IdKind(..), Model, RankedRelic)
import View.Relic


value : Model key -> Monad Points
value model =
    model.relics
        |> Monad.mapAndSum (relicCost model)
        |> Monad.map (\rp -> { zero | rewardPoints = -rp })


relicCost : Model key -> RankedRelic -> Monad Int
relicCost ({ class } as model) details =
    Utils.find "Relic" .name details.name (Relic.all model.relics) View.Relic.relicToShortString
        |> Monad.andThen
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
                            details.cost - 2

                        else
                            details.cost

                    multiplier : Int
                    multiplier =
                        case relic.name of
                            RelicCosmicPearl cosmicPearl ->
                                max 1 <| List.length cosmicPearl.add + List.length cosmicPearl.change

                            _ ->
                                1
                in
                (baseCost * multiplier)
                    |> Utils.checkRequirements relic (Relic.toString details.name) model
            )
        |> Monad.withRewardInfo IdKindRelic (View.Relic.relicToShortString details.name)
