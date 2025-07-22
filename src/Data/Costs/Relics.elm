module Data.Costs.Relics exposing (value)

import Data.Costs.Monad as Monad exposing (Monad)
import Data.Costs.Utils as Utils exposing (Points, zero)
import Generated.Relic
import Generated.Types as Types exposing (Class, Relic(..))
import Types exposing (CosmicPearlData, Model, RankedRelic)


value : Model key -> Monad Points
value model =
    model.relics
        |> Monad.mapAndSum (relicCost model.class model.cosmicPearl)
        |> Monad.map (\rp -> { zero | rewardPoints = -rp })


relicCost : Maybe Class -> CosmicPearlData -> RankedRelic -> Monad Int
relicCost class pearl details =
    Utils.find "Relic" .name details.name Generated.Relic.all Types.relicToString
        |> Monad.map
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
                        if details.name == CosmicPearl then
                            max 1 <| List.length pearl.add + List.length pearl.change

                        else
                            1
                in
                baseCost * multiplier
            )
        |> Monad.withRewardInfo (Types.relicToString details.name)
