module Data.Costs.Relics exposing (value)

import Data.Costs.Monad as Monad exposing (Monad)
import Data.Costs.Utils as Utils exposing (Points, zero)
import Generated.Relic
import Generated.Types as Types exposing (Relic(..))
import Types exposing (IdKind(..), Model, RankedRelic)


value : Model key -> Monad Points
value model =
    model.relics
        |> Monad.mapAndSum (relicCost model)
        |> Monad.map (\rp -> { zero | rewardPoints = -rp })


relicCost : Model key -> RankedRelic -> Monad Int
relicCost ({ class, cosmicPearl } as model) details =
    Utils.find "Relic" .name details.name Generated.Relic.all Types.relicToString
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
                        if details.name == RelicCosmicPearl then
                            max 1 <| List.length cosmicPearl.add + List.length cosmicPearl.change

                        else
                            1
                in
                (baseCost * multiplier)
                    |> Utils.checkRequirements relic (Types.relicToString details.name) model
            )
        |> Monad.withRewardInfo IdKindRelic (Types.relicToString details.name)
