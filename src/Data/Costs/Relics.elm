module Data.Costs.Relics exposing (value)

import Data.Costs.Monad as Monad exposing (Monad)
import Data.Costs.Points as Points exposing (Points)
import Data.Costs.Utils as Utils
import Generated.Relic as Relic
import Types exposing (IdKind(..), Model, RankedRelic)
import View.Relic


value : Model key -> Monad Points
value model =
    model.relics
        |> Monad.mapAndSum (relicValue model)


relicValue : Model key -> RankedRelic -> Monad Points
relicValue ({ class } as model) details =
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
                in
                -details.cost
                    |> Utils.applyClassBonusIf isClass
                    |> Points.fromRewardPoints
                    |> Utils.checkRequirements relic (Relic.toString details.name) model
            )
        |> Monad.withPointsInfo IdKindRelic (View.Relic.relicToShortString details.name)
