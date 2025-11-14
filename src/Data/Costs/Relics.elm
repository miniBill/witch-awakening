module Data.Costs.Relics exposing (value)

import Data.Costs.Monad as Monad exposing (Monad)
import Data.Costs.Utils as Utils exposing (Points)
import Generated.Relic as Relic
import Types exposing (IdKind(..), Model, RankedRelic)
import View.Relic


value : Model key -> Monad Points
value model =
    model.relics
        |> Monad.mapAndSum (relicValue model)
        |> Monad.map Utils.rewardPointsToPoints


relicValue : Model key -> RankedRelic -> Monad Int
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
                details.cost
                    |> Utils.applyClassBonusIf isClass
                    |> negate
                    |> Utils.checkRequirements relic (Relic.toString details.name) model
            )
        |> Monad.withRewardInfo IdKindRelic (View.Relic.relicToShortString details.name)
