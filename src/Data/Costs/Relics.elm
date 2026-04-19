module Data.Costs.Relics exposing (value)

import Data.Costs.Monad as Monad exposing (Monad)
import Data.Costs.Points as Points exposing (Points)
import Data.Costs.Utils as Utils
import Data.Costs.Value as Value
import Generated.Relic as Relic
import Generated.Types as Types
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
                    nameKey : String
                    nameKey =
                        View.Relic.relicToShortString details.name
                in
                if relic.name == Types.RelicHeirloom && class == Just Types.ClassWizard then
                    Points.zero
                        |> Monad.succeed
                        |> Monad.withInfo
                            { label = nameKey
                            , kind = IdKindRelic
                            , anchor = Just nameKey
                            , value = Value.FreeBecause "[Wizard]"
                            }

                else
                    let
                        isClass : Bool
                        isClass =
                            case class of
                                Nothing ->
                                    False

                                Just Types.ClassWizard ->
                                    not (List.isEmpty relic.classes)

                                Just c ->
                                    List.member c relic.classes
                    in
                    -details.cost
                        |> Utils.applyClassBonusIf isClass
                        |> Points.fromRewardPoints
                        |> Utils.checkRequisites relic (Relic.toString details.name) model
                        |> Monad.withPointsInfo IdKindRelic nameKey
            )
