module Data.Costs.Race exposing (value)

import Data.Costs.Monad as Monad exposing (Monad)
import Data.Costs.Utils as Utils exposing (Points)
import Generated.Types as Types
import Types exposing (Model)


value : Model key -> Monad Points
value model =
    model.races
        |> Monad.mapAndSum
            (\race ->
                Monad.succeed 0
                    |> Monad.withPowerInfo (Types.raceToString race)
            )
        |> Monad.map Utils.powerToPoints
