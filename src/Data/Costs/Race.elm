module Data.Costs.Race exposing (value)

import Data.Costs.Monad as Monad exposing (Monad)
import Data.Costs.Utils as Utils exposing (Points)
import Types exposing (Model)
import View.Race


value : Model key -> Monad Points
value model =
    model.races
        |> Monad.mapAndSum
            (\race ->
                Monad.succeed 0
                    |> Monad.withPowerInfo (View.Race.raceToShortString race)
            )
        |> Monad.map Utils.powerToPoints
