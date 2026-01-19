module Data.Costs.Race exposing (value)

import Data.Costs.Monad as Monad exposing (Monad)
import Data.Costs.Points as Points exposing (Points)
import Types exposing (IdKind(..), Model)
import View.Race


value : Model key -> Monad Points
value model =
    model.races
        |> Monad.mapAndSum
            (\race ->
                Points.zero
                    |> Monad.succeed
                    |> Monad.withPointsInfo IdKindRace (View.Race.raceToShortString race)
            )
