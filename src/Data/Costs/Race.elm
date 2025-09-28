module Data.Costs.Race exposing (value)

import Data.Costs.Monad as Monad exposing (Monad)
import Data.Costs.Utils as Utils exposing (Points)
import Types exposing (Model)
import View.Race


value : Model key -> Monad Points
value model =
    let
        inner : Monad Points
        inner =
            model.races
                |> Monad.mapAndSum
                    (\race ->
                        Monad.succeed 0
                            |> Monad.withPowerInfo (View.Race.raceToShortString race)
                    )
                |> Monad.map Utils.powerToPoints
    in
    case model.races of
        [ _ ] ->
            inner

        [] ->
            inner
                |> Monad.withWarning "You need to select a race."

        _ ->
            case model.mainRace of
                Nothing ->
                    inner
                        |> Monad.withWarning "You need to select a main race."

                Just _ ->
                    inner
