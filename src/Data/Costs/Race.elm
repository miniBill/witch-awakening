module Data.Costs.Race exposing (value)

import Data.Costs.Monad as Monad exposing (Monad)
import Data.Costs.Points as Points exposing (Points)
import Generated.Types as Types
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
        |> checkNormalWizard model


checkNormalWizard : Model key -> Monad a -> Monad a
checkNormalWizard model =
    case model.class of
        Just Types.ClassWizard ->
            if List.member Types.RaceNeutral model.races then
                case model.mainRace of
                    Nothing ->
                        identity

                    Just mainRace ->
                        if mainRace == Types.RaceNeutral then
                            identity

                        else
                            Monad.withWarning "Wizards must pick Normal as their main race"

            else
                Monad.withWarning "Wizards must pick Normal as their main race"

        _ ->
            identity
