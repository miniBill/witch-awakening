module Data.Costs.Class exposing (value)

import Data.Costs.Monad as Monad exposing (Monad)
import Data.Costs.Points as Points exposing (Points)
import Generated.Types exposing (Class(..))
import Types exposing (Model)


value : Model key -> Monad Points
value model =
    case model.class of
        Just class ->
            case class of
                ClassWarlock ->
                    Monad.succeed (Points.fromRewardPoints 20)

                ClassAcademic ->
                    Monad.succeed Points.zero

                ClassSorceress ->
                    Monad.succeed Points.zero

        Nothing ->
            Monad.succeed Points.zero
                |> Monad.withWarning "You need to select a class"
