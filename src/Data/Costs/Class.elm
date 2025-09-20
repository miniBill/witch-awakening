module Data.Costs.Class exposing (value)

import Data.Costs.Monad as Monad exposing (Monad)
import Data.Costs.Utils exposing (Points, zero)
import Generated.Types exposing (Class(..))
import Types exposing (Model)


value : Model key -> Monad Points
value model =
    case model.class of
        Just class ->
            case class of
                ClassWarlock ->
                    Monad.succeed { zero | rewardPoints = 20 }

                ClassAcademic ->
                    Monad.succeed zero

                ClassSorceress ->
                    Monad.succeed zero

        Nothing ->
            Monad.succeed zero
                |> Monad.withWarning "You need to select a class"
