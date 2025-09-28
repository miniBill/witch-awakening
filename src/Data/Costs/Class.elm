module Data.Costs.Class exposing (value)

import Data.Costs.Monad as Monad exposing (Monad)
import Data.Costs.Utils exposing (Points, zero)
import Generated.Types exposing (Class(..))
import Types exposing (Model)


value : Model key -> Monad Points
value model =
    case model.classes of
        [ class ] ->
            valueFor class

        [] ->
            Monad.succeed zero
                |> Monad.withWarning "You need to select a class."

        _ ->
            case model.mainClass of
                Nothing ->
                    Monad.succeed zero
                        |> Monad.withWarning "You need to select a main class."

                Just mainClass ->
                    valueFor mainClass


valueFor : Class -> Monad Points
valueFor class =
    case class of
        ClassWarlock ->
            Monad.succeed { zero | rewardPoints = 20 }

        ClassAcademic ->
            Monad.succeed zero

        ClassSorceress ->
            Monad.succeed zero
