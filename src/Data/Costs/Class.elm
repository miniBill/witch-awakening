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
                Warlock ->
                    Monad.succeed { zero | rewardPoints = 20 }

                Academic ->
                    Monad.succeed zero

                Sorceress ->
                    Monad.succeed zero

        Nothing ->
            Monad.error "You need to select a class"
