module Data.Costs.Factions exposing (value)

import Data.Costs.Monad as Monad exposing (Monad)
import Data.Costs.Utils exposing (Points, zero)
import Types exposing (Model)


value : Model key -> Monad Points
value model =
    case model.faction of
        Nothing ->
            Monad.succeed { zero | power = 4 }

        Just ( _, False ) ->
            Monad.succeed { zero | power = 2 }

        Just ( _, True ) ->
            Monad.succeed zero
