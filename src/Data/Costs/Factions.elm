module Data.Costs.Factions exposing (value)

import Data.Costs.Monad as Monad exposing (Monad)
import Data.Costs.Utils as Utils exposing (Points)
import Generated.Types as Types
import Types exposing (IdKind(..), Model)


value : Model key -> Monad Points
value model =
    (case model.faction of
        Nothing ->
            Monad.succeed 4
                |> Monad.withPowerInfo IdKindFaction "Factionless"

        Just ( name, False ) ->
            [ Monad.succeed 0
                |> Monad.withPowerInfo IdKindFaction (Types.factionToString name)
            , Monad.succeed 2
                |> Monad.withPowerInfo IdKindFaction "No faction magic"
            ]
                |> Monad.mapAndSum identity

        Just ( name, True ) ->
            Monad.succeed 0
                |> Monad.withPowerInfo IdKindFaction (Types.factionToString name)
    )
        |> Monad.map Utils.powerToPoints
