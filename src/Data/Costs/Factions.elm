module Data.Costs.Factions exposing (value)

import Data.Costs.Monad as Monad exposing (Monad)
import Data.Costs.Points as Points exposing (Points)
import Data.Costs.Utils as Utils
import Data.Costs.Value as Value
import Generated.Faction as Faction
import Types exposing (IdKind(..), Model)


value : Model key -> Monad Points
value model =
    let
        fromFactions : List (Monad Points)
        fromFactions =
            case model.factions of
                [] ->
                    Monad.succeed (Points.fromPower 2)
                        |> Monad.withPointsInfo IdKindFaction "Factionless"
                        |> List.singleton

                _ ->
                    model.factions
                        |> Monad.combineMap
                            (\name ->
                                Monad.succeed Points.zero
                                    |> Monad.withPointsInfo IdKindFaction (Faction.toString name)
                            )
                        |> Monad.map Points.sum
                        |> (if List.length model.factions > 2 then
                                Monad.withWarning "Cannot take more than two Factions"

                            else
                                identity
                           )
                        |> List.singleton

        fromPerks : List (Monad Points)
        fromPerks =
            case model.factionPerks of
                [] ->
                    [ Monad.succeed (Points.fromPower 2)
                        |> Monad.withPointsInfo IdKindFaction "No faction magic"
                    ]

                _ ->
                    model.factionPerks
                        |> Monad.mapAndSum
                            (\factionPerk ->
                                Utils.find "Faction" .name factionPerk Faction.all Faction.toString
                                    |> Monad.andThen
                                        (\factionDetails ->
                                            Monad.succeed Points.zero
                                                |> Monad.withInfo
                                                    { kind = IdKindFaction
                                                    , anchor = Just (Faction.toString factionPerk)
                                                    , label = factionDetails.perk
                                                    , value = Value.PowerAndRewardPoints Points.zero
                                                    }
                                        )
                            )
                        |> (if List.length model.factionPerks > 2 then
                                Monad.withWarning "Cannot take more than two Faction Perks"

                            else
                                identity
                           )
                        |> List.singleton
    in
    (fromFactions ++ fromPerks)
        |> Monad.combineAndSum
