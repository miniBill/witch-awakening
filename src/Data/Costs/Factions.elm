module Data.Costs.Factions exposing (value)

import Data.Costs.Monad as Monad exposing (Monad)
import Data.Costs.Utils as Utils exposing (Points)
import Generated.Faction as Faction
import Types exposing (IdKind(..), Model)


value : Model key -> Monad Points
value model =
    let
        fromFactions : List (Monad Int)
        fromFactions =
            case model.factions of
                [] ->
                    Monad.succeed 2
                        |> Monad.withPowerInfo IdKindFaction "Factionless"
                        |> List.singleton

                _ ->
                    model.factions
                        |> List.map
                            (\name ->
                                Monad.succeed 0
                                    |> Monad.withPowerInfo IdKindFaction (Faction.toString name)
                            )
                        |> Monad.mapAndSum identity
                        |> (if List.length model.factions > 2 then
                                Monad.withWarning "Cannot take more than two Factions"

                            else
                                identity
                           )
                        |> List.singleton

        fromPerks : List (Monad Int)
        fromPerks =
            case model.factionPerks of
                [] ->
                    [ Monad.succeed 2
                        |> Monad.withPowerInfo IdKindFaction "No faction magic"
                    ]

                _ ->
                    model.factionPerks
                        |> Monad.mapAndSum
                            (\factionPerk ->
                                Utils.find "Faction" .name factionPerk Faction.all Faction.toString
                                    |> Monad.andThen
                                        (\factionDetails ->
                                            Monad.succeed 0
                                                |> Monad.withInfo
                                                    { kind = IdKindFaction
                                                    , anchor = Just (Faction.toString factionPerk)
                                                    , label = factionDetails.perk
                                                    , value = Monad.Power 0
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
        |> Monad.mapAndSum identity
        |> Monad.map Utils.powerToPoints
