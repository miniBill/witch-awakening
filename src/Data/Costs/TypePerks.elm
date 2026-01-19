module Data.Costs.TypePerks exposing (value)

import Data.Costs.Monad as Monad exposing (Monad)
import Data.Costs.Points as Points exposing (Points)
import Data.Costs.Utils as Utils
import Data.Race as Race
import Generated.TypePerk
import Generated.Types as Types exposing (Race)
import Types exposing (IdKind(..), Model)
import View.Race


value : Model key -> Monad Points
value model =
    model.typePerks
        |> Monad.mapAndSum (typePerkValue model)
        |> checkForGenie model


checkForGenie : Model key -> Monad p -> Monad p
checkForGenie model p =
    if List.any Race.isGenie model.races && not (List.any Race.isGenie model.typePerks) then
        p
            |> Monad.withWarning "Genies need to take their type perk"

    else
        p


typePerkValue : Model key -> Race -> Monad Points
typePerkValue model race =
    Utils.find "Type perk" .race race Generated.TypePerk.all View.Race.raceToShortString
        |> Monad.andThen
            (\found ->
                Points.fromPower -found.cost
                    |> Monad.succeed
                    |> Monad.withPointsInfo IdKindTypePerk (View.Race.raceToShortString race)
                    |> (if List.any (Types.isSameRace race) model.races then
                            identity

                        else
                            Monad.withWarning ("Selected type perk from a race you don't have: " ++ View.Race.raceToShortString race)
                       )
            )
