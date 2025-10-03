module Data.Costs.TypePerks exposing (value)

import Data.Costs.Monad as Monad exposing (Monad)
import Data.Costs.Utils as Utils exposing (Points)
import Generated.TypePerk
import Generated.Types as Types exposing (Race)
import Types exposing (IdKind(..), Model)
import View.Race


value : Model key -> Monad Points
value model =
    model.typePerks
        |> Monad.mapAndSum (typePerkCost model)
        |> Monad.map (\p -> Utils.powerToPoints -p)


typePerkCost : Model key -> Race -> Monad Int
typePerkCost model race =
    Utils.find "Type perk" .race race Generated.TypePerk.all View.Race.raceToShortString
        |> Monad.andThen
            (\{ cost } ->
                Monad.succeed cost
                    |> Monad.withInfo
                        { label = View.Race.raceToShortString race
                        , kind = IdKindTypePerk
                        , anchor = Just (View.Race.raceToShortString race)
                        , value = Monad.Power cost
                        }
                    |> (if List.any (Types.isSameRace race) model.races then
                            identity

                        else
                            Monad.withWarning ("Selected type perk from a race you don't have: " ++ View.Race.raceToShortString race)
                       )
            )
