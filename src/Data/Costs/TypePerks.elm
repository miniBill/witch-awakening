module Data.Costs.TypePerks exposing (value)

import Data.Costs.Monad as Monad exposing (Monad)
import Data.Costs.Utils as Utils exposing (Points)
import Generated.TypePerk
import Generated.Types exposing (Race)
import Types exposing (Model)
import View.Race


value : Model key -> Monad Points
value model =
    model.typePerks
        |> Monad.mapAndSum typePerkCost
        |> Monad.map (\p -> Utils.powerToPoints -p)


typePerkCost : Race -> Monad Int
typePerkCost race =
    Utils.find "Type perk" .race race Generated.TypePerk.all View.Race.raceToShortString
        |> Monad.andThen
            (\{ cost } ->
                Monad.succeed cost
                    |> Monad.withInfo
                        { label = View.Race.raceToShortString race
                        , anchor = Just ("perk-" ++ View.Race.raceToShortString race)
                        , value = Monad.Power cost
                        }
            )
