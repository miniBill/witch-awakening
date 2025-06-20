module Data.Costs.TypePerks exposing (value)

import Data.Costs.Monad as Monad exposing (Monad)
import Data.Costs.Utils as Utils exposing (Points)
import Generated.TypePerk
import Generated.Types as Types exposing (Race)
import Types exposing (Model)


value : Model key -> Monad Points
value model =
    model.typePerks
        |> Monad.mapAndSum typePerkCost
        |> Monad.map (\p -> Utils.powerToPoints -p)


typePerkCost : Race -> Monad Int
typePerkCost race =
    Utils.find "Type perk" .race race Generated.TypePerk.all Types.raceToString
        |> Monad.andThen
            (\{ cost } ->
                Monad.succeed cost
                    |> Monad.withInfo
                        { label = Types.raceToString race
                        , anchor = Just ("perk-" ++ Types.raceToString race)
                        , value = Monad.Power cost
                        }
            )
