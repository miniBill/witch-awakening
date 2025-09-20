module Data.Costs.Quests exposing (value)

import Data.Costs.Monad as Monad exposing (Monad)
import Data.Costs.Utils as Utils exposing (Points, zero)
import Data.Quest as Quest
import Dict
import Dict.Extra
import Generated.Quest
import Generated.Types as Types exposing (Faction(..), Quest)
import List.Extra
import Types exposing (Model)


value : Model key -> Monad Points
value model =
    model.quests
        |> Monad.combineMap questCost
        |> Monad.andThen
            (\rpsAndSlots ->
                let
                    ( rps, questsDetails ) =
                        List.unzip rpsAndSlots

                    slotsChecks : Monad (List ())
                    slotsChecks =
                        questsDetails
                            |> (case model.faction of
                                    Nothing ->
                                        List.Extra.removeWhen (\details -> details.faction == Just FactionIndependents)

                                    Just ( faction, _ ) ->
                                        List.Extra.removeWhen (\details -> details.faction == Just faction)
                               )
                            |> Dict.Extra.groupBy (\{ slot } -> Types.slotToString slot)
                            |> Dict.map (\_ s -> List.length s)
                            |> Dict.toList
                            |> Monad.combineMap
                                (\( slot, count ) ->
                                    let
                                        check : Int -> Monad ()
                                        check expected =
                                            if count > expected then
                                                Monad.succeed ()
                                                    |> Monad.withWarning
                                                        ("Too many "
                                                            ++ slot
                                                            ++ " quest: expected up to "
                                                            ++ String.fromInt expected
                                                            ++ ", got "
                                                            ++ String.fromInt count
                                                        )

                                            else
                                                Monad.succeed ()
                                    in
                                    case slot of
                                        "Folk" ->
                                            check 5

                                        "Noble" ->
                                            check 4

                                        "Heroic" ->
                                            check 2

                                        "Epic" ->
                                            check 1

                                        _ ->
                                            Monad.error ("Unexpected quest slot: " ++ slot)
                                )
                in
                slotsChecks
                    |> Monad.map (\_ -> { zero | rewardPoints = List.sum rps })
            )


questCost : Quest -> Monad ( Int, Quest.Details )
questCost named =
    Utils.find "Quest" .name named Generated.Quest.all Types.questToString
        |> Monad.andThen
            (\quest ->
                quest.reward
                    |> Monad.succeed
                    |> Monad.withRewardInfo (Types.questToString named)
                    |> Monad.map (\rp -> ( rp, quest ))
            )
