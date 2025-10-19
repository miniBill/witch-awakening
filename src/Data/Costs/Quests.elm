module Data.Costs.Quests exposing (value)

import Data.Costs.Monad as Monad exposing (Monad)
import Data.Costs.Utils as Utils exposing (Points, zero)
import Dict
import Dict.Extra
import Generated.Quest as Quest
import Generated.Slot as Slot
import Generated.Types exposing (Faction(..), Quest(..))
import List.Extra
import Types exposing (IdKind(..), Model)


value : Model key -> Monad Points
value model =
    model.quests
        |> Monad.combineMap (questCost model)
        |> Monad.andThen
            (\rpsAndSlots ->
                let
                    ( rps, questsDetails ) =
                        List.unzip rpsAndSlots

                    slotsChecks : Monad (List ())
                    slotsChecks =
                        questsDetails
                            |> (if List.isEmpty model.factions then
                                    List.Extra.removeWhen (\details -> details.faction == Just FactionIndependents)

                                else
                                    List.Extra.removeWhen
                                        (\details ->
                                            case details.faction of
                                                Nothing ->
                                                    False

                                                Just faction ->
                                                    List.member faction model.factions
                                        )
                               )
                            |> Dict.Extra.groupBy (\{ slot } -> Slot.toString slot)
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


questCost : Model key -> Quest -> Monad ( Int, Quest.Details )
questCost model named =
    Utils.find "Quest" .name named Quest.all Quest.toString
        |> Monad.andThen
            (\quest ->
                let
                    base : Monad Int
                    base =
                        quest.reward
                            |> Utils.checkRequirements quest (Quest.toString named) model
                            |> Monad.withRewardInfo IdKindQuest (Quest.toString named)
                in
                if named == QuestDomesticated && List.member QuestHouseFire model.quests then
                    [ base
                    , 2
                        |> Monad.succeed
                        |> Monad.withInfo
                            { kind = IdKindQuest
                            , label = "Domesticated + House Fire"
                            , value = Monad.RewardPoints 2
                            , anchor = Just (Quest.toString named)
                            }
                    ]
                        |> Monad.mapAndSum identity
                        |> Monad.map (\rp -> ( rp, quest ))

                else
                    base |> Monad.map (\rp -> ( rp, quest ))
            )
