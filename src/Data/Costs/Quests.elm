module Data.Costs.Quests exposing (value)

import Data.Costs.Monad as Monad exposing (Monad)
import Data.Costs.Points as Points exposing (Points)
import Data.Costs.Utils as Utils
import Data.Costs.Value as Value
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
        |> Monad.combineMap (questValue model)
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
                                                let
                                                    msg : String
                                                    msg =
                                                        "Too many "
                                                            ++ slot
                                                            ++ " quest: expected up to "
                                                            ++ String.fromInt expected
                                                            ++ ", got "
                                                            ++ String.fromInt count
                                                in
                                                Monad.succeed ()
                                                    |> Monad.withWarning msg

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
                    |> Monad.map (\_ -> Points.sum rps)
            )


questValue : Model key -> Quest -> Monad ( Points, Quest.Details )
questValue model named =
    Utils.find "Quest" .name named Quest.all Quest.toString
        |> Monad.andThen
            (\quest ->
                let
                    base : Monad Points
                    base =
                        Points.fromRewardPoints quest.reward
                            |> Utils.checkRequirements quest (Quest.toString named) model
                            |> Monad.withPointsInfo IdKindQuest (Quest.toString named)
                in
                if named == QuestDomesticated && List.member QuestHouseFire model.quests then
                    [ base
                    , Points.fromRewardPoints 2
                        |> Monad.succeed
                        |> Monad.withInfo
                            { kind = IdKindQuest
                            , label = "Domesticated + House Fire"
                            , value = Value.fromRewardPoints 2
                            , anchor = Just (Quest.toString named)
                            }
                    ]
                        |> Monad.combineAndSum
                        |> Monad.map (\rp -> ( rp, quest ))

                else
                    base |> Monad.map (\rp -> ( rp, quest ))
            )
