module Data.Costs.Complications exposing (complicationsRawValue, powerCap, value)

import Data.Complication as Complication
import Data.Costs.Monad as Monad exposing (Monad)
import Data.Costs.Utils as Utils exposing (Points, zero)
import Generated.Complication
import Generated.Types as Types exposing (GameMode(..))
import List.Extra
import Types exposing (ComplicationKind(..), Model)


{-| "Complications can only increase the starting power by up to 30 points"
-}
normalInitialWarning : String
normalInitialWarning =
    "Complications can only increase the starting power by up to 30 points"


{-| "In Early Bird mode, complications can only increase the starting power/power cap by up to 30 points"
-}
earlyBirdWarning : String
earlyBirdWarning =
    "In Early Bird mode, complications can only increase the starting power/power cap by up to 30 points"


{-| "In Story Arc mode, complications can only increase the power cap by up to 60 points"
-}
storyArcWarning : String
storyArcWarning =
    "In Story Arc mode, complications can only increase the power cap by up to 60 points"


{-| "Complications can only increase the power cap by up to 30 points"
-}
normalCapWarning : String
normalCapWarning =
    "Complications can only increase the power cap by up to 30 points"


powerCap : Model key -> Monad Points
powerCap model =
    case model.gameMode of
        Nothing ->
            model.towardsCap
                |> Utils.capWithWarning 30 normalCapWarning
                |> Monad.map (Utils.sum { zero | power = 100 })

        Just StoryArc ->
            complicationsRawValue model
                |> Monad.andThen (Utils.capWithWarning 60 storyArcWarning)
                |> Monad.map (Utils.sum { zero | power = 150 })

        Just EarlyBird ->
            complicationsRawValue model
                |> Monad.andThen (Utils.capWithWarning 30 earlyBirdWarning)
                |> Monad.map (Utils.sum { zero | power = 75 })

        Just SkillTree ->
            Utils.slotUnsupported

        Just Constellation ->
            Utils.slotUnsupported


value : Model key -> Monad Points
value model =
    complicationsRawValue model
        |> Monad.andThen
            (\raw ->
                case model.gameMode of
                    Just StoryArc ->
                        if model.capBuild then
                            raw
                                |> Utils.capWithWarning 60 storyArcWarning

                        else
                            Monad.succeed zero

                    Just EarlyBird ->
                        raw
                            |> Utils.capWithWarning 30 earlyBirdWarning

                    Just SkillTree ->
                        Utils.slotUnsupported

                    Just Constellation ->
                        Utils.slotUnsupported

                    Nothing ->
                        if model.capBuild then
                            model.towardsCap
                                |> Utils.capWithWarning 30 normalInitialWarning

                        else
                            (raw - model.towardsCap)
                                |> Utils.capWithWarning 30 normalInitialWarning
            )


complicationsRawValue : Model key -> Monad Int
complicationsRawValue model =
    Monad.mapAndSum (complicationValue model) model.complications


complicationValue : Model key -> Types.RankedComplication -> Monad Int
complicationValue model complication =
    let
        get : Int -> List a -> Monad a
        get tier list =
            case List.Extra.getAt (tier - 1) list of
                Just v ->
                    Monad.succeed v

                Nothing ->
                    Monad.error <| "Could not get tier " ++ String.fromInt tier ++ " for complication " ++ Types.complicationToString complication.name
    in
    case List.Extra.find (\{ name } -> name == complication.name) Generated.Complication.all of
        Nothing ->
            Monad.error <| "Could not find complication " ++ Types.complicationToString complication.name

        Just details ->
            let
                raw : Monad Int
                raw =
                    case ( details.content, complication.kind ) of
                        ( Complication.Single s _, _ ) ->
                            Monad.succeed s

                        ( Complication.WithTiers _ tiers _, Tiered tier ) ->
                            Monad.map Tuple.second <| get tier tiers

                        ( Complication.WithChoices _ choices _, Tiered tier ) ->
                            Monad.map Tuple.second <| get tier choices

                        ( Complication.WithGains costs _, Tiered tier ) ->
                            get tier costs

                        ( _, Nontiered ) ->
                            Monad.error <| "Need a tier for complication " ++ Types.complicationToString complication.name
            in
            raw
                |> Monad.map
                    (\r ->
                        let
                            bonus : Int
                            bonus =
                                if details.class /= Nothing && details.class == model.class then
                                    2

                                else
                                    0
                        in
                        r + bonus
                    )
                |> Monad.withPowerInfo
                    (Types.complicationToString details.name)
