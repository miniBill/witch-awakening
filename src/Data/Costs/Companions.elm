module Data.Costs.Companions exposing (value)

import Data.Companion as Companion
import Data.Costs.Monad as Monad exposing (Monad)
import Data.Costs.Utils as Utils exposing (Points)
import Dict exposing (Dict)
import Generated.Companion as Companion
import Generated.Types as Types exposing (Class, Companion, Faction(..), Race)
import List.Extra
import Types exposing (IdKind(..), Model)


value : Model key -> Monad Points
value model =
    model.companions
        |> Monad.combineMap getCompanion
        |> Monad.andThen (totalCompanionValue model)


totalCompanionValue : Model key -> List ( Maybe Faction, Companion.Details ) -> Monad Points
totalCompanionValue model companions =
    let
        treasure : Bool
        treasure =
            List.member FactionTheCollegeOfArcadia model.factionPerks

        filterMapAtMost : Int -> (a -> Maybe b) -> List a -> List b
        filterMapAtMost n f list =
            let
                go : Int -> List a -> List b -> List b
                go i queue acc =
                    if i <= 0 then
                        List.reverse acc

                    else
                        case queue of
                            [] ->
                                List.reverse acc

                            h :: t ->
                                case f h of
                                    Nothing ->
                                        go i t acc

                                    Just v ->
                                        go (i - 1) t (v :: acc)
            in
            go n list []

        mostExpensiveFirst : List ( Maybe Faction, Int, Companion.Details )
        mostExpensiveFirst =
            companions
                |> List.filterMap
                    (\( f, c ) ->
                        Maybe.map
                            (\cost -> ( f, cost, c ))
                            c.cost
                    )
                |> List.sortBy (\( _, cost, _ ) -> -cost)

        sameFaction : List ( Int, Companion.Details )
        sameFaction =
            mostExpensiveFirst
                |> filterMapAtMost
                    (if treasure then
                        4

                     else
                        2
                    )
                    (\( faction, cost, c ) ->
                        case faction of
                            Just f ->
                                if List.member f model.factions then
                                    Just ( cost, c )

                                else
                                    Nothing

                            Nothing ->
                                Nothing
                    )

        sameKind : List ( String, Int, Companion.Details )
        sameKind =
            mostExpensiveFirst
                |> filterMapAtMost
                    (if treasure then
                        4

                     else
                        2
                    )
                    (\( _, cost, companion ) ->
                        if sameRace companion model.races then
                            Just ( "Same race", cost, companion )

                        else if sameClass companion model.class then
                            Just ( "Same class", cost, companion )

                        else
                            Nothing
                    )

        withReason :
            String
            -> List ( Int, Companion.Details )
            -> List ( String, Int, Companion.Details )
        withReason label group =
            List.map
                (\( cost, details ) -> ( label, cost, details ))
                group

        tryPick :
            List (List ( String, Int, Companion.Details ))
            -> Dict String String
        tryPick lists =
            lists
                |> List.Extra.removeWhen List.isEmpty
                |> List.Extra.cartesianProduct
                |> List.map
                    (\picked ->
                        let
                            unique : List ( String, Int, Companion.Details )
                            unique =
                                picked
                                    |> List.Extra.uniqueBy (\( _, _, { name } ) -> name)
                        in
                        { value =
                            unique
                                |> List.map (\( _, cost, _ ) -> cost)
                                |> List.sum
                        , freebies =
                            unique
                                |> List.map
                                    (\( label, _, { name } ) ->
                                        ( Companion.toString name, label )
                                    )
                                |> Dict.fromList
                        }
                    )
                |> List.Extra.maximumBy .value
                |> Maybe.map .freebies
                |> Maybe.withDefault Dict.empty

        forFree : Dict String String
        forFree =
            if treasure then
                let
                    possiblyFriendly : List ( Int, Companion.Details )
                    possiblyFriendly =
                        List.filterMap
                            (\( f, cost, c ) ->
                                if
                                    (f /= Just FactionTheOutsiders)
                                        && (f /= Just FactionAlphazonIndustries)
                                then
                                    Just ( cost, c )

                                else
                                    Nothing
                            )
                            mostExpensiveFirst
                in
                tryPick
                    [ withReason "Same faction" sameFaction
                    , withReason "True Treasure [br] same faction" sameFaction
                    , sameKind
                    , withReason "True Treasure [br] friendly faction" possiblyFriendly
                    ]

            else
                tryPick
                    [ withReason "Same faction" sameFaction
                    , sameKind
                    ]
    in
    companions
        |> Monad.mapAndSum
            (\( _, { name, cost } ) ->
                let
                    nameString : String
                    nameString =
                        Companion.toString name
                in
                case Dict.get nameString forFree of
                    Just reason ->
                        Monad.succeed 0
                            |> Monad.withInfo
                                { label = nameString
                                , kind = IdKindCompanion
                                , anchor = Nothing
                                , value = Monad.FreeBecause reason
                                }

                    Nothing ->
                        case cost of
                            Just v ->
                                Monad.succeed -v
                                    |> Monad.withPowerInfo IdKindCompanion nameString

                            Nothing ->
                                Monad.error <| "Companion " ++ nameString ++ " does not have a fixed cost"
            )
        |> Monad.map Utils.rewardPointsToPoints


sameClass : Companion.Details -> Maybe Class -> Bool
sameClass companion maybeClass =
    case companion.class of
        Companion.ClassOne class_ ->
            Just class_ == maybeClass

        Companion.ClassAny ->
            True

        Companion.ClassNone ->
            False

        Companion.ClassSpecial ->
            False


sameRace : Companion.Details -> List Race -> Bool
sameRace companion races =
    List.isEmpty companion.races
        || List.any (\companionRace -> List.any (Types.isSameRace companionRace) races) companion.races


getCompanion : Companion -> Monad ( Maybe Faction, Companion.Details )
getCompanion companion =
    case
        List.Extra.findMap
            (\( faction, group ) ->
                List.Extra.findMap
                    (\({ name } as c) ->
                        if name == companion then
                            Just ( faction, c )

                        else
                            Nothing
                    )
                    group
            )
            Companion.all
    of
        Just p ->
            Monad.succeed p

        Nothing ->
            Monad.error <| "Companion " ++ Companion.toString companion ++ " not found"
