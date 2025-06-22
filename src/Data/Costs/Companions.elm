module Data.Costs.Companions exposing (value)

import Data.Companion as Companion
import Data.Costs.Monad as Monad exposing (Monad)
import Data.Costs.Utils as Utils exposing (Points)
import Dict exposing (Dict)
import Generated.Companion
import Generated.Types as Types exposing (Class, Companion, Faction(..), Race)
import List.Extra
import Types exposing (Model)


value : Model key -> Monad Points
value model =
    let
        totalCompanionValue : List ( Maybe Faction, Companion.Details ) -> Monad Points
        totalCompanionValue companions =
            let
                treasure : Bool
                treasure =
                    model.faction == Just ( TheCollegeOfArcadia, True )

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
                    case model.faction of
                        Nothing ->
                            []

                        Just ( f, _ ) ->
                            mostExpensiveFirst
                                |> List.filterMap
                                    (\( faction, cost, c ) ->
                                        if faction == Just f then
                                            Just ( cost, c )

                                        else
                                            Nothing
                                    )
                                |> List.take
                                    (if treasure then
                                        4

                                     else
                                        2
                                    )

                sameKind : List ( String, Int, Companion.Details )
                sameKind =
                    mostExpensiveFirst
                        |> List.filterMap
                            (\( _, cost, companion ) ->
                                if sameRace companion model.races then
                                    Just ( "Same race", cost, companion )

                                else if sameClass companion model.class then
                                    Just ( "Same class", cost, companion )

                                else
                                    Nothing
                            )
                        |> List.take
                            (if treasure then
                                4

                             else
                                2
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
                                                ( Types.companionToString name, label )
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
                                        if f == Just TheOutsiders || f == Just AlphazonIndustries || f == Just TheCollegeOfArcadia then
                                            Nothing

                                        else
                                            Just ( cost, c )
                                    )
                                    mostExpensiveFirst
                        in
                        tryPick
                            [ withReason "Same faction" sameFaction
                            , withReason "Same faction - True Treasure" sameFaction
                            , sameKind
                            , withReason "Possibly friendly faction - True Treasure" possiblyFriendly
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
                                Types.companionToString name
                        in
                        case Dict.get nameString forFree of
                            Just reason ->
                                Monad.succeed 0
                                    |> Monad.withInfo
                                        { label = nameString
                                        , anchor = Nothing
                                        , value = Monad.FreeBecause reason
                                        }

                            Nothing ->
                                case cost of
                                    Just v ->
                                        Monad.succeed -v
                                            |> Monad.withPowerInfo nameString

                                    Nothing ->
                                        Monad.error <| "Companion " ++ nameString ++ " does not have a fixed cost"
                    )
                |> Monad.map Utils.powerToPoints
    in
    model.companions
        |> Monad.combineMap getCompanion
        |> Monad.andThen totalCompanionValue


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
        || List.any (\companionRace -> List.member companionRace races) companion.races


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
            Generated.Companion.all
    of
        Just p ->
            Monad.succeed p

        Nothing ->
            Monad.error <| "Companion " ++ Types.companionToString companion ++ " not found"
