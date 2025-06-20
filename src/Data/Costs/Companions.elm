module Data.Costs.Companions exposing (value)

import Data.Companion as Companion
import Data.Costs.Monad as Monad exposing (Monad)
import Data.Costs.Utils as Utils exposing (Points, zero)
import Generated.Companion
import Generated.Types as Types exposing (Class, Companion, Faction(..), Race, companionToString)
import List.Extra
import Types exposing (Model)


value : Model key -> Monad Points
value model =
    let
        totalCompanionCost : List ( Maybe Faction, Companion.Details ) -> Monad Points
        totalCompanionCost companions =
            companions
                |> Monad.mapAndSum
                    (\( _, { name, cost } ) ->
                        case cost of
                            Just v ->
                                Monad.succeed v

                            Nothing ->
                                Monad.error <| "Companion " ++ Types.companionToString name ++ " does not have a fixed cost"
                    )
                |> Monad.map Utils.powerToPoints

        forFree : List ( Maybe Faction, Companion.Details ) -> Monad Points
        forFree companions =
            let
                treasure : Bool
                treasure =
                    model.faction == Just ( TheCollegeOfArcadia, True )

                byCost : List ( Maybe Faction, Int, Companion.Details )
                byCost =
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
                            byCost
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
                    byCost
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

                tryPick : List (List ( String, Int, Companion.Details )) -> Monad Points
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
                                    { zero
                                        | power =
                                            unique
                                                |> List.map (\( _, cost, _ ) -> cost)
                                                |> List.sum
                                    }
                                , warnings = []
                                , infos =
                                    unique
                                        |> List.map
                                            (\( label, _, { name } ) ->
                                                { label = companionToString name
                                                , anchor = Just (companionToString name)
                                                , value = Monad.FreeBecause label
                                                }
                                            )
                                }
                            )
                        |> List.Extra.maximumBy (\details -> details.value.power)
                        |> Maybe.withDefault
                            { value = zero
                            , warnings = []
                            , infos = []
                            }
                        |> Ok
            in
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
                            byCost
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
    model.companions
        |> Monad.combineMap getCompanion
        |> Monad.andThen
            (\companions ->
                Monad.map2
                    Utils.sum
                    (Monad.map Utils.negate (totalCompanionCost companions))
                    (forFree companions)
            )


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
