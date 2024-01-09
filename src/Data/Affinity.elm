module Data.Affinity exposing (all, baseAffinities, fromModel)

import Data.Race as Race
import Generated.Types exposing (Affinity(..), Race(..))
import List.Extra
import Types exposing (CosmicPearlData)


fromModel :
    { a
        | races : List Race
        , mainRace : Maybe Race
        , cosmicPearl : CosmicPearlData
        , typePerks : List Race
    }
    -> List Affinity
fromModel { races, mainRace, cosmicPearl, typePerks } =
    let
        base : List Affinity
        base =
            case ( mainRace, races ) of
                ( Just race, _ ) ->
                    baseAffinities race

                ( Nothing, [ race ] ) ->
                    baseAffinities race

                _ ->
                    []

        fromTypePerk : List Affinity
        fromTypePerk =
            [ ( Nymph, Mind ), ( Empusa, Wind ), ( Doll, Mind ) ]
                |> List.filterMap
                    (\( type_, affinity ) ->
                        if List.member type_ typePerks then
                            Just affinity

                        else
                            Nothing
                    )

        afterChange : List Affinity
        afterChange =
            List.foldl
                (\( from, to ) acc -> to :: List.Extra.remove from acc)
                base
                cosmicPearl.change
    in
    (afterChange ++ cosmicPearl.add ++ fromTypePerk)
        |> (::) All
        |> List.Extra.unique


baseAffinities : Race -> List Affinity
baseAffinities race =
    Race.all [ race ]
        |> List.Extra.find (\{ name } -> name == race)
        |> Maybe.map .affinities
        |> Maybe.withDefault []


all : List Affinity
all =
    [ Beast, Blood, Body, Earth, Fire, Life, Metal, Mind, Nature, Necro, Soul, Water, Wind ]
