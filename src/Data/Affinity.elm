module Data.Affinity exposing (all, baseAffinities, fromModel)

import Data.Race as Race
import Generated.Types exposing (Affinity(..), Race(..))
import List.Extra
import Types exposing (Model)


fromModel : Model key -> List Affinity
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

        nymphPerk : List Affinity
        nymphPerk =
            if List.member Nymph typePerks then
                [ Mind ]

            else
                []

        afterChange : List Affinity
        afterChange =
            List.foldl
                (\( from, to ) acc -> to :: List.Extra.remove from acc)
                base
                cosmicPearl.change
    in
    (afterChange ++ cosmicPearl.add ++ nymphPerk)
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
