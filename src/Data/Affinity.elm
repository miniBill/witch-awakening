module Data.Affinity exposing (Details, baseAffinities, fromModel)

import Generated.Race
import Generated.Types exposing (Affinity(..), Race(..))
import List.Extra
import Types exposing (CosmicPearlData)


type alias Details =
    { name : Affinity
    , dlc : Maybe String
    }


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
            [ ( Nymph, Mind )
            , ( Empusa, Wind )
            , ( Doll, Mind )
            , ( Firebird, Life )
            ]
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
    Generated.Race.all [ race ]
        |> List.Extra.find (\{ name } -> name == race)
        |> Maybe.map .affinities
        |> Maybe.withDefault []
