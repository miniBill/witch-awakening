module Data.Affinity exposing (AffinityList, InAffinity(..), affinitiesForRace, defaultList, fromModel, isInAffinity, selectable, toList)

import Data.Magic as Magic
import Generated.Affinity as Affinity
import Generated.Race
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
    -> AffinityList
fromModel { races, mainRace, cosmicPearl, typePerks } =
    let
        (AffinityList base) =
            case ( mainRace, races ) of
                ( Just race, _ ) ->
                    affinitiesForRace race

                ( Nothing, [ race ] ) ->
                    affinitiesForRace race

                _ ->
                    defaultList

        fromTypePerk : List Affinity
        fromTypePerk =
            [ ( RaceNymph, AffinityMind )
            , ( RaceEmpusa, AffinityWind )
            , ( RaceDoll, AffinityMind )
            , ( RaceFirebird, AffinityLife )
            , ( RaceMummy, AffinityWater )
            , ( RaceNyctimene, AffinityLife )
            , ( RaceShadeglass, AffinityFire )
            , ( RaceMarid, AffinityAll )
            , ( RaceCantor, AffinitySoul )
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
        |> fromList


fromList : List Affinity -> AffinityList
fromList list =
    list
        |> List.Extra.unique
        |> AffinityList


affinitiesForRace : Race -> AffinityList
affinitiesForRace race =
    Generated.Race.all [ race ]
        |> List.Extra.find (\{ name } -> name == race)
        |> Maybe.map (\{ affinities } -> fromList affinities)
        |> Maybe.withDefault defaultList


type AffinityList
    = AffinityList (List Affinity)


type InAffinity
    = OffAffinity
    | InAffinity
    | DoubleAffinity


isInAffinity : Magic.Affinities -> AffinityList -> InAffinity
isInAffinity magicAffinities affinities =
    case magicAffinities of
        Magic.Regular regular ->
            isInAffinityRegular regular affinities

        Magic.Alternative alternatives ->
            List.foldl
                (\alternative acc ->
                    case ( acc, isInAffinityElementalism alternative affinities ) of
                        ( DoubleAffinity, _ ) ->
                            DoubleAffinity

                        ( _, DoubleAffinity ) ->
                            DoubleAffinity

                        ( InAffinity, _ ) ->
                            InAffinity

                        ( _, InAffinity ) ->
                            InAffinity

                        _ ->
                            OffAffinity
                )
                OffAffinity
                alternatives


isInAffinityRegular : List Affinity -> AffinityList -> InAffinity
isInAffinityRegular regular (AffinityList affinities) =
    let
        inList : Bool
        inList =
            List.any
                (\affinity -> List.member affinity affinities)
                regular
    in
    case ( List.member AffinityAll regular, inList ) of
        ( True, True ) ->
            DoubleAffinity

        ( False, False ) ->
            OffAffinity

        _ ->
            InAffinity


isInAffinityElementalism : List Affinity -> AffinityList -> InAffinity
isInAffinityElementalism elementalism (AffinityList affinities) =
    let
        inList : Bool
        inList =
            List.all
                (\affinity -> List.member affinity affinities)
                elementalism
    in
    case ( List.member AffinityAll elementalism, inList ) of
        ( True, True ) ->
            DoubleAffinity

        ( False, False ) ->
            OffAffinity

        _ ->
            InAffinity


toList : AffinityList -> List Affinity
toList (AffinityList list) =
    list


defaultList : AffinityList
defaultList =
    AffinityList []


selectable : List Affinity.Details
selectable =
    List.filter
        (\{ name } ->
            Affinity.isSelectable name
        )
        Affinity.all
