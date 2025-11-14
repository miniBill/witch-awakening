module Data.Affinity exposing (AffinityList, InAffinity(..), affinitiesForRace, affinitiesForTypePerks, defaultList, fromModel, isInAffinity, selectable, toList)

import Data.Magic as Magic
import Generated.Affinity as Affinity
import Generated.Race
import Generated.Types exposing (Affinity(..), Race(..), Relic(..))
import List.Extra
import Types exposing (RankedRelic)


fromModel :
    { a
        | races : List Race
        , mainRace : Maybe Race
        , relics : List RankedRelic
        , typePerks : List Race
    }
    -> AffinityList
fromModel model =
    let
        ( pearlChange, pearlAdd ) =
            model.relics
                |> List.Extra.findMap
                    (\perk ->
                        case perk.name of
                            RelicCosmicPearl data ->
                                Just ( data.change, data.add )

                            _ ->
                                Nothing
                    )
                |> Maybe.withDefault ( [], [] )

        base : List Affinity
        base =
            case ( model.mainRace, model.races ) of
                ( Just race, _ ) ->
                    toList (affinitiesForRace race)

                ( Nothing, [ race ] ) ->
                    toList (affinitiesForRace race)

                _ ->
                    []

        afterChange : List Affinity
        afterChange =
            List.foldl
                (\( from, to ) acc -> to :: List.Extra.remove from acc)
                (base ++ toList (affinitiesForTypePerks model.typePerks))
                pearlChange
    in
    (afterChange ++ pearlAdd)
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


affinitiesForTypePerks : List Race -> AffinityList
affinitiesForTypePerks typePerks =
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
        |> fromList


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
