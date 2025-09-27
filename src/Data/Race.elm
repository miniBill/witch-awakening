module Data.Race exposing (Details, isGenie, withVariantAffinity1, withVariantAffinity2)

import Generated.Types exposing (Affinity(..), Race(..), Size)
import List.Extra


type alias Details =
    { name : Race
    , affinities : List Affinity
    , tank : Size
    , charge : Size
    , content : String
    , dlc : Maybe String
    }


withVariantAffinity1 :
    (Race -> Maybe Affinity)
    ->
        { name : Affinity -> Race
        , tank : Size
        , affinities : List Affinity
        , charge : Size
        , dlc : Maybe String
        , content : String
        }
    -> List Race
    -> Details
withVariantAffinity1 match details races =
    let
        affinity : Affinity
        affinity =
            List.Extra.findMap match races
                |> Maybe.withDefault AffinityAll
    in
    { name = details.name affinity
    , tank = details.tank
    , affinities = details.affinities ++ [ affinity ]
    , charge = details.charge
    , dlc = details.dlc
    , content = details.content
    }


withVariantAffinity2 :
    (Race -> Maybe ( Affinity, Affinity ))
    ->
        { name : Affinity -> Affinity -> Race
        , tank : Size
        , affinities : List Affinity
        , charge : Size
        , dlc : Maybe String
        , content : String
        }
    -> List Race
    -> Details
withVariantAffinity2 match details races =
    let
        ( aff1, aff2 ) =
            List.Extra.findMap match races
                |> Maybe.withDefault ( AffinityAll, AffinityAll )
    in
    { name = details.name aff1 aff2
    , tank = details.tank
    , affinities = [ aff1, aff2 ]
    , charge = details.charge
    , dlc = details.dlc
    , content = details.content
    }


isGenie : Race -> Bool
isGenie race =
    case race of
        RaceGenie _ ->
            True

        _ ->
            False
