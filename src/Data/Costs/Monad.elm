module Data.Costs.Monad exposing (Info, Monad, Value(..), andThen, combine, combineMap, error, map, map2, map3, mapAndSum, succeed, withInfo, withPowerInfo, withRewardInfo, withWarning, withWarningMaybe)

import ResultME exposing (ResultME)
import Types exposing (IdKind)


type alias Info =
    { label : String
    , kind : IdKind
    , anchor : Maybe String
    , value : Value
    }


type Value
    = Power Int
    | RewardPoints Int
    | FreeBecause String


type alias Monad a =
    ResultME
        String
        { value : a
        , warnings : List String
        , infos : List Info
        }


succeed : a -> Monad a
succeed x =
    Ok
        { value = x
        , warnings = []
        , infos = []
        }


error : String -> Monad value
error msg =
    ResultME.error msg


map : (a -> b) -> Monad a -> Monad b
map f a =
    ResultME.map
        (\av ->
            { value = f av.value
            , infos = av.infos
            , warnings = av.warnings
            }
        )
        a


map2 : (a -> b -> c) -> Monad a -> Monad b -> Monad c
map2 f a b =
    ResultME.map2
        (\av bv ->
            { value = f av.value bv.value
            , infos = av.infos ++ bv.infos
            , warnings = av.warnings ++ bv.warnings
            }
        )
        a
        b


map3 : (a -> b -> c -> d) -> Monad a -> Monad b -> Monad c -> Monad d
map3 f a b c =
    ResultME.map3
        (\av bv cv ->
            { value = f av.value bv.value cv.value
            , infos = av.infos ++ bv.infos ++ cv.infos
            , warnings = av.warnings ++ bv.warnings ++ cv.warnings
            }
        )
        a
        b
        c


andThen : (a -> Monad b) -> Monad a -> Monad b
andThen f a =
    ResultME.andThen
        (\v ->
            f v.value
                |> ResultME.map
                    (\fv ->
                        { value = fv.value
                        , warnings = v.warnings ++ fv.warnings
                        , infos = v.infos ++ fv.infos
                        }
                    )
        )
        a


combine : List (Monad a) -> Monad (List a)
combine list =
    List.foldr
        (\e a -> map2 (::) e a)
        (succeed [])
        list


combineMap : (a -> Monad b) -> List a -> Monad (List b)
combineMap f list =
    list
        |> List.map f
        |> combine


mapAndSum : (item -> Monad Int) -> List item -> Monad Int
mapAndSum toValue list =
    combineMap toValue list
        |> map List.sum


withPowerInfo : IdKind -> String -> Monad Int -> Monad Int
withPowerInfo kind key r =
    Result.map
        (\v ->
            { v
                | infos =
                    { label = key
                    , kind = kind
                    , anchor = Just key
                    , value = Power v.value
                    }
                        :: v.infos
            }
        )
        r


withRewardInfo : IdKind -> String -> Monad Int -> Monad Int
withRewardInfo kind key r =
    Result.map
        (\v ->
            { v
                | infos =
                    { label = key
                    , kind = kind
                    , anchor = Just key
                    , value = RewardPoints v.value
                    }
                        :: v.infos
            }
        )
        r


withInfo : Info -> Monad a -> Monad a
withInfo info r =
    Result.map
        (\v ->
            { v
                | infos = info :: v.infos
            }
        )
        r


withWarning : String -> Monad a -> Monad a
withWarning warning r =
    Result.map
        (\v ->
            { v
                | warnings = warning :: v.warnings
            }
        )
        r


withWarningMaybe : Maybe String -> Monad a -> Monad a
withWarningMaybe maybeWarning r =
    case maybeWarning of
        Nothing ->
            r

        Just warning ->
            withWarning warning r
