module Data.Costs.Monad exposing (Monad, andThen, combine, error, map, map2, map3, mapAndSum, succeed, withInfo, withWarning)

import ResultME exposing (ResultME)


type alias Monad a =
    ResultME
        String
        { value : a
        , warnings : List String
        , infos : List String
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


mapAndSum : (item -> Monad Int) -> List item -> Monad Int
mapAndSum toValue list =
    list
        |> List.map toValue
        |> combine
        |> map List.sum


withInfo : String -> Monad a -> Monad a
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
