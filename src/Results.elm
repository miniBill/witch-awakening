module Results exposing (Results(..), andThen, combine, combineMap, map, map2, map3, mapErrors, withDefault)

{-| Like result, but accumulates errors.
-}


type Results a
    = Oks a
    | Errs (List String)


map : (a -> b) -> Results a -> Results b
map f cost =
    case cost of
        Oks x ->
            Oks (f x)

        Errs e ->
            Errs e


map2 : (a -> b -> c) -> Results a -> Results b -> Results c
map2 f l r =
    case ( l, r ) of
        ( Errs le, Errs re ) ->
            Errs (le ++ re)

        ( Oks _, Errs re ) ->
            Errs re

        ( Errs le, _ ) ->
            Errs le

        ( Oks lo, Oks ro ) ->
            Oks (f lo ro)


map3 : (a -> b -> c -> d) -> Results a -> Results b -> Results c -> Results d
map3 f l m r =
    map2 identity (map2 f l m) r


andThen : (a -> Results b) -> Results a -> Results b
andThen f cost =
    case cost of
        Oks x ->
            f x

        Errs e ->
            Errs e


combine : List (Results a) -> Results (List a)
combine list =
    combineMap identity list


combineMap : (a -> Results b) -> List a -> Results (List b)
combineMap f list =
    List.foldl
        (\value acc ->
            map2 (::) (f value) acc
        )
        (Oks [])
        list
        |> map List.reverse


withDefault : a -> Results a -> a
withDefault default cost =
    case cost of
        Oks v ->
            v

        Errs _ ->
            default


mapErrors : (List String -> List String) -> Results a -> Results a
mapErrors f cost =
    case cost of
        Oks _ ->
            cost

        Errs e ->
            Errs (f e)
