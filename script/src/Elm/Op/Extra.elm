module Elm.Op.Extra exposing (appendsStrings)

import Elm
import Elm.Op


appendsStrings : List Elm.Expression -> Elm.Expression
appendsStrings list =
    case list of
        [] ->
            Elm.string ""

        [ x ] ->
            x

        h :: t ->
            List.foldl (\e a -> Elm.Op.append a e) h t
