module Elm.Declare.Extra exposing (Record)

import Elm
import Elm.Annotation
import Elm.Declare


type alias Record type_ =
    { annotation : Elm.Annotation.Annotation
    , declaration : Elm.Declaration
    , internal : Elm.Declare.Internal Elm.Annotation.Annotation
    , make : type_ -> Elm.Expression
    }
