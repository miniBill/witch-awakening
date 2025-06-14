module Elm.Declare.Extra exposing (buildCustomRecord, customRecord, exposeConstructor, withDeclarations, withField, withHelper)

import Elm
import Elm.Annotation
import Elm.Declare


withDeclarations : List Elm.Declaration -> Elm.Declare.Module val -> Elm.Declare.Module val
withDeclarations declarations module_ =
    { module_ | declarations = List.reverse declarations ++ module_.declarations }


withHelper : a -> Elm.Declare.Module (a -> b) -> Elm.Declare.Module b
withHelper helper module_ =
    { name = module_.name
    , declarations = module_.declarations
    , call = module_.call helper
    }


type alias CustomRecord type_ =
    { name : String
    , fields : List ( String, Elm.Annotation.Annotation )
    , make : type_ -> List ( String, Elm.Expression )
    }


customRecord : String -> CustomRecord type_
customRecord name =
    { name = name
    , fields = []
    , make = \_ -> []
    }


withField :
    String
    -> (type_ -> Elm.Expression)
    -> Elm.Annotation.Annotation
    -> CustomRecord type_
    -> CustomRecord type_
withField fieldName getter annotation record =
    { name = record.name
    , fields = ( fieldName, annotation ) :: record.fields
    , make = \v -> ( fieldName, getter v ) :: record.make v
    }


buildCustomRecord :
    CustomRecord type_
    ->
        { annotation : Elm.Annotation.Annotation
        , declaration : Elm.Declaration
        , internal : Elm.Declare.Internal Elm.Annotation.Annotation
        , make : type_ -> Elm.Expression
        }
buildCustomRecord record =
    let
        annotation : Elm.Annotation.Annotation
        annotation =
            Elm.Annotation.record record.fields

        inner : Elm.Declare.Annotation
        inner =
            Elm.Declare.alias record.name annotation
    in
    { annotation = inner.annotation
    , declaration = inner.declaration
    , internal = inner.internal
    , make = \v -> Elm.record (List.reverse (record.make v)) |> Elm.withType annotation
    }


exposeConstructor : Elm.Declare.Annotation -> Elm.Declare.Annotation
exposeConstructor unexposed =
    { unexposed | declaration = Elm.exposeConstructor unexposed.declaration }
