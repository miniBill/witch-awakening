module Elm.Declare.Extra exposing (buildCustomRecord, customRecord, withDeclarations, withField, withHelper)

import Elm
import Elm.Annotation
import Elm.Declare


withDeclarations : List Elm.Declaration -> Elm.Declare.Module val -> Elm.Declare.Module val
withDeclarations declarations module_ =
    { module_ | declarations = module_.declarations ++ declarations }


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
        { declaration : Elm.Declare.Annotation
        , make : type_ -> Elm.Expression
        }
buildCustomRecord record =
    let
        annotation : Elm.Annotation.Annotation
        annotation =
            Elm.Annotation.record record.fields
    in
    { declaration = Elm.Declare.alias record.name annotation
    , make = \v -> Elm.record (List.reverse (record.make v)) |> Elm.withType annotation
    }
