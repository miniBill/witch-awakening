module Generate.Types exposing (TypesModule, annotation, file, value)

import Data
import Elm
import Elm.Annotation
import Elm.Declare
import Elm.Declare.Extra
import Generate.Enums
import Generate.Utils exposing (yassify)
import Parsers


type alias TypesModule =
    {}


moduleName : List String
moduleName =
    [ "Generated", "Types" ]


file : List Parsers.DLC -> Elm.Declare.Module TypesModule
file dlcList =
    Elm.Declare.module_ moduleName TypesModule
        |> Elm.Declare.Extra.withDeclarations (List.map Generate.Enums.enumToDeclarations (Data.enums dlcList))


value : String -> Elm.Expression
value name =
    Elm.value
        { importFrom = moduleName
        , name = yassify name
        , annotation = Nothing
        }


annotation : String -> Elm.Annotation.Annotation
annotation =
    Elm.Annotation.named moduleName
