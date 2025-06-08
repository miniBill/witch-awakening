module Generate.Utils exposing (annotationFromTypes, valueFromTypes, yassify)

import Elm
import Elm.Annotation
import String.Extra


yassify : String -> String
yassify str =
    str
        |> String.replace "Æ" "Ae"
        |> String.replace "æ" "ae"
        |> String.replace "\"" ""
        |> String.Extra.classify


valueFromTypes : String -> Elm.Expression
valueFromTypes name =
    Elm.value
        { importFrom = [ "Generated", "Types" ]
        , name = yassify name
        , annotation = Nothing
        }


annotationFromTypes : String -> Elm.Annotation.Annotation
annotationFromTypes =
    Elm.Annotation.named [ "Generated", "Types" ]
