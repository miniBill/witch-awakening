module Generate.Utils exposing (yassify)

import String.Extra


yassify : String -> String
yassify str =
    str
        |> String.replace "Æ" "Ae"
        |> String.replace "æ" "ae"
        |> String.replace "\"" ""
        |> String.Extra.classify
