module Generate.Utils exposing (yassify)

import String.Extra


{-| Call `String.Extra.classify`, after replacing æ => ae, Æ => AE and stripping `"`.
-}
yassify : String -> String
yassify str =
    str
        |> String.replace "Æ" "Ae"
        |> String.replace "æ" "ae"
        |> String.replace "\"" ""
        |> String.Extra.classify
