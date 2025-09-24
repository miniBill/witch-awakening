module Generate.Utils exposing (color, yassify)

import Bitwise
import Elm exposing (Expression)
import Gen.Color
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


color : Int -> Expression
color c =
    Gen.Color.rgb255
        (c |> Bitwise.shiftRightBy 16)
        (c |> Bitwise.shiftRightBy 8 |> modBy 256)
        (c |> modBy 256)
