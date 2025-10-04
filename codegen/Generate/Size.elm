module Generate.Size exposing (SizeModule, file)

import Elm
import Elm.Declare
import Generate.Enum as Enum exposing (Enum)


type alias SizeModule =
    { toString : Elm.Expression -> Elm.Expression
    }


file : Enum -> Elm.Declare.Module SizeModule
file enum =
    Elm.Declare.module_ [ "Generated", "Size" ] SizeModule
        |> Elm.Declare.with (Enum.toString enum)
