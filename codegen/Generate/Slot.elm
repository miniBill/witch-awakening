module Generate.Slot exposing (SlotModule, file)

import Elm
import Elm.Declare
import Generate.Enum as Enum exposing (Enum)


type alias SlotModule =
    { toString : Elm.Expression -> Elm.Expression
    }


file : Enum -> Elm.Declare.Module SlotModule
file enum =
    Elm.Declare.module_ [ "Generated", "Slot" ] SlotModule
        |> Elm.Declare.with (Enum.toString enum)
