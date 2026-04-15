module Generate.ComplicationCategory exposing (ComplicationCategoryModule, file)

import Elm
import Elm.Declare
import Generate.Enum as Enum exposing (Enum)


type alias ComplicationCategoryModule =
    { toString : Elm.Expression -> Elm.Expression
    }


file : Enum -> Elm.Declare.Module ComplicationCategoryModule
file enum =
    Elm.Declare.module_ [ "Generated", "ComplicationCategory" ] ComplicationCategoryModule
        |> Elm.Declare.with (Enum.toString enum)
