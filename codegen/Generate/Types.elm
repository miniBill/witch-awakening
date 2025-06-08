module Generate.Types exposing (file)

import Data
import Elm.Declare
import Elm.Declare.Extra
import Generate.Enums
import Parsers


file : List Parsers.DLC -> Elm.Declare.Module TypesModule
file dlcList =
    Elm.Declare.module_ [ "Generated", "Types" ] TypesModule
        |> Elm.Declare.Extra.withDeclarations (List.map Generate.Enums.enumToDeclarations (Data.enums dlcList))


type alias TypesModule =
    {}
