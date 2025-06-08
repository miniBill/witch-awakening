module Generate.Types exposing (module_)

import Data
import Elm.Declare
import Elm.Declare.Extra
import Generate.Enums
import Parsers


module_ : List Parsers.DLC -> Elm.Declare.Module TypesModule
module_ dlcList =
    Elm.Declare.module_ [ "Generated", "Types" ] TypesModule
        |> Elm.Declare.Extra.withDeclarations (List.map Generate.Enums.enumToDeclarations (Data.enums dlcList))


type alias TypesModule =
    {}
