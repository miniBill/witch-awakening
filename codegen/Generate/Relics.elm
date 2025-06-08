module Generate.Relics exposing (relicsFile)

import Elm
import Elm.Annotation
import Elm.Declare
import Elm.Declare.Extra
import Gen.Data.Relic
import Generate.Utils exposing (annotationFromTypes, valueFromTypes, yassify)
import Parsers exposing (Content(..))
import String.Extra


type alias RelicsModule =
    { all : Elm.Expression
    , declaration : Elm.Annotation.Annotation
    }


relicsFile : List ( Maybe String, Parsers.Relic ) -> Elm.Declare.Module RelicsModule
relicsFile dlcRelics =
    Elm.Declare.module_ [ "Generated", "Relic" ] RelicsModule
        |> Elm.Declare.with (allRelics dlcRelics)
        |> Elm.Declare.with relicDetails.declaration
        |> Elm.Declare.Extra.withDeclarations (dlcToRelics dlcRelics)


allRelics : List ( Maybe String, Parsers.Relic ) -> Elm.Declare.Value
allRelics dlcRelics =
    dlcRelics
        |> List.map (\( _, relic ) -> Elm.val (String.Extra.decapitalize (yassify relic.name)))
        |> Elm.list
        |> Elm.withType (Elm.Annotation.list Gen.Data.Relic.annotation_.details)
        |> Elm.Declare.value "all"


relicDetails :
    { declaration : Elm.Declare.Annotation
    , make :
        { name : Elm.Expression
        , classes : Elm.Expression
        , dlc : Elm.Expression
        , content : Elm.Expression
        }
        -> Elm.Expression
    }
relicDetails =
    Elm.Declare.Extra.customRecord "Details"
        |> Elm.Declare.Extra.withField "name" .name (annotationFromTypes "Relic")
        |> Elm.Declare.Extra.withField "classes" .classes (Elm.Annotation.list (annotationFromTypes "Class"))
        |> Elm.Declare.Extra.withField "dlc" .dlc (Elm.Annotation.maybe Elm.Annotation.string)
        |> Elm.Declare.Extra.withField "content" .content Gen.Data.Relic.annotation_.content
        |> Elm.Declare.Extra.buildCustomRecord


dlcToRelics : List ( Maybe String, Parsers.Relic ) -> List Elm.Declaration
dlcToRelics relics =
    List.map
        (\( dlcName, relic ) ->
            relicDetails.make
                { name = valueFromTypes relic.name
                , classes = Elm.list (List.map valueFromTypes relic.classes)
                , dlc = Elm.maybe (Maybe.map Elm.string dlcName)
                , content =
                    case relic.content of
                        Parsers.Single cost description ->
                            if relic.name == "Cosmic Pearl" then
                                Gen.Data.Relic.make_.cosmicPearlContent (Elm.int cost) (Elm.string description)

                            else
                                Gen.Data.Relic.make_.single (Elm.int cost) (Elm.string description)

                        Parsers.WithCosts costs description ->
                            Gen.Data.Relic.make_.withChoices (Elm.list (List.map Elm.int costs)) (Elm.string description)

                        Parsers.WithChoices ever _ _ _ ->
                            never ever
                }
                |> Elm.declaration (yassify relic.name)
                |> Elm.expose
        )
        relics
