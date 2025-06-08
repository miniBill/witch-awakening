module Generate.FromDLC.Classes exposing (module_)

import Elm
import Elm.Annotation
import Elm.Arg
import Elm.Case
import Elm.Declare
import Elm.Declare.Extra
import Generate.Utils exposing (yassify)
import Parsers
import String.Extra


module_ : List ( Maybe String, Parsers.Class ) -> Elm.Declare.Module ClassesModule
module_ dlcClasses =
    Elm.Declare.module_ [ "Generated", "Classes" ] ClassesModule
        |> Elm.Declare.with (allClasses dlcClasses)
        |> Elm.Declare.with (classToColor dlcClasses)
        |> Elm.Declare.with classDetails.declaration
        |> Elm.Declare.Extra.withDeclarations (dlcToClasses dlcClasses)


classDetails :
    { declaration : Elm.Declare.Annotation
    , make :
        { content : Elm.Expression
        , color : Elm.Expression
        , dlc : Elm.Expression
        , name : Elm.Expression
        }
        -> Elm.Expression
    }
classDetails =
    Elm.Declare.Extra.customRecord "Details"
        |> Elm.Declare.Extra.withField "name" .name (Elm.Annotation.named [ "Generated", "Types" ] "Class")
        |> Elm.Declare.Extra.withField "dlc" .dlc (Elm.Annotation.maybe Elm.Annotation.string)
        |> Elm.Declare.Extra.withField "color" .color Elm.Annotation.int
        |> Elm.Declare.Extra.withField "content" .content Elm.Annotation.string
        |> Elm.Declare.Extra.buildCustomRecord


type alias ClassesModule =
    { all : Elm.Expression
    , classToColor : Elm.Expression -> Elm.Expression
    , classDetails : Elm.Annotation.Annotation
    }


allClasses : List ( Maybe String, Parsers.Class ) -> Elm.Declare.Value
allClasses dlcClasses =
    dlcClasses
        |> List.map (\( _, class ) -> Elm.val (String.Extra.decapitalize (yassify class.name)))
        |> Elm.list
        |> Elm.withType (Elm.Annotation.list classDetails.declaration.annotation)
        |> Elm.Declare.value "all"


classToColor : List ( Maybe String, Parsers.Class ) -> Elm.Declare.Function (Elm.Expression -> Elm.Expression)
classToColor dlcClasses =
    Elm.Declare.fn "classToColor"
        (Elm.Arg.var "class")
        (\class ->
            dlcClasses
                |> List.map
                    (\( _, classData ) ->
                        Elm.Case.branch
                            (Elm.Arg.customType classData.name ())
                            (\() -> Elm.hex classData.color)
                    )
                |> Elm.Case.custom class (Elm.Annotation.named [ "Generated", "Types" ] "Class")
        )


dlcToClasses : List ( Maybe String, Parsers.Class ) -> List Elm.Declaration
dlcToClasses classes =
    List.map
        (\( dlcName, class ) ->
            classDetails.make
                { name = fromTypes class.name
                , dlc = Elm.maybe (Maybe.map Elm.string dlcName)
                , color = Elm.hex class.color
                , content = Elm.string class.description
                }
                |> Elm.declaration (yassify class.name)
                |> Elm.expose
        )
        classes


fromTypes : String -> Elm.Expression
fromTypes name =
    Elm.value
        { importFrom = [ "Generated", "Types" ]
        , name = yassify name
        , annotation = Nothing
        }
