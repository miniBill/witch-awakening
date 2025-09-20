module Generate.Classes exposing (file)

import Elm
import Elm.Annotation
import Elm.Arg
import Elm.Case
import Elm.Declare
import Elm.Declare.Extra
import Generate.Types exposing (TypesModule)
import Generate.Utils exposing (yassify)
import Parsers
import String.Extra


type alias ClassesModule =
    { all : Elm.Expression
    , classToColor : Elm.Expression -> Elm.Expression
    , classDetails : Elm.Annotation.Annotation
    }


file : TypesModule -> List ( Maybe String, Parsers.Class ) -> Elm.Declare.Module ClassesModule
file types dlcClasses =
    Elm.Declare.module_ [ "Generated", "Classes" ] ClassesModule
        |> Elm.Declare.with (all types dlcClasses)
        |> Elm.Declare.with (classToColor types dlcClasses)
        |> Elm.Declare.with (classDetails types)
        |> Elm.Declare.Extra.withDeclarations (dlcToClasses types dlcClasses)


classDetails :
    TypesModule
    ->
        { annotation : Elm.Annotation.Annotation
        , declaration : Elm.Declaration
        , internal : Elm.Declare.Internal Elm.Annotation.Annotation
        , make :
            { content : Elm.Expression
            , color : Elm.Expression
            , dlc : Elm.Expression
            , name : Elm.Expression
            }
            -> Elm.Expression
        }
classDetails types =
    Elm.Declare.Extra.customRecord "Details"
        |> Elm.Declare.Extra.withField "name" .name types.class.annotation
        |> Elm.Declare.Extra.withField "dlc" .dlc (Elm.Annotation.maybe Elm.Annotation.string)
        |> Elm.Declare.Extra.withField "color" .color Elm.Annotation.int
        |> Elm.Declare.Extra.withField "content" .content Elm.Annotation.string
        |> Elm.Declare.Extra.buildCustomRecord


all : TypesModule -> List ( Maybe String, Parsers.Class ) -> Elm.Declare.Value
all types dlcClasses =
    dlcClasses
        |> List.map (\( _, class ) -> Elm.val (String.Extra.decapitalize (yassify class.name)))
        |> Elm.list
        |> Elm.withType (Elm.Annotation.list (classDetails types).annotation)
        |> Elm.Declare.value "all"


classToColor : TypesModule -> List ( Maybe String, Parsers.Class ) -> Elm.Declare.Function (Elm.Expression -> Elm.Expression)
classToColor types dlcClasses =
    Elm.Declare.fn "classToColor"
        (Elm.Arg.var "class")
        (\class ->
            dlcClasses
                |> List.map
                    (\( _, classData ) ->
                        Elm.Case.branch
                            (types.class.argWith classData.name [])
                            (\_ -> Elm.hex classData.color)
                    )
                |> Elm.Case.custom class types.class.annotation
        )


dlcToClasses : TypesModule -> List ( Maybe String, Parsers.Class ) -> List Elm.Declaration
dlcToClasses types classes =
    List.map
        (\( dlcName, class ) ->
            (classDetails types).make
                { name = types.class.value class.name
                , dlc = Elm.maybe (Maybe.map Elm.string dlcName)
                , color = Elm.hex class.color
                , content = Elm.string class.description
                }
                |> Elm.declaration (yassify class.name)
                |> Elm.expose
        )
        classes
