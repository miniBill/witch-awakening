module Generate.Class exposing (file)

import Elm
import Elm.Annotation
import Elm.Arg
import Elm.Case
import Elm.Declare
import Elm.Declare.Extra
import Gen.Color
import Generate.Enum as Enum exposing (Enum)
import Generate.Types exposing (TypesModule)
import Generate.Utils as Utils exposing (yassify)
import Parsers
import String.Extra


type alias ClassModule =
    { all : Elm.Expression
    , toColor : Elm.Expression -> Elm.Expression
    , toString : Elm.Expression -> Elm.Expression
    , details : Elm.Annotation.Annotation
    }


file : TypesModule -> Enum -> List ( Maybe String, Parsers.Class ) -> Elm.Declare.Module ClassModule
file types enum dlcClasses =
    Elm.Declare.module_ [ "Generated", "Class" ] ClassModule
        |> Elm.Declare.with (all types dlcClasses)
        |> Elm.Declare.with (toColor types dlcClasses)
        |> Elm.Declare.with (Enum.toString enum)
        |> Elm.Declare.with (details types)
        |> Elm.Declare.withDeclarations (dlcToClasses types dlcClasses)


details :
    TypesModule
    ->
        Elm.Declare.Extra.Record
            { content : Elm.Expression
            , color : Elm.Expression
            , dlc : Elm.Expression
            , name : Elm.Expression
            }
details types =
    Elm.Declare.record "Details"
        |> Elm.Declare.withField "name" .name types.class.annotation
        |> Elm.Declare.withField "dlc" .dlc (Elm.Annotation.maybe Elm.Annotation.string)
        |> Elm.Declare.withField "color" .color Gen.Color.annotation_.color
        |> Elm.Declare.withField "content" .content Elm.Annotation.string
        |> Elm.Declare.buildRecord


all : TypesModule -> List ( Maybe String, Parsers.Class ) -> Elm.Declare.Value
all types dlcClasses =
    dlcClasses
        |> List.map
            (\( _, class ) ->
                yassify class.name
                    |> String.Extra.decapitalize
                    |> Elm.val
            )
        |> Elm.list
        |> Elm.withType (Elm.Annotation.list (details types).annotation)
        |> Elm.Declare.value "all"


toColor : TypesModule -> List ( Maybe String, Parsers.Class ) -> Elm.Declare.Function (Elm.Expression -> Elm.Expression)
toColor types dlcClasses =
    Elm.Declare.fn "toColor"
        (Elm.Arg.var "class")
        (\class ->
            dlcClasses
                |> List.map
                    (\( _, classData ) ->
                        Elm.Case.branch
                            (types.class.argWith classData.name [])
                            (\_ ->
                                Utils.color classData.color
                            )
                    )
                |> Elm.Case.custom class types.class.annotation
        )


dlcToClasses : TypesModule -> List ( Maybe String, Parsers.Class ) -> List Elm.Declaration
dlcToClasses types classes =
    List.map
        (\( dlcName, class ) ->
            (details types).make
                { name = types.class.value class.name
                , dlc = Elm.maybe (Maybe.map Elm.string dlcName)
                , color = Utils.color class.color
                , content = Elm.string class.description
                }
                |> Elm.declaration (yassify class.name)
                |> Elm.expose
        )
        classes
