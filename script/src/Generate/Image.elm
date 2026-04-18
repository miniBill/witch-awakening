module Generate.Image exposing (ImageModule, file, moduleName, valueFrom)

import BuildTask exposing (FileOrDirectory)
import Buildfile
import Dict
import Dict.Extra
import Elm
import Elm.Annotation
import Elm.Arg
import Elm.Declare
import Elm.Declare.Extra
import Elm.Let
import Elm.Op
import Elm.Op.Extra
import Gen.CodeGen.Generate as Generate
import Gen.Html
import Gen.Html.Attributes
import Gen.Html.Picture
import Gen.Html.Source
import Gen.List
import Gen.String
import Generate.Utils exposing (yassify)
import Parser exposing ((|.), (|=), Parser)
import Path exposing (Path)
import ResultME exposing (ResultME)
import String.Extra
import Triple.Extra


type alias ImageModule =
    { valueFrom : String -> Elm.Expression
    , image : Elm.Annotation.Annotation
    , toPicture : Elm.Expression -> Elm.Expression -> Elm.Expression
    }


moduleName : List String
moduleName =
    [ "Generated", "Image" ]


file : List { svg : Bool, width : Int, height : Int, filename : Path, hash : FileOrDirectory } -> ResultME Generate.Error (Elm.Declare.Module ImageModule)
file sizesList =
    sizesList
        |> ResultME.combineMap fileToDeclaration
        |> Result.map
            (\declarations ->
                Elm.Declare.module_ moduleName
                    (\image toPicture_ ->
                        { valueFrom =
                            \name ->
                                Elm.value
                                    { name = name
                                    , importFrom = moduleName
                                    , annotation = Just image
                                    }
                        , image = image
                        , toPicture = toPicture_
                        }
                    )
                    |> Elm.Declare.with imageType
                    |> Elm.Declare.with toPicture
                    |> Elm.Declare.withUnexposed toSources
                    |> Elm.Declare.withUnexposed getSizesDeclaration
                    |> Elm.Declare.withUnexposed Buildfile.standardFormats
                    |> Elm.Declare.withDeclarations (addGroups declarations)
            )


type alias ImageData =
    { name : String
    , section : String
    , declaration : Elm.Declaration
    , group : Maybe { name : String, index : Int }
    }


fileToDeclaration : { svg : Bool, width : Int, height : Int, filename : Path, hash : FileOrDirectory } -> ResultME Generate.Error ImageData
fileToDeclaration { filename, width, height } =
    let
        name : String
        name =
            Path.toString (Path.directory filename)
                ++ "/"
                ++ Path.filenameWithoutExtension filename
                |> yassify

        declaration : Elm.Declaration
        declaration =
            [ ( "width", Elm.int width )
            , ( "height", Elm.int height )
            , ( "src", Elm.string (Path.toString filename) )
            ]
                |> Elm.record
                |> Elm.withType imageType.annotation
                |> Elm.declaration name

        section : String
        section =
            name
                |> String.Extra.humanize
                |> String.split " "
                |> List.take 1
                |> String.join " "
    in
    { name = name
    , section = section
    , declaration = declaration
    , group =
        Parser.run imageGroupParser name
            |> Result.toMaybe
            |> Maybe.map
                (\( groupName, indexInGroup ) ->
                    { name = groupName
                    , index = indexInGroup
                    }
                )
    }
        |> Ok


addGroups : List ImageData -> List Elm.Declaration
addGroups declarations =
    let
        groupedDeclarations : List Elm.Declaration
        groupedDeclarations =
            declarations
                |> Dict.Extra.groupBy .section
                |> Dict.map
                    (\section decls ->
                        Elm.group (Elm.docs ("## " ++ section) :: List.map .declaration decls)
                    )
                |> Dict.values

        declarationsForGroups : Elm.Declaration
        declarationsForGroups =
            declarations
                |> List.filterMap
                    (\{ name, group } ->
                        Maybe.map (\g -> ( name, g.name, g.index )) group
                    )
                |> Dict.Extra.groupBy Triple.Extra.second
                |> Dict.map groupDeclaration
                |> Dict.values
                |> (::) (Elm.docs "## Groups")
                |> Elm.group

        groupDeclaration : String -> List ( String, String, Int ) -> Elm.Declaration
        groupDeclaration groupName decls =
            decls
                |> List.map
                    (\( name, _, index ) ->
                        ( "image" ++ String.fromInt index
                        , Elm.val (String.Extra.decapitalize name)
                            |> Elm.withType imageType.annotation
                        )
                    )
                |> Elm.record
                |> Elm.declaration groupName
    in
    (groupedDeclarations
        ++ [ declarationsForGroups ]
    )
        |> List.map Elm.expose


imageType : Elm.Declare.Extra.Record { src : Elm.Expression, height : Elm.Expression, width : Elm.Expression }
imageType =
    Elm.Declare.record "Image"
        |> Elm.Declare.withField "width" .width Elm.Annotation.int
        |> Elm.Declare.withField "height" .height Elm.Annotation.int
        |> Elm.Declare.withField "src" .src Elm.Annotation.string
        |> Elm.Declare.buildRecord
        |> Elm.Declare.withDocumentation "Image data"


imageGroupParser : Parser ( String, Int )
imageGroupParser =
    Parser.succeed Tuple.pair
        |= Parser.getChompedString
            (Parser.chompIf Char.isAlpha
                |. Parser.chompWhile Char.isAlpha
            )
        |= Parser.int
        |. Parser.end
        |> Parser.andThen
            (\( k, i ) ->
                if i > 100 then
                    Parser.problem "Not a grouped images"

                else
                    Parser.succeed ( String.Extra.decapitalize k, i )
            )


valueFrom : String -> Elm.Expression
valueFrom name =
    Elm.value
        { importFrom = moduleName
        , name = name
        , annotation = Nothing
        }


minSize : number
minSize =
    50


getSizesDeclaration : Elm.Declare.Function (Elm.Expression -> Elm.Expression)
getSizesDeclaration =
    Elm.Declare.fn "getSizes" (Elm.Arg.varWith "width" Elm.Annotation.int) <| \width ->
    Elm.Let.letIn identity
        |> Elm.Let.fn2 "go"
            (Elm.Arg.varWith "factor" Elm.Annotation.int)
            (Elm.Arg.varWith "acc" (Elm.Annotation.list Elm.Annotation.int))
            (\factor acc ->
                Elm.Let.letIn identity
                    |> Elm.Let.value "w" (Elm.Op.intDivide width factor)
                    |> Elm.Let.withBody
                        (\w ->
                            Elm.ifThen (Elm.Op.gte w (Elm.int minSize))
                                (Elm.apply (Elm.val "go") [ Elm.Op.multiply factor (Elm.int 2), Elm.Op.cons w acc ])
                                (Gen.List.call_.reverse acc)
                        )
                    |> Elm.withType (Elm.Annotation.list Elm.Annotation.int)
            )
        |> Elm.Let.withBody
            (\go ->
                go (Elm.int 1) (Elm.list [])
                    |> Elm.withType (Elm.Annotation.list Elm.Annotation.int)
            )


toPicture : Elm.Declare.Function (Elm.Expression -> Elm.Expression -> Elm.Expression)
toPicture =
    Elm.Declare.fn2 "toPicture"
        (Elm.Arg.varWith "attrs"
            (Elm.Annotation.list (Gen.Html.annotation_.attribute (Elm.Annotation.var "msg")))
        )
        (Elm.Arg.varWith "image" imageType.annotation)
    <| \attrs image ->
    Gen.Html.Picture.call_.picture
        (Elm.Op.cons
            (Gen.Html.Attributes.call_.width (image |> Elm.get "width"))
            (Elm.Op.cons
                (Gen.Html.Attributes.call_.height (image |> Elm.get "height"))
                attrs
            )
        )
        (Elm.record
            [ ( "sources"
              , Buildfile.standardFormats.value
                    |> Gen.List.call_.map
                        (Elm.functionReduced "format"
                            (toSources.call
                                image
                            )
                        )
              )
            , ( "src", Elm.Op.append (Elm.string "public/") (image |> Elm.get "src") )
            , ( "alt", Elm.maybe Nothing )
            ]
        )
        |> Elm.withType (Gen.Html.annotation_.html (Elm.Annotation.var "msg"))


toSources : Elm.Declare.Function (Elm.Expression -> Elm.Expression -> Elm.Expression)
toSources =
    Elm.Declare.fn2 "toSources"
        (Elm.Arg.varWith "image" imageType.annotation)
        (Elm.Arg.varWith "config"
            (Elm.Annotation.record
                [ ( "extension", Elm.Annotation.string )
                , ( "format", Gen.Html.Source.annotation_.imageType )
                ]
            )
        )
    <| \image config ->
    Elm.Let.letIn identity
        |> Elm.Let.value "base"
            (image
                |> Elm.get "src"
                |> Gen.String.call_.split (Elm.string ".")
                |> Gen.List.call_.reverse
                |> Gen.List.call_.drop (Elm.int 1)
                |> Gen.List.call_.reverse
                |> Gen.String.call_.join (Elm.string ".")
            )
        |> Elm.Let.withBody
            (\base ->
                getSizesDeclaration.call (image |> Elm.get "width")
                    |> Gen.List.call_.map
                        (Elm.fn (Elm.Arg.varWith "w" Elm.Annotation.int) <| \w ->
                        Elm.record
                            [ ( "url"
                              , Elm.Op.Extra.appendStrings
                                    [ Elm.string "public/"
                                    , base
                                    , Elm.string "-"
                                    , Gen.String.call_.fromInt w
                                    , Elm.string "."
                                    , Elm.get "extension" config
                                    ]
                              )
                            , ( "width", Elm.maybe (Just w) )
                            ]
                        )
                    |> Gen.Html.Source.call_.fromImagesAndWidths
                    |> Gen.Html.Source.withType (Elm.get "format" config)
            )
        |> Elm.withType (Gen.Html.Source.annotation_.source Gen.Html.Source.annotation_.withWidths)
