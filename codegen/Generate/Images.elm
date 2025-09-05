module Generate.Images exposing (ImagesModule, images, valueFrom)

import Dict
import Dict.Extra
import Elm
import Elm.Annotation
import Elm.Declare
import Elm.Declare.Extra
import Gen.CodeGen.Generate as Generate
import Parser exposing ((|.), (|=), Parser)
import Result.Extra
import String.Extra
import Triple.Extra


type alias ImagesModule =
    { image : Elm.Annotation.Annotation }


moduleName : List String
moduleName =
    [ "Images" ]


images : List String -> Result (List Generate.Error) (Elm.Declare.Module ImagesModule)
images sizesList =
    sizesList
        |> List.concatMap String.lines
        |> List.filter (\line -> not (String.isEmpty line))
        |> Result.Extra.combineMap
            (\line ->
                case String.split " " line of
                    filePath :: "PNG" :: size :: _ ->
                        fromLine filePath size

                    filePath :: "WEBP" :: size :: _ ->
                        fromLine filePath size

                    _ ->
                        Err <| "Wrong line: " ++ line
            )
        |> Result.map addGroups
        |> Result.Extra.mapBoth
            (\e ->
                [ { title = "Error"
                  , description = e
                  }
                ]
            )
            (\declarations ->
                Elm.Declare.module_ moduleName ImagesModule
                    |> Elm.Declare.with imageType
                    |> Elm.Declare.Extra.withDeclarations declarations
            )


type alias ImageData =
    { name : String
    , section : String
    , declaration : Elm.Declaration
    , group : Maybe { name : String, index : Int }
    }


fromLine : String -> String -> Result String ImageData
fromLine filePath size =
    let
        fileName : Maybe String
        fileName =
            filePath
                |> String.split "/"
                |> List.drop 1
                |> List.concatMap (String.split ".")
                |> List.head
    in
    case
        ( fileName
        , List.map String.toInt <| String.split "x" size
        )
    of
        ( Just name, [ Just width, Just height ] ) ->
            let
                declaration : Elm.Declaration
                declaration =
                    [ ( "width", Elm.int width )
                    , ( "height", Elm.int height )
                    , ( "src", Elm.string filePath )
                    ]
                        |> Elm.record
                        |> Elm.withType (Elm.Annotation.named [] "Image")
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

        _ ->
            Err <| "Unexpected size: " ++ size


addGroups : List ImageData -> List Elm.Declaration
addGroups =
    \declarations ->
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
                            , Elm.val name
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


imageType :
    { annotation : Elm.Annotation.Annotation
    , declaration : Elm.Declaration
    , internal : Elm.Declare.Internal Elm.Annotation.Annotation
    , make : { src : Elm.Expression, height : Elm.Expression, width : Elm.Expression } -> Elm.Expression
    }
imageType =
    Elm.Declare.Extra.customRecord "Image"
        |> Elm.Declare.Extra.withField "width" .width Elm.Annotation.int
        |> Elm.Declare.Extra.withField "height" .height Elm.Annotation.int
        |> Elm.Declare.Extra.withField "src" .src Elm.Annotation.string
        |> Elm.Declare.Extra.buildCustomRecord


imageGroupParser : Parser ( String, Int )
imageGroupParser =
    Parser.succeed Tuple.pair
        |= Parser.getChompedString
            (Parser.chompIf Char.isAlpha
                |. Parser.chompWhile Char.isAlpha
            )
        |= Parser.int
        |. Parser.end


valueFrom : String -> Elm.Expression
valueFrom name =
    Elm.value
        { importFrom = moduleName
        , name = name
        , annotation = Nothing
        }
