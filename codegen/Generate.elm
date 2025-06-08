module Generate exposing (main)

{-| -}

import Dict
import Dict.Extra
import Elm
import Elm.Annotation
import Gen.CodeGen.Generate as Generate exposing (Directory)
import Generate.FromDLC
import Json.Decode exposing (Decoder, Value)
import Parser exposing ((|.), (|=), Parser)
import Parsers
import Result.Extra
import String.Extra
import Triple.Extra


main : Program Value () ()
main =
    Platform.worker
        { init = init
        , update = \_ model -> ( model, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }


init : Value -> ( (), Cmd () )
init flags =
    case Json.Decode.decodeValue directoryDecoder flags of
        Ok input ->
            ( ()
            , case toFiles input of
                Ok result ->
                    Cmd.batch <|
                        List.map Generate.info result.info
                            ++ [ Generate.files result.files ]

                Err errors ->
                    Generate.error errors
            )

        Err e ->
            ( ()
            , Generate.error
                [ { title = "Error decoding flags"
                  , description = Json.Decode.errorToString e
                  }
                ]
            )


toFiles :
    Directory
    -> Result (List Generate.Error) { info : List String, files : List Elm.File }
toFiles root =
    let
        go : String -> Generate.Directory -> List ( String, String, String )
        go folder (Generate.Directory { files, directories }) =
            List.map
                (\( fileName, fileContent ) ->
                    ( folder, fileName, fileContent )
                )
                (Dict.toList files)
                ++ List.concatMap
                    (\( directoryName, dir ) ->
                        go
                            (if String.isEmpty folder then
                                directoryName

                             else
                                folder ++ "/" ++ directoryName
                            )
                            dir
                    )
                    (Dict.toList directories)
    in
    go "" root
        |> Result.Extra.combineMap
            (\( folder, fileName, fileContent ) ->
                case fileName of
                    "sizes" ->
                        images fileContent
                            |> Result.map
                                (\file ->
                                    ( file, [], [] )
                                )

                    _ ->
                        if String.endsWith "_gradient.ppm" fileName then
                            gradient (String.dropRight (String.length "_gradient.ppm") fileName) fileContent
                                |> Result.map (\declaration -> ( [], [ declaration ], [] ))

                        else if String.endsWith ".md" fileName then
                            parseDLC folder fileName fileContent
                                |> Result.map (\dlc -> ( [], [], [ dlc ] ))

                        else
                            Err
                                [ { title = "Unexpected file"
                                  , description = "File " ++ fileName ++ " unexpected, don’t know how to handle it"
                                  }
                                ]
            )
        |> Result.map
            (\list ->
                let
                    gradientsFile : Elm.File
                    gradientsFile =
                        Elm.file [ "Gradients" ]
                            (List.concatMap Triple.Extra.second list)

                    imagesFile : Elm.File
                    imagesFile =
                        Elm.file [ "Images" ]
                            (List.concatMap Triple.Extra.first list)

                    dlcList : List Parsers.DLC
                    dlcList =
                        List.concatMap Triple.Extra.third list
                            |> Dict.Extra.groupBy (\{ name } -> Maybe.withDefault "" name)
                            |> Dict.foldl
                                (\name grouped acc ->
                                    { name =
                                        if String.isEmpty name then
                                            Nothing

                                        else
                                            Just name
                                    , items = List.concatMap .items grouped
                                    }
                                        :: acc
                                )
                                []
                in
                { info = []
                , files =
                    gradientsFile
                        :: imagesFile
                        :: Generate.FromDLC.files dlcList
                }
            )


parseDLC : String -> String -> String -> Result (List Generate.Error) Parsers.DLC
parseDLC folder filename content =
    Parser.run Parsers.dlc content
        |> Result.mapError
            (\deadEnds ->
                [ { title = "Error parsing DLC file"
                  , description =
                        "Could not parse " ++ folder ++ "/" ++ filename ++ "\n" ++ errorToString deadEnds
                  }
                ]
            )


errorToString : List Parser.DeadEnd -> String
errorToString deadEnds =
    String.join "\n" <|
        List.map deadEndToString deadEnds


deadEndToString : Parser.DeadEnd -> String
deadEndToString deadEnd =
    "At " ++ String.fromInt deadEnd.row ++ ":" ++ String.fromInt deadEnd.col ++ ": " ++ problemToString deadEnd.problem


problemToString : Parser.Problem -> String
problemToString problem =
    case problem of
        Parser.ExpectingInt ->
            "Expecting int"

        Parser.ExpectingHex ->
            "Expecting hex"

        Parser.ExpectingOctal ->
            "Expecting octal"

        Parser.ExpectingBinary ->
            "Expecting binary"

        Parser.ExpectingFloat ->
            "Expecting float"

        Parser.ExpectingNumber ->
            "Expecting number"

        Parser.ExpectingVariable ->
            "Expecting variable"

        Parser.ExpectingSymbol s ->
            "Expecting symbol " ++ s

        Parser.ExpectingKeyword k ->
            "Expecting keyword " ++ k

        Parser.Expecting e ->
            "Expecting " ++ e

        Parser.ExpectingEnd ->
            "Expecting end"

        Parser.UnexpectedChar ->
            "Unexpected char"

        Parser.Problem p ->
            "Problem: " ++ p

        Parser.BadRepeat ->
            "Bad repetition"


gradient :
    String
    -> String
    -> Result (List Generate.Error) Elm.Declaration
gradient name content =
    case
        content
            |> String.dropLeft 1
            |> String.lines
            |> List.map
                (\line ->
                    line
                        |> String.split " "
                        |> List.filter (\number -> not (String.isEmpty number))
                        |> List.filterMap String.toInt
                )
            |> List.filter (\line -> not (List.isEmpty line))
    of
        [ 3 ] :: [ 1, _ {- height -} ] :: [ 255 ] :: rows ->
            rows
                |> Result.Extra.combineMap
                    (\row ->
                        case row of
                            [ r, g, b ] ->
                                Ok ( r, g, b )

                            _ ->
                                Err
                                    [ { title = "Invalid row"
                                      , description =
                                            "Row \""
                                                ++ String.join " " (List.map String.fromInt row)
                                                ++ "\" is not valid"
                                      }
                                    ]
                    )
                |> Result.map
                    (\triples ->
                        triples
                            |> List.map
                                (\( r, g, b ) ->
                                    Elm.triple
                                        (Elm.int r)
                                        (Elm.int g)
                                        (Elm.int b)
                                )
                            |> Elm.list
                            |> Elm.declaration (name ++ "Gradient")
                            |> Elm.expose
                    )

        _ ->
            Err [ { title = "Invalid file", description = "Could not parse file" } ]


images : String -> Result (List Generate.Error) (List Elm.Declaration)
images sizes =
    let
        fromLine :
            String
            -> String
            ->
                Result
                    String
                    { name : String
                    , section : String
                    , declaration : Elm.Declaration
                    , group : Maybe { name : String, index : Int }
                    }
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
    in
    sizes
        |> String.lines
        |> List.filter (\line -> not (String.isEmpty line))
        |> Result.Extra.combineMap
            (\line ->
                case String.split " " line of
                    filePath :: "PNG" :: size :: _ ->
                        fromLine filePath size

                    filePath :: "JPEG" :: size :: _ ->
                        fromLine filePath size

                    filePath :: "WEBP" :: size :: _ ->
                        fromLine filePath size

                    _ ->
                        Err <| "Wrong line: " ++ line
            )
        |> Result.mapError
            (\e ->
                [ { title = "Error"
                  , description = e
                  }
                ]
            )
        |> Result.map
            (\declarations ->
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
                                        |> Elm.withType
                                            (Elm.Annotation.named [] "Image")
                                    )
                                )
                            |> Elm.record
                            |> Elm.declaration groupName
                in
                (Elm.alias "Image"
                    (Elm.Annotation.record
                        [ ( "width", Elm.Annotation.int )
                        , ( "height", Elm.Annotation.int )
                        , ( "src", Elm.Annotation.string )
                        ]
                    )
                    :: groupedDeclarations
                    ++ [ declarationsForGroups ]
                )
                    |> List.map Elm.expose
            )


imageGroupParser : Parser ( String, Int )
imageGroupParser =
    Parser.succeed Tuple.pair
        |= Parser.getChompedString
            (Parser.chompIf Char.isAlpha
                |. Parser.chompWhile Char.isAlpha
            )
        |= Parser.int
        |. Parser.end


directoryDecoder : Decoder Generate.Directory
directoryDecoder =
    Json.Decode.lazy
        (\_ ->
            Json.Decode.oneOf
                [ Json.Decode.map Ok Json.Decode.string
                , Json.Decode.map Err directoryDecoder
                ]
                |> Json.Decode.dict
                |> Json.Decode.map
                    (\entries ->
                        entries
                            |> Dict.toList
                            |> List.foldl
                                (\( name, entry ) ( dirAcc, fileAcc ) ->
                                    case entry of
                                        Ok file ->
                                            ( dirAcc, ( name, file ) :: fileAcc )

                                        Err directory ->
                                            ( ( name, directory ) :: dirAcc, fileAcc )
                                )
                                ( [], [] )
                            |> (\( dirAcc, fileAcc ) ->
                                    Generate.Directory
                                        { directories = Dict.fromList dirAcc
                                        , files = Dict.fromList fileAcc
                                        }
                               )
                    )
        )
