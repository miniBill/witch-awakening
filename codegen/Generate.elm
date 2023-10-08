module Generate exposing (main)

{-| -}

import Dict
import Elm
import Elm.Annotation
import Gen.CodeGen.Generate as Generate
import Json.Decode exposing (Decoder, Value)
import Result.Extra


main : Program Value () ()
main =
    Generate.withFeedback toFiles


toFiles :
    Value
    ->
        Result
            (List Generate.Error)
            { info : List String, files : List Elm.File }
toFiles flags =
    case Json.Decode.decodeValue directoryDecoder flags of
        Err _ ->
            Err [ { title = "Invalid flags", description = "Could not decode flags" } ]

        Ok (Generate.Directory { files }) ->
            files
                |> Dict.toList
                |> Result.Extra.combineMap
                    (\( fileName, fileContent ) ->
                        case fileName of
                            "sizes" ->
                                images fileContent
                                    |> Result.map
                                        (\file ->
                                            ( [ file ], [] )
                                        )

                            _ ->
                                if String.endsWith "_gradient.ppm" fileName then
                                    gradient (String.dropRight (String.length "_gradient.ppm") fileName) fileContent
                                        |> Result.map (\declaration -> ( [], [ declaration ] ))

                                else
                                    Err
                                        [ { title = "Unexpected file"
                                          , description = "File " ++ fileName ++ " unexpected, don't know how to handle it"
                                          }
                                        ]
                    )
                |> Result.map
                    (\list ->
                        let
                            gradients : Elm.File
                            gradients =
                                Elm.file [ "Gradients" ]
                                    (List.concatMap Tuple.second list)
                        in
                        { info = []
                        , files = gradients :: List.concatMap Tuple.first list
                        }
                    )


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


images : String -> Result (List Generate.Error) Elm.File
images sizes =
    let
        fromLine : String -> String -> Result String Elm.Declaration
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
                                |> Elm.expose
                    in
                    Ok declaration

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
                Elm.file [ "Images" ]
                    (Elm.expose
                        (Elm.alias "Image"
                            (Elm.Annotation.record
                                [ ( "width", Elm.Annotation.int )
                                , ( "height", Elm.Annotation.int )
                                , ( "src", Elm.Annotation.string )
                                ]
                            )
                        )
                        :: List.map
                            (\declaration -> declaration)
                            declarations
                    )
            )


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
