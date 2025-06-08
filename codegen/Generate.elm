module Generate exposing (main)

{-| -}

import Dict
import Elm
import Elm.Declare
import Gen.CodeGen.Generate as Generate exposing (Directory)
import Generate.FromDLC
import Generate.Gradients
import Generate.Images
import Json.Decode exposing (Decoder, Value)
import Result.Extra
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
                        Ok ( [ fileContent ], [], [] )

                    _ ->
                        if String.endsWith "_gradient.ppm" fileName then
                            Ok ( [], [ ( fileName, fileContent ) ], [] )

                        else if String.endsWith ".md" fileName then
                            Ok ( [], [], [ ( folder, fileName, fileContent ) ] )

                        else
                            Err
                                [ { title = "Unexpected file"
                                  , description = "File " ++ fileName ++ " unexpected, don’t know how to handle it"
                                  }
                                ]
            )
        |> Result.andThen
            (\list ->
                Result.map3
                    (\gradientsFile imagesFile dlcFiles ->
                        { info = []
                        , files = gradientsFile :: imagesFile :: dlcFiles
                        }
                    )
                    (List.concatMap Triple.Extra.second list
                        |> Generate.Gradients.gradients
                    )
                    (List.concatMap Triple.Extra.first list
                        |> Generate.Images.images
                        |> Result.map Elm.Declare.toFile
                    )
                    (List.concatMap Triple.Extra.third list
                        |> Generate.FromDLC.files
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
