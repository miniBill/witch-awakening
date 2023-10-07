module Generate exposing (main)

{-| -}

import Dict
import Elm
import Elm.Declare
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
            case Dict.get "sizes" files of
                Just sizes ->
                    images sizes

                Nothing ->
                    Err [ { title = "Missing file", description = "File `sizes` is missing" } ]


images : String -> Result (List { title : String, description : String }) { info : List String, files : List Elm.File }
images sizes =
    let
        fromLine : String -> String -> Result String ( Int, Elm.Expression, Elm.Declaration )
        fromLine filePath size =
            let
                maybeNumber =
                    filePath
                        |> String.split "/"
                        |> List.drop 1
                        |> List.concatMap (String.split "-")
                        |> List.head
                        |> Maybe.andThen String.toInt
            in
            case
                ( maybeNumber
                , List.map String.toInt <| String.split "x" size
                )
            of
                ( Just number, [ Just width, Just height ] ) ->
                    let
                        { declaration, value } =
                            [ ( "width", Elm.int width )
                            , ( "height", Elm.int height )
                            , ( "url", Elm.string filePath )
                            ]
                                |> Elm.record
                                |> Elm.Declare.value ("image_" ++ String.fromInt number)
                    in
                    Ok ( number, value, declaration )

                _ ->
                    Err <| "Unexpected size: " ++ size
    in
    sizes
        |> String.split "\n"
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
                let
                    listDeclaration : Elm.Declaration
                    listDeclaration =
                        declarations
                            |> List.sortBy (\( number, _, _ ) -> number)
                            |> List.map (\( _, value, _ ) -> value)
                            |> Elm.list
                            |> Elm.declaration "list"
                            |> Elm.expose
                in
                { info = []
                , files =
                    [ Elm.file [ "Images" ]
                        (listDeclaration
                            :: List.map
                                (\( _, _, declaration ) -> declaration)
                                declarations
                        )
                    ]
                }
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
