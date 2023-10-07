module Generate exposing (main)

{-| -}

import Elm
import Elm.Declare
import Gen.CodeGen.Generate as Generate
import Result.Extra


main : Program String () ()
main =
    Generate.withFeedback toFiles


toFiles :
    String
    ->
        Result
            (List Generate.Error)
            { info : List String, files : List Elm.File }
toFiles sizes =
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
