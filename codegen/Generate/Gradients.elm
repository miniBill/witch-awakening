module Generate.Gradients exposing (gradients)

import Elm
import Gen.CodeGen.Generate as Generate
import Result.Extra


gradients : List ( String, String ) -> Result (List Generate.Error) Elm.File
gradients files =
    files
        |> Result.Extra.combineMap gradient
        |> Result.map (Elm.file [ "Gradients" ])


gradient : ( String, String ) -> Result (List Generate.Error) Elm.Declaration
gradient ( fileName, content ) =
    let
        name : String
        name =
            String.dropRight (String.length "_gradient.ppm") fileName
    in
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
            rowsToExpression rows
                |> Result.map
                    (\expr ->
                        expr
                            |> Elm.declaration (name ++ "Gradient")
                            |> Elm.expose
                    )

        _ ->
            Err [ { title = "Invalid file", description = "Could not parse file" } ]


rowsToExpression : List (List Int) -> Result (List Generate.Error) Elm.Expression
rowsToExpression rows =
    rows
        |> Result.Extra.combineMap parseRow
        |> Result.map Elm.list


parseRow : List Int -> Result (List Generate.Error) Elm.Expression
parseRow row =
    case row of
        [ r, g, b ] ->
            Ok (Elm.triple (Elm.int r) (Elm.int g) (Elm.int b))

        _ ->
            Err
                [ { title = "Invalid row"
                  , description =
                        "Row \""
                            ++ String.join " " (List.map String.fromInt row)
                            ++ "\" is not valid"
                  }
                ]
