module Generate.Gradient exposing (gradient, gradients, suffix)

import Elm
import Gen.CodeGen.Generate as Generate
import List.Extra
import Path exposing (Path)
import ResultME exposing (ResultME)


gradients : List { path : Path, content : String } -> ResultME String Elm.File
gradients files =
    files
        |> ResultME.combineMap gradient
        |> Result.map (Elm.file [ "Generated", "Gradient" ])


gradient : { path : Path, content : String } -> ResultME String Elm.Declaration
gradient { path, content } =
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
                        let
                            name : String
                            name =
                                String.dropRight (String.length suffix) (Path.toString path)
                        in
                        expr
                            |> Elm.declaration (name ++ "Gradient")
                            |> Elm.expose
                    )

        _ ->
            ResultME.error "Could not parse file"


suffix : String
suffix =
    "_gradient.ppm"


rowsToExpression : List (List Int) -> ResultME String Elm.Expression
rowsToExpression rows =
    rows
        |> List.concat
        |> List.Extra.greedyGroupsOf 3
        |> ResultME.combineMap parseRow
        |> Result.map Elm.list


parseRow : List Int -> ResultME String Elm.Expression
parseRow row =
    case row of
        [ r, g, b ] ->
            Elm.triple (Elm.int r) (Elm.int g) (Elm.int b)
                |> Ok

        _ ->
            let
                msg : String
                msg =
                    "Row \""
                        ++ String.join " " (List.map String.fromInt row)
                        ++ "\" is not valid"
            in
            ResultME.error msg
