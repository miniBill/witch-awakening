module Parser.Error exposing (toString)

import Parser


toString : List Parser.DeadEnd -> String
toString deadEnds =
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
