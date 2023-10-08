module MarkMini exposing (Block(..), Color(..), Piece(..), blockParser)

import Parser exposing ((|.), (|=), Parser)
import Set exposing (Set)
import Types exposing (Class(..))


type Block
    = SectionTitle String
    | Paragraph { pieces : List Piece, center : Bool }


type Piece
    = Speech (List Piece)
    | Colored Color (List Piece)
    | Italic (List Piece)
    | Underlined (List Piece)
    | Bold (List Piece)
    | Text String
    | Number String


type Color
    = Choice
    | Class Class


blockParser : Parser Block
blockParser =
    Parser.oneOf
        [ Parser.succeed (SectionTitle << String.trim)
            |. Parser.symbol "#"
            |. Parser.spaces
            |= Parser.getChompedString (Parser.chompWhile (\c -> c /= '\n'))
        , Parser.succeed (\pieces -> Paragraph { pieces = pieces, center = True })
            |. Parser.symbol "[center]"
            |= mainParser
        , Parser.succeed (\pieces -> Paragraph { pieces = pieces, center = False })
            |= mainParser
        ]


mainParser : Parser (List Piece)
mainParser =
    Parser.oneOf
        [ Parser.succeed Speech
            |. Parser.symbol "\""
            |= innerParser '"'
            |. Parser.symbol "\""
        , Parser.succeed Underlined
            |. Parser.symbol "__"
            |= innerParser '_'
            |. Parser.symbol "__"
        , Parser.succeed Italic
            |. Parser.symbol "_"
            |= innerParser '_'
            |. Parser.symbol "_"
        , Parser.succeed Bold
            |. Parser.symbol "*"
            |= innerParser '*'
            |. Parser.symbol "*"
        , Parser.succeed Number
            |. Parser.symbol "["
            |= Parser.getChompedString (Parser.chompWhile (\c -> c /= ']'))
            |. Parser.symbol "]"
        , Parser.succeed Colored
            |. Parser.symbol "{"
            |= Parser.oneOf
                [ Parser.succeed Choice
                    |. Parser.symbol "choice"
                , Parser.succeed (Class Academic)
                    |. Parser.symbol "academic"
                , Parser.succeed (Class Sorceress)
                    |. Parser.symbol "sorceress"
                , Parser.succeed (Class Warlock)
                    |. Parser.symbol "warlock"
                ]
            |. Parser.symbol " "
            |= innerParser '}'
            |. Parser.symbol "}"
        , Parser.succeed Text
            |= Parser.getChompedString
                (Parser.succeed ()
                    |. Parser.chompIf (\c -> not (Set.member c special))
                    |. Parser.chompWhile (\c -> not (Set.member c special))
                )
        ]
        |> many


innerParser : Char -> Parser (List Piece)
innerParser until =
    Parser.chompWhile (\c -> c /= until)
        |> Parser.getChompedString
        |> Parser.andThen
            (\chomped ->
                case Parser.run mainParser chomped of
                    Ok res ->
                        Parser.succeed res

                    Err deadEnds ->
                        Parser.problem <| "Error in inner parser: " ++ Parser.deadEndsToString deadEnds
            )


special : Set Char
special =
    [ '_'
    , '"'
    , '*'
    , '{'
    , '['
    ]
        |> Set.fromList


many : Parser a -> Parser (List a)
many parser =
    Parser.loop []
        (\acc ->
            Parser.oneOf
                [ Parser.succeed
                    (\e ->
                        Parser.Loop (e :: acc)
                    )
                    |= parser
                , Parser.succeed
                    (\_ ->
                        Parser.Done <| List.reverse acc
                    )
                    |= Parser.end
                ]
        )