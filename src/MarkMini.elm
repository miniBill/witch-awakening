module MarkMini exposing (Block(..), Color(..), Piece(..), blockParser)

import Generated.Types exposing (Class(..))
import Parser exposing ((|.), (|=), Parser)
import Set exposing (Set)


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
    | Number Int


type Color
    = ChoiceColor
    | ClassColor Class


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
        , Parser.succeed
            (\str ->
                case String.toInt str of
                    Nothing ->
                        Text ("[" ++ str ++ "]")

                    Just i ->
                        Number i
            )
            |. Parser.symbol "["
            |= Parser.getChompedString (Parser.chompWhile (\c -> c /= ']'))
            |. Parser.symbol "]"
        , Parser.succeed Colored
            |. Parser.symbol "{"
            |= Parser.oneOf
                [ Parser.succeed ChoiceColor
                    |. Parser.symbol "choice"
                , Parser.succeed (ClassColor Academic)
                    |. Parser.symbol "academic"
                , Parser.succeed (ClassColor Sorceress)
                    |. Parser.symbol "sorceress"
                , Parser.succeed (ClassColor Warlock)
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
