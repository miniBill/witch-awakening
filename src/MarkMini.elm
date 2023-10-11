module MarkMini exposing (Block(..), Color(..), Piece(..), blockParser)

import Generated.Types exposing (Class(..), Slot(..))
import Parser exposing ((|.), (|=), Parser)
import Result.Extra
import Set exposing (Set)


type Block
    = SectionTitle String
    | Paragraph { pieces : List Piece, center : Bool }
    | UnorderedList (List (List Piece))


type Piece
    = Speech (List Piece)
    | Colored Color (List Piece)
    | Italic (List Piece)
    | Underlined (List Piece)
    | Bold (List Piece)
    | Slot Slot
    | Text String
    | Number Int


type Color
    = ChoiceColor
    | ClassColor Class
    | SlotColor Slot


blockParser : Parser Block
blockParser =
    Parser.oneOf
        [ Parser.succeed (SectionTitle << String.trim)
            |. Parser.symbol "#"
            |. Parser.spaces
            |= Parser.getChompedString (Parser.chompWhile (\c -> c /= '\n'))
        , Parser.succeed UnorderedList
            |. Parser.symbol "-"
            |. Parser.spaces
            |= unorderedListParser
        , Parser.succeed (\pieces -> Paragraph { pieces = pieces, center = True })
            |. Parser.symbol "[center]"
            |= mainParser
        , Parser.succeed (\pieces -> Paragraph { pieces = pieces, center = False })
            |= mainParser
        ]


unorderedListParser : Parser (List (List Piece))
unorderedListParser =
    Parser.getChompedString (Parser.chompUntilEndOr "\n\n")
        |> Parser.andThen
            (\chomped ->
                case
                    chomped
                        |> String.split "\n"
                        |> Result.Extra.combineMap
                            (\line ->
                                let
                                    trimmed : String
                                    trimmed =
                                        String.trim line
                                in
                                Parser.run mainParser
                                    (if String.startsWith "-" trimmed then
                                        String.dropLeft 1 trimmed

                                     else
                                        trimmed
                                    )
                            )
                of
                    Ok res ->
                        Parser.succeed res

                    Err _ ->
                        Parser.problem "Failed to parse item"
            )


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
        , Parser.succeed (Text "\"")
            |. Parser.symbol "\\\""
        , Parser.succeed (Text "*")
            |. Parser.symbol "\\*"
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
        , Parser.succeed identity
            |. Parser.symbol "("
            |= Parser.oneOf
                [ Parser.succeed Slot
                    |= Parser.oneOf
                        [ Parser.succeed Folk |. Parser.symbol "folk"
                        , Parser.succeed Noble |. Parser.symbol "noble"
                        , Parser.succeed Heroic |. Parser.symbol "heroic"
                        , Parser.succeed Epic |. Parser.symbol "epic"
                        , Parser.succeed White |. Parser.symbol "white"
                        ]
                    |. Parser.symbol ")"
                , Parser.succeed (Text "(")
                ]
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
                , Parser.succeed (SlotColor Folk)
                    |. Parser.symbol "folk"
                , Parser.succeed (SlotColor Noble)
                    |. Parser.symbol "noble"
                , Parser.succeed (SlotColor Heroic)
                    |. Parser.symbol "heroic"
                , Parser.succeed (SlotColor Epic)
                    |. Parser.symbol "epic"
                , Parser.succeed (SlotColor White)
                    |. Parser.symbol "white"
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
    , '\\'
    , '('
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
