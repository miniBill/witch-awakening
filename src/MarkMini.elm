module MarkMini exposing (Block(..), Color(..), Piece(..), blockParser)

import Generated.Types as Types exposing (Affinity, Class(..), Slot(..))
import List.Extra
import Parser exposing ((|.), (|=), Parser)
import Result.Extra
import Set exposing (Set)


type Block
    = SectionTitle String
    | Paragraph { pieces : List Piece, center : Bool, mono : Bool }
    | UnorderedList (List (List Piece))


type Piece
    = Speech (List Piece)
    | Colored Color (List Piece)
    | Smol (List Piece)
    | Italic (List Piece)
    | Underlined (List Piece)
    | Bold (List Piece)
    | Slot Slot
    | Affinity Affinity
    | Class Class
    | JackOfAll
    | Star
    | Text String
    | Link String
    | Power String
    | Kisses String
    | Warning
    | Error
    | Checkmark
    | RewardPoints String
    | LineBreak


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
            |= Parser.getChompedString (Parser.chompWhile <| \c -> c /= '\n')
        , Parser.succeed UnorderedList
            |. Parser.symbol "-"
            |. Parser.spaces
            |= unorderedListParser
        , Parser.succeed
            (\attrs pieces ->
                Paragraph
                    { pieces = pieces
                    , center = List.member "center" attrs
                    , mono = List.member "mono" attrs
                    }
            )
            |= Parser.backtrackable
                (Parser.sequence
                    { start = "{"
                    , end = "}"
                    , separator = ","
                    , trailing = Parser.Optional
                    , spaces = Parser.spaces
                    , item =
                        [ "center", "mono" ]
                            |> List.map Parser.symbol
                            |> Parser.oneOf
                            |> Parser.getChompedString
                    }
                )
            |= mainParser
        , Parser.succeed (\pieces -> Paragraph { pieces = pieces, center = False, mono = False })
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
        [ Parser.succeed Text
            |. Parser.symbol "\\"
            |= Parser.getChompedString (Parser.chompIf <| \_ -> True)
        , Parser.succeed Speech
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
        , Parser.succeed parseSquareBrackets
            |. Parser.symbol "["
            |= Parser.getChompedString (Parser.chompWhile <| \c -> c /= ']')
            |. Parser.symbol "]"
        , Parser.succeed identity
            |. Parser.symbol "("
            |= Parser.oneOf
                [ Parser.succeed Slot
                    |= slotParser
                    |. Parser.symbol ")"
                , Parser.succeed (Text "(")
                ]
        , Parser.succeed identity
            |. Parser.symbol "K"
            |= Parser.oneOf
                [ Parser.succeed Kisses
                    |= Parser.getChompedString
                        (Parser.chompIf (\c -> Char.isDigit c || c == ' ')
                            |. Parser.chompWhile (\c -> Char.isDigit c || c == ',')
                        )
                , Parser.succeed (Text "K")
                ]
        , Parser.succeed identity
            |. Parser.symbol "{"
            |= Parser.oneOf
                [ Parser.succeed Colored
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
                , Parser.succeed Smol
                    |. Parser.symbol "smol "
                    |= innerParser '}'
                , Parser.succeed RewardPoints
                    |= Parser.getChompedString
                        (Parser.chompIf (\c -> Char.isDigit c || c == '-' || c == '+' || c == '/')
                            |. Parser.chompWhile (\c -> Char.isDigit c || c == '-' || c == '+' || c == '/')
                        )
                ]
            |. Parser.symbol "}"
        , Parser.succeed Text
            |= Parser.getChompedString
                (Parser.succeed ()
                    |. Parser.chompIf (\c -> not (Set.member c special))
                    |. Parser.chompWhile (\c -> not (Set.member c special))
                )
        ]
        |> many


slotParser : Parser Slot
slotParser =
    Parser.oneOf
        [ Parser.succeed Folk |. Parser.symbol "folk"
        , Parser.succeed Noble |. Parser.symbol "noble"
        , Parser.succeed Heroic |. Parser.symbol "heroic"
        , Parser.succeed Epic |. Parser.symbol "epic"
        , Parser.succeed White |. Parser.symbol "white"
        ]


parseSquareBrackets : String -> Piece
parseSquareBrackets str =
    case
        firstThatBuildsJust
            [ \input ->
                String.toInt input
                    |> Maybe.map (\_ -> Power input)
            , \input ->
                Types.affinityFromString input
                    |> Maybe.map Affinity
            , \input ->
                Types.classFromString input
                    |> Maybe.map Class
            , \input ->
                if String.startsWith "http" input then
                    Just (Link input)

                else
                    Nothing
            ]
            str
    of
        Just piece ->
            piece

        Nothing ->
            case ( str, String.toLower str ) of
                ( "K", _ ) ->
                    Kisses ""

                ( "W", _ ) ->
                    Warning

                ( "E", _ ) ->
                    Error

                ( "C", _ ) ->
                    Checkmark

                ( "-", _ ) ->
                    Power str

                ( "All", _ ) ->
                    Affinity Types.All

                ( "Jack-of-All", _ ) ->
                    JackOfAll

                ( _, "star" ) ->
                    Star

                ( _, "br" ) ->
                    LineBreak

                _ ->
                    if List.member str [ "OR", "/", "DESCRIPTION:", "LOCATION:", "RELATIONS:" ] then
                        Power str

                    else
                        Text ("[" ++ str ++ "]")


firstThatBuildsJust : List (a -> Maybe b) -> a -> Maybe b
firstThatBuildsJust list str =
    List.Extra.findMap (\f -> f str) list


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
    , 'K'
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
