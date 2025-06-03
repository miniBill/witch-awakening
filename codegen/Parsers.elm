module Parsers exposing (Content(..), DLC, DLCItem(..), Magic, Perk, Race, dlc)

import Dict exposing (Dict)
import Maybe.Extra
import Parser exposing ((|.), (|=), Parser, andThen, backtrackable, getChompedString, int, keyword, map, oneOf, problem, sequence, spaces, succeed, symbol)
import Parser.Workaround exposing (chompUntilAfter, chompUntilEndOrAfter)


type alias DLC =
    { name : Maybe String
    , items : List DLCItem
    }


type DLCItem
    = DLCRace Race
    | DLCPerk Perk
    | DLCMagic Magic


dlc : Parser DLC
dlc =
    succeed DLC
        |. keyword "#"
        |= (chompUntilAfter "\n"
                |> getChompedString
                |> map
                    (\rawName ->
                        case String.trim rawName of
                            "Core" ->
                                Nothing

                            trimmed ->
                                Just trimmed
                    )
           )
        |. spaces
        |= many
            (oneOf
                [ map DLCRace race
                , map DLCPerk perk
                , map DLCMagic magic
                ]
            )
        |. spaces
        |. Parser.end



-- Item parsers --


type alias Race =
    { name : String
    , elements : List String
    , manaCapacity : String
    , manaRate : String
    , description : String
    , perk :
        Maybe
            { cost : Int
            , description : String
            }
    }


race : Parser Race
race =
    succeed Race
        |= header "##" "Race"
        |= listItem "Elements"
            (\raw ->
                raw
                    |> String.split ","
                    |> List.map String.trim
                    |> succeed
            )
        |= listItem "Mana capacity" succeed
        |= listItem "Mana rate" succeed
        |= paragraphs True
        |= oneOf
            [ succeed
                (\cost description ->
                    Just
                        { cost = cost
                        , description = description
                        }
                )
                |. symbol "###"
                |. spaces
                |. keyword "Perk"
                |. spaces
                |= listItem "Cost" intParser
                |= paragraphs True
            , succeed Nothing
            ]
        |. spaces


type alias Perk =
    { name : String
    , element : String
    , class : String
    , isMeta : Bool
    , content : Content
    }


type Content
    = Single Int String
    | WithChoices String (List ( String, Int )) String
    | WithCosts String (List Int)


perk : Parser Perk
perk =
    succeed Perk
        |= header "##" "Perk"
        |= listItem "Element" succeed
        |= listItem "Class" succeed
        |= Parser.oneOf
            [ listItem "Meta" boolParser
            , succeed False
            ]
        |= Parser.oneOf
            [ succeed Single
                |= listItem "Cost" intParser
                |= paragraphs True
            , succeed (\c d -> WithCosts d c)
                |= listItem "Costs" intListParser
                |= paragraphs True
            , succeed WithChoices
                |= paragraphs False
                |= many
                    (succeed (\c d -> ( String.trim d, c ))
                        |. symbol "-"
                        |. spaces
                        |. symbol "["
                        |. spaces
                        |= oneOf
                            [ succeed negate
                                |. symbol "-"
                                |. spaces
                                |= int
                            , int
                            ]
                        |. spaces
                        |. symbol "]"
                        |. spaces
                        |= getChompedString (chompUntilAfter "\n")
                    )
                |= paragraphs False
            ]


type alias Magic =
    { name : String
    , class : Maybe String
    , elements : List String
    , star : Bool
    , isElementalism : Bool
    , description : String
    , ranks : Dict Int String
    }


magic : Parser Magic
magic =
    succeed Magic
        |= header "##" "Magic"
        |= oneOf
            [ listItem "Class" (\class -> succeed (Just class))
            , succeed Nothing
            ]
        |= listItem "Elements"
            (\raw ->
                raw
                    |> String.split ","
                    |> List.map String.trim
                    |> succeed
            )
        |= Parser.oneOf
            [ listItem "Star" boolParser
            , succeed False
            ]
        |= Parser.oneOf
            [ listItem "Elementalism" boolParser
            , succeed False
            ]
        |= paragraphs True
        |= (many
                (succeed Tuple.pair
                    |= (succeed String.trim
                            |. backtrackable
                                (keyword "###"
                                    |. spaces
                                )
                            |. keyword "Rank"
                            |. spaces
                            |= getChompedString (chompUntilAfter "\n")
                            |. spaces
                            |> andThen intParser
                       )
                    |= paragraphs True
                )
                |> map Dict.fromList
           )



-- Generic parsers --


many : Parser a -> Parser (List a)
many inner =
    sequence
        { start = ""
        , end = ""
        , separator = ""
        , spaces = succeed ()
        , trailing = Parser.Optional
        , item = inner
        }


paragraphs : Bool -> Parser String
paragraphs acceptList =
    many (paragraph acceptList)
        |> Parser.getChompedString
        |> Parser.map String.trim


paragraph : Bool -> Parser ()
paragraph acceptList =
    Parser.chompIf (\c -> (acceptList || c /= '-') && c /= '#')
        |. chompUntilEndOrAfter "\n"
        |. Parser.spaces


boolParser : String -> Parser Bool
boolParser raw =
    case raw of
        "True" ->
            succeed True

        "False" ->
            succeed False

        _ ->
            problem (raw ++ " is not a valid boolean")


intParser : String -> Parser Int
intParser raw =
    case String.toInt raw of
        Nothing ->
            problem (raw ++ " is not a valid number")

        Just n ->
            succeed n


intListParser : String -> Parser (List Int)
intListParser raw =
    case raw |> String.split "," |> Maybe.Extra.combineMap (\piece -> piece |> String.trim |> String.toInt) of
        Nothing ->
            problem (raw ++ " is not a valid list of numbers")

        Just n ->
            succeed n


header : String -> String -> Parser String
header level key =
    succeed String.trim
        |. backtrackable
            (keyword level
                |. spaces
            )
        |. keyword key
        |. spaces
        |. symbol ":"
        |. spaces
        |= getChompedString (chompUntilAfter "\n")
        |. spaces


listItem : String -> (String -> Parser a) -> Parser a
listItem key continuation =
    succeed String.trim
        |. backtrackable
            (symbol "-"
                |. spaces
            )
        |. keyword key
        |. symbol ":"
        |. spaces
        |= getChompedString (chompUntilAfter "\n")
        |. spaces
        |> andThen continuation
