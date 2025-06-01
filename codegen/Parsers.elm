module Parsers exposing (DLC, DLCItem(..), Perk, Race, dlc)

import Parser exposing ((|.), (|=), Parser, andThen, backtrackable, commit, getChompedString, keyword, map, oneOf, sequence, spaces, succeed, symbol)
import Parser.Workaround exposing (chompUntilAfter, chompUntilEndOrAfter)


type alias DLC =
    { name : String
    , items : List DLCItem
    }


type DLCItem
    = DLCRace Race
    | DLCPerk Perk


dlc : Parser DLC
dlc =
    succeed DLC
        |. keyword "#"
        |= (chompUntilAfter "\n"
                |> getChompedString
                |> map String.trim
           )
        |. spaces
        |= sequence
            { start = ""
            , end = ""
            , item =
                oneOf
                    [ map DLCRace race
                    , map DLCPerk perk
                    ]
            , separator = ""
            , spaces = spaces
            , trailing = Parser.Optional
            }



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
        |= (listItem "Elements"
                |> map
                    (\raw ->
                        raw
                            |> String.split ","
                            |> List.map String.trim
                    )
           )
        |= listItem "Mana capacity"
        |= listItem "Mana rate"
        |= paragraphs
        |= oneOf
            [ succeed
                (\cost description ->
                    Just
                        { cost = cost
                        , description = description
                        }
                )
                |. header "###" "Perk"
                |= (listItem "Cost" |> andThen intParser)
                |= paragraphs
            , succeed Nothing
            ]


paragraphs : Parser String
paragraphs =
    Parser.sequence
        { start = ""
        , end = ""
        , item = paragraph
        , trailing = Parser.Optional
        , spaces = Parser.succeed ()
        , separator = ""
        }
        |> Parser.getChompedString
        |> Parser.map String.trim


paragraph : Parser a
paragraph =
    Parser.chompIf (\c -> c /= '-' && c /= '#')
        |. chompUntilEndOrAfter "\n"
        |. Parser.spaces


type alias Perk =
    { name : String
    , element : String
    , class : String
    , cost : Int
    , description : String
    }


perk : Parser Perk
perk =
    succeed Perk
        |= header "##" "Perk"
        |= listItem "Element"
        |= listItem "Class"
        |= (listItem "Cost" |> andThen intParser)
        |= paragraphs


intParser : String -> Parser Int
intParser raw =
    case String.toInt raw of
        Nothing ->
            Parser.problem (raw ++ " is not a valid number")

        Just n ->
            Parser.succeed n



-- Utilities --


header : String -> String -> Parser String
header level key =
    succeed String.trim
        |. backtrackable (keyword level)
        |. spaces
        |. keyword key
        |. commit ()
        |. spaces
        |. symbol ":"
        |. spaces
        |= getChompedString (chompUntilAfter "\n")
        |. spaces


listItem : String -> Parser String
listItem key =
    succeed String.trim
        |. backtrackable
            (symbol "-"
                |. spaces
            )
        |. keyword key
        |. commit ()
        |. symbol ":"
        |. spaces
        |= getChompedString (chompUntilAfter "\n")
        |. spaces
