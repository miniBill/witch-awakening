module Parsers exposing (Affinity, Companion, Content(..), DLC, DLCItem(..), Magic, MagicAffinity(..), Perk, Race, Relic, Score(..), dlc)

import Dict exposing (Dict)
import Dict.Extra
import Hex
import Maybe.Extra
import Parser exposing ((|.), (|=), Parser, andThen, backtrackable, getChompedString, int, keyword, map, oneOf, sequence, spaces, succeed, symbol)
import Parser.Workaround exposing (chompUntilAfter, chompUntilEndOrAfter)
import Regex
import Set exposing (Set)


type alias DLC =
    { name : Maybe String
    , items : List DLCItem
    }


type DLCItem
    = DLCRace Race
    | DLCPerk Perk
    | DLCMagic Magic
    | DLCAffinity Affinity
    | DLCRelic Relic
    | DLCCompanion Companion


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
                , map DLCAffinity affinity
                , map DLCRelic relic
                , map DLCCompanion companion
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
    (section "##" "Race" Race
        |> requiredItem "Elements"
            (\raw ->
                raw
                    |> String.split ","
                    |> List.map String.trim
                    |> Ok
            )
        |> requiredItem "Mana capacity" Ok
        |> requiredItem "Mana rate" Ok
        |> parseSection
    )
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
    , content : Content ()
    }


type Content a
    = Single Int String
    | WithChoices a String (List ( String, Int )) String
    | WithCosts (List Int) String


perk : Parser Perk
perk =
    (section "##" "Perk" Perk
        |> requiredItem "Element" Ok
        |> requiredItem "Class" Ok
        |> flag "Meta"
        |> parseSection
    )
        |= oneOf
            [ succeed Single
                |= listItem "Cost" intParser
                |= paragraphs True
            , succeed WithCosts
                |= listItem "Costs" intListParser
                |= paragraphs True
            , succeed (WithChoices ())
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


type Section a
    = Section
        { key : String
        , name : String
        , parser : String -> Dict String (List String) -> Result String a
        , items : Set String
        }


section : String -> String -> (String -> ctor) -> Section ctor
section key name ctor =
    Section
        { key = key
        , name = name
        , parser = \n _ -> Ok (ctor n)
        , items = Set.empty
        }


requiredItem : String -> (String -> Result String a) -> Section (a -> b) -> Section b
requiredItem key parser (Section i) =
    Section
        { key = i.key
        , name = i.name
        , parser =
            \n dict ->
                Result.map2 identity
                    (i.parser n dict)
                    (case Dict.get key dict of
                        Just [ value ] ->
                            parser value

                        Just [] ->
                            Err ("Missing required property: " ++ key)

                        Just (_ :: _ :: _) ->
                            Err ("Multiple values for property: " ++ key)

                        Nothing ->
                            Err ("Missing required property: " ++ key)
                    )
        , items = Set.insert key i.items
        }


optionalItem : String -> a -> (String -> Result String a) -> Section (a -> b) -> Section b
optionalItem key default parser (Section i) =
    Section
        { key = i.key
        , name = i.name
        , parser =
            \n dict ->
                Result.map2 identity
                    (i.parser n dict)
                    (case Dict.get key dict of
                        Just [ value ] ->
                            parser value

                        Just [] ->
                            Ok default

                        Just (_ :: _ :: _) ->
                            Err ("Multiple values for property: " ++ key)

                        Nothing ->
                            Ok default
                    )
        , items = Set.insert key i.items
        }


manyItems : String -> (List String -> Result String a) -> Section (a -> b) -> Section b
manyItems key parser (Section i) =
    Section
        { key = i.key
        , name = i.name
        , parser =
            \n dict ->
                Result.map2 identity
                    (i.parser n dict)
                    (Dict.get key dict
                        |> Maybe.withDefault []
                        |> parser
                    )
        , items = Set.insert key i.items
        }


maybeItem : String -> (String -> Result String a) -> Section (Maybe a -> b) -> Section b
maybeItem key parser s =
    optionalItem key Nothing (\raw -> raw |> parser |> Result.map Just) s


flag : String -> Section (Bool -> b) -> Section b
flag key s =
    optionalItem key False boolParser s


parseSection : Section a -> Parser a
parseSection (Section i) =
    succeed Tuple.pair
        |. backtrackable
            (keyword i.key
                |. spaces
            )
        |. keyword i.name
        |. spaces
        |. symbol ":"
        |. spaces
        |= getChompedString (chompUntilAfter "\n")
        |. spaces
        |= many
            (i.items
                |> Set.toList
                |> List.sortBy (\j -> -(String.length j))
                |> List.map
                    (\key ->
                        listItem key (\value -> Ok ( key, value ))
                    )
                |> oneOf
            )
        |> andThen
            (\( n, k ) ->
                i.parser (String.trim n)
                    (Dict.Extra.groupBy Tuple.first k
                        |> Dict.map
                            (\_ v ->
                                List.map Tuple.second v
                            )
                    )
                    |> resultToParser
            )


type alias Magic =
    { name : String
    , class : Maybe String
    , elements : MagicAffinity
    , hasRankZero : Bool
    , isElementalism : Bool
    , description : String
    , ranks : Dict Int String
    }


type MagicAffinity
    = Regular (List String)
    | Alternative (List (List String))


magic : Parser Magic
magic =
    (section "##" "Magic" Magic
        |> maybeItem "Class" Ok
        |> requiredItem "Elements"
            (\raw ->
                let
                    splat : List (List String)
                    splat =
                        raw
                            |> String.split ","
                            |> List.map String.trim
                            |> List.map
                                (\alternative ->
                                    alternative
                                        |> String.split "+"
                                        |> List.map String.trim
                                )

                    asSingleton : List a -> Maybe a
                    asSingleton a =
                        case a of
                            [ x ] ->
                                Just x

                            _ ->
                                Nothing
                in
                case Maybe.Extra.combineMap asSingleton splat of
                    Just v ->
                        Ok (Regular v)

                    Nothing ->
                        Ok (Alternative splat)
            )
        |> flag "Has rank zero"
        |> flag "Elementalism"
        |> parseSection
    )
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
                            |> andThen
                                (\raw ->
                                    intParser raw
                                        |> resultToParser
                                )
                       )
                    |= paragraphs True
                )
                |> map Dict.fromList
           )


resultToParser : Result String a -> Parser a
resultToParser result =
    case result of
        Ok v ->
            succeed v

        Err e ->
            Parser.problem e


type alias Affinity =
    { name : String
    , color : Int
    , rainbow : Bool
    , symbol : Maybe String
    }


affinity : Parser Affinity
affinity =
    section "##" "Affinity" Affinity
        |> requiredItem "Color" hexParser
        |> flag "Rainbow"
        |> maybeItem "Symbol" Ok
        |> parseSection


type alias Relic =
    { name : String
    , class : Maybe String
    , content : Content Never
    }


relic : Parser Relic
relic =
    (section "##"
        "Relic"
        (\name class toContent content ->
            { name = name
            , class = class
            , content = toContent content
            }
        )
        |> maybeItem "Class" Ok
        |> oneOfItems
            [ requiredItem "Cost" (intParser >> Result.map Single)
            , requiredItem "Costs" (intListParser >> Result.map WithCosts)
            ]
        |> parseSection
    )
        |= paragraphs True


oneOfItems : List (Section a -> Section b) -> Section a -> Section b
oneOfItems options (Section s) =
    let
        mapped =
            options
                |> List.map
                    (\option ->
                        let
                            (Section r) =
                                option (Section s)
                        in
                        r
                    )
    in
    Section
        { key = s.key
        , name = s.name
        , items = List.foldl (\e acc -> Set.union e.items acc) Set.empty mapped
        , parser =
            \n d ->
                List.foldl
                    (\e acc ->
                        case acc of
                            Ok v ->
                                Ok v

                            Err eacc ->
                                case e.parser n d of
                                    Ok v ->
                                        Ok v

                                    Err ee ->
                                        Err (ee :: eacc)
                    )
                    (Err [])
                    mapped
                    |> Result.mapError (\es -> "Expected one of:" ++ String.join "\n" es)
        }


type alias Companion =
    { name : String
    , fullName : Maybe String
    , faction : Maybe String
    , class : Maybe String
    , races : List String
    , hasPerk : Bool
    , cost : Maybe Int
    , power : Score
    , teamwork : Score
    , sociability : Score
    , morality : Score
    , positives : List String
    , negatives : List String
    , mixed : List String
    , has : String
    , quote : String
    , description : String
    }


type Score
    = NormalScore Int
    | SpecialEffect { worse : Maybe Int, better : Int }


companion : Parser Companion
companion =
    let
        score : String -> Section (Score -> a) -> Section a
        score label =
            requiredItem label
                (\v ->
                    case
                        v
                            |> String.split "-"
                            |> Maybe.Extra.combineMap String.toInt
                    of
                        Just [ s ] ->
                            Ok (NormalScore s)

                        Just [ 1, better ] ->
                            Ok (SpecialEffect { worse = Nothing, better = better })

                        Just [ worse, better ] ->
                            Ok (SpecialEffect { worse = Just worse, better = better })

                        _ ->
                            Err ("Invalid score: " ++ v)
                )
    in
    (section "##" "Companion" Companion
        |> maybeItem "Full name" Ok
        |> maybeItem "Faction" Ok
        |> maybeItem "Class" Ok
        |> oneOfItems
            [ requiredItem "Race" (\r -> Ok [ r ])
            , requiredItem "Races" (\r -> Ok (List.map String.trim (String.split "," r)))
            , optionalItem "___" [] (\_ -> Ok [])
            ]
        |> flag "Has Perk"
        |> maybeItem "Cost" intParser
        |> score "Power"
        |> score "Teamwork"
        |> score "Sociability"
        |> score "Morality"
        |> manyItems "Positive" Ok
        |> manyItems "Negative" Ok
        |> manyItems "Mixed" Ok
        |> optionalItem "Has" "" Ok
        |> requiredItem "Quote" Ok
        |> parseSection
    )
        |= paragraphs True



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
        |> Parser.map
            (\chomped ->
                chomped
                    |> String.trim
                    |> Regex.replace commentRegex (\_ -> "")
            )


commentRegex : Regex.Regex
commentRegex =
    Regex.fromString "<!--.*-->"
        |> Maybe.withDefault Regex.never


paragraph : Bool -> Parser ()
paragraph acceptList =
    Parser.chompIf (\c -> (acceptList || c /= '-') && c /= '#')
        |. chompUntilEndOrAfter "\n"
        |. Parser.spaces


boolParser : String -> Result String Bool
boolParser raw =
    case raw of
        "True" ->
            Ok True

        "False" ->
            Ok False

        _ ->
            Err (raw ++ " is not a valid boolean")


hexParser : String -> Result String Int
hexParser raw =
    Hex.fromString (String.toLower raw)
        |> Result.mapError (\e -> raw ++ " is not a valid hex number: " ++ e)


intParser : String -> Result String Int
intParser raw =
    case String.toInt raw of
        Nothing ->
            Err (raw ++ " is not a valid number")

        Just n ->
            Ok n


intListParser : String -> Result String (List Int)
intListParser raw =
    case raw |> String.split "," |> Maybe.Extra.combineMap (\piece -> piece |> String.trim |> String.toInt) of
        Nothing ->
            Err (raw ++ " is not a valid list of numbers")

        Just n ->
            Ok n


listItem : String -> (String -> Result String a) -> Parser a
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
        |> andThen
            (\raw ->
                continuation raw
                    |> resultToParser
            )
