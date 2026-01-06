module Parsers exposing (Affinity, Class, Companion, Complication, Content(..), DLC, DLCItem(..), Evil(..), Faction, GameMode, Magic, MagicAffinity(..), Perk, Quest, Race, Relic, Score(..), dlc, parseFiles)

import Ansi.Color
import Dict exposing (Dict)
import Dict.Extra
import Gen.CodeGen.Generate as Generate
import Generate.Enum exposing (Argument(..))
import Hex
import List.Extra
import List.Nonempty as Nonempty
import Maybe.Extra
import Parser exposing ((|.), (|=), Parser, andThen, backtrackable, getChompedString, int, keyword, map, oneOf, sequence, spaces, succeed, symbol)
import Parser.Error exposing (DeadEnd)
import Parser.Workaround exposing (chompUntilAfter, chompUntilEndOrAfter)
import Regex
import ResultME exposing (ResultME)
import Set exposing (Set)


type alias DLC =
    { name : Maybe String
    , author : Maybe String
    , link : Maybe String
    , items : List DLCItem
    }


type DLCItem
    = DLCAffinity Affinity
    | DLCClass Class
    | DLCGameMode GameMode
    | DLCRace Race
    | DLCCompanion Companion
    | DLCQuest Quest
    | DLCComplication Complication
    | DLCMagic Magic
    | DLCPerk Perk
    | DLCRelic Relic
    | DLCFaction Faction


parseFiles : List ( String, String, String ) -> ResultME Generate.Error (List DLC)
parseFiles inputs =
    let
        join : (DLC -> Maybe String) -> List DLC -> Maybe String
        join prop list =
            case List.filterMap prop list |> List.Extra.unique of
                [] ->
                    Nothing

                filtered ->
                    Just (String.join ", " filtered)
    in
    inputs
        |> ResultME.combineMap parseDLC
        |> Result.map
            (\dlcList ->
                dlcList
                    |> Dict.Extra.groupBy (\{ name } -> Maybe.withDefault "" name)
                    |> Dict.foldl
                        (\name grouped acc ->
                            { name =
                                if String.isEmpty name then
                                    Nothing

                                else
                                    Just name
                            , author = join .author grouped
                            , link = join .link grouped
                            , items = List.concatMap .items grouped
                            }
                                :: acc
                        )
                        []
            )


parseDLC : ( String, String, String ) -> ResultME Generate.Error DLC
parseDLC ( folder, filename, content ) =
    case Parser.run dlc content of
        Ok parsed ->
            Ok parsed

        Err deadEnds ->
            ResultME.error
                { title = "Error parsing DLC file"
                , description =
                    "Could not parse " ++ folder ++ "/" ++ filename ++ "\n\n" ++ errorToString content deadEnds
                }


errorToString : String -> List (DeadEnd {} Parser.Problem) -> String
errorToString src deadEnds =
    Parser.Error.renderError
        { text = identity
        , formatContext = Ansi.Color.fontColor Ansi.Color.cyan
        , formatCaret = Ansi.Color.fontColor Ansi.Color.red
        , newline = "\n"
        , linesOfExtraContext = 3
        }
        Parser.Error.forParser
        src
        deadEnds
        |> String.concat


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
        |= oneOf
            [ map Just (listItem "Author" Ok)
            , succeed Nothing
            ]
        |= oneOf
            [ map Just (listItem "Link" Ok)
            , succeed Nothing
            ]
        |= many
            (oneOf
                [ map DLCAffinity affinity
                , map DLCClass class
                , map DLCGameMode gameMode
                , map DLCCompanion companion
                , map DLCQuest quest
                , map DLCComplication complication
                , map DLCMagic magic
                , map DLCPerk perk
                , map DLCRace race
                , map DLCRelic relic
                , map DLCFaction faction
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
            { name : Maybe String
            , gain : Maybe String
            , cost : Int
            , description : String
            }
    }


race : Parser Race
race =
    (section "##" "Race" Race
        |> requiredItem "Elements" (stringListParser >> Result.map (List.Extra.remove "!"))
        |> requiredItem "Mana capacity" Ok
        |> requiredItem "Mana rate" Ok
        |> parseSection
    )
        |= paragraphs True
        |= oneOf
            [ backtrackable
                (section "###"
                    "Perk"
                    (\name gain cost description ->
                        Just
                            { name = Just name
                            , gain = gain
                            , cost = cost
                            , description = description
                            }
                    )
                    |> maybeItem "Gain" Ok
                    |> requiredItem "Cost" intParser
                    |> parseSection
                )
                |= paragraphs True
            , succeed
                (\gain cost description ->
                    Just
                        { name = Nothing
                        , gain = gain
                        , cost = cost
                        , description = description
                        }
                )
                |. sectionHeader "###" "Perk"
                |= oneOf
                    [ succeed Just
                        |= listItem "Gain" Ok
                    , succeed Nothing
                    ]
                |= listItem "Cost" intParser
                |= paragraphs True
            , succeed Nothing
            ]
        |. spaces


type alias Perk =
    { name : String
    , elements : List String
    , class : String
    , requires : Maybe String
    , isMeta : Bool
    , arguments : List Argument
    , content : Content ()
    }


type Content a
    = Single Int String
    | WithChoices a String (List ( String, Int )) String
    | WithCosts (List Int) String


perk : Parser Perk
perk =
    (section "##"
        "Perk"
        (\name element elements -> Perk name (elements |> Maybe.withDefault element))
        |> manyItems "Element" Ok
        |> maybeItem "Elements" stringListParser
        |> requiredItem "Class" Ok
        |> maybeItem "Requires" Ok
        |> flagItem "Meta"
        |> manyItems "Extra Argument" (ResultME.combineMap argument)
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
                |= many tierParser
                |= paragraphs False
            ]


argument : String -> ResultME String Argument
argument arg =
    case String.split " " arg of
        [ "List", v ] ->
            Ok (ListArgument v)

        [ v ] ->
            Ok (ValueArgument v)

        _ ->
            ResultME.error ("Unrecognized argument: " ++ arg)


type Section a
    = Section
        { key : String
        , name : String
        , parser : String -> Dict String (List String) -> ResultME String a
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


requiredItem : String -> (String -> ResultME String a) -> Section (a -> b) -> Section b
requiredItem key parser (Section i) =
    Section
        { key = i.key
        , name = i.name
        , parser =
            \n dict ->
                ResultME.map2 identity
                    (i.parser n dict)
                    (case Dict.get key dict of
                        Just [ value ] ->
                            parser value

                        Just [] ->
                            ResultME.error ("Missing required property: " ++ key)

                        Just (_ :: _ :: _) ->
                            ResultME.error ("Multiple values for property: " ++ key)

                        Nothing ->
                            ResultME.error ("Missing required property: " ++ key)
                    )
        , items = Set.insert key i.items
        }


optionalItem : String -> a -> (String -> ResultME String a) -> Section (a -> b) -> Section b
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
                            ResultME.error ("Multiple values for property: " ++ key)

                        Nothing ->
                            Ok default
                    )
        , items = Set.insert key i.items
        }


manyItems : String -> (List String -> ResultME String a) -> Section (a -> b) -> Section b
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


maybeItem : String -> (String -> ResultME String a) -> Section (Maybe a -> b) -> Section b
maybeItem key parser s =
    optionalItem key Nothing (\raw -> raw |> parser |> Result.map Just) s


flagItem : String -> Section (Bool -> b) -> Section b
flagItem key s =
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
    , faction : Maybe String
    , elements : MagicAffinity
    , hasRankZero : Bool
    , isElementalism : Bool
    , requires : Maybe String
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
        |> maybeItem "Faction" Ok
        |> optionalItem "Elements"
            (Regular [])
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
        |> flagItem "Has rank zero"
        |> flagItem "Elementalism"
        |> maybeItem "Requires" Ok
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


resultToParser : ResultME String a -> Parser a
resultToParser result =
    case result of
        Ok v ->
            succeed v

        Err e ->
            Parser.problem (e |> Nonempty.toList |> String.join ", ")


type alias Affinity =
    { name : String
    , color : Int
    , rainbow : Bool
    , selectable : Bool
    , symbol : Maybe String
    }


affinity : Parser Affinity
affinity =
    section "##" "Affinity" Affinity
        |> requiredItem "Color" hexParser
        |> flagItem "Rainbow"
        |> optionalItem "Selectable" True boolParser
        |> maybeItem "Symbol" Ok
        |> parseSection


type alias Relic =
    { name : String
    , classes : List String
    , requires : Maybe String
    , arguments : List Argument
    , content : Content Never
    }


relic : Parser Relic
relic =
    (section "##"
        "Relic"
        (\name classes requires extraArguments toContent content ->
            { name = name
            , classes = classes
            , requires = requires
            , arguments = extraArguments
            , content = toContent content
            }
        )
        |> oneOfItems
            [ requiredItem "Class" (\c -> Ok [ c ])
            , requiredItem "Classes" stringListParser
            , optionalItem nonexistentKey [] (\_ -> Ok [])
            ]
        |> maybeItem "Requires" Ok
        |> manyItems "Extra Argument" (ResultME.combineMap argument)
        |> oneOfItems
            [ requiredItem "Cost" (intParser >> Result.map Single)
            , requiredItem "Costs" (intListParser >> Result.map WithCosts)
            ]
        |> parseSection
    )
        |= paragraphs True


type alias Faction =
    { name : String
    , shortName : String
    , collectiveName : String
    , motto : String
    , isHuman : Bool
    , description : String
    , location : String
    , relations : String
    , perk : String
    , perkContent : String
    }


faction : Parser Faction
faction =
    Parser.succeed
        (\head description location relations factionPerk ->
            { name = head.name
            , motto = head.motto
            , shortName = head.shortName
            , collectiveName = head.collectiveName
            , isHuman = head.isHuman
            , description = description
            , location = location
            , relations = relations
            , perk = factionPerk.name
            , perkContent = factionPerk.description
            }
        )
        |= (section "##"
                "Faction"
                (\name motto shortName collectiveName isHuman ->
                    { name = name
                    , motto = motto
                    , shortName = shortName
                    , collectiveName = collectiveName
                    , isHuman = isHuman
                    }
                )
                |> requiredItem "Motto" Ok
                |> requiredItem "Short name" Ok
                |> requiredItem "Collective name" Ok
                |> optionalItem "Human" False boolParser
                |> parseSection
           )
        |= paragraphs True
        |. sectionHeader "###" "Location"
        |= paragraphs True
        |. sectionHeader "###" "Relations"
        |= paragraphs True
        |= ((section "###"
                "Perk"
                (\name description ->
                    { name = name
                    , description = description
                    }
                )
                |> parseSection
            )
                |= paragraphs True
           )


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
                            ResultME.error ("Invalid score: " ++ v)
                )
    in
    (section "##" "Companion" Companion
        |> maybeItem "Full name" Ok
        |> maybeItem "Faction" Ok
        |> maybeItem "Class" Ok
        |> oneOfItems
            [ requiredItem "Race" (\r -> Ok [ r ])
            , requiredItem "Races" stringListParser
            , optionalItem nonexistentKey [] (\_ -> Ok [])
            ]
        |> flagItem "Has Perk"
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


type alias Quest =
    { name : String
    , slot : String
    , evil : Evil
    , repeatable : Bool
    , threat : Maybe Int
    , conflict : Maybe Int
    , reward : Int
    , requires : Maybe String
    , faction : Maybe String
    , description : String
    , notes : List String
    , sidebars : List String
    }


type Evil
    = EvilYes
    | EvilMaybe
    | EvilNo


quest : Parser Quest
quest =
    let
        evilFlagParser : Evil -> String -> ResultME String Evil
        evilFlagParser evil input =
            input
                |> boolParser
                |> Result.andThen
                    (\f ->
                        if f then
                            Ok evil

                        else
                            ResultME.error "Unexpected value: False"
                    )

        maybeIntParser : String -> ResultME String (Maybe Int)
        maybeIntParser s =
            if s == "?" then
                Ok Nothing

            else
                intParser s
                    |> Result.map Just
    in
    succeed
        (\ctor description sidebars ->
            case String.split "Notes:" description of
                [ before, after ] ->
                    ctor before
                        (after
                            |> String.split "\n"
                            |> List.map
                                (\s ->
                                    s
                                        |> String.trim
                                        |> String.dropLeft 1
                                )
                            |> List.Extra.removeWhen String.isEmpty
                        )
                        sidebars

                _ ->
                    ctor description [] sidebars
        )
        |= (section "##" "Quest" Quest
                |> requiredItem "Slot" Ok
                |> oneOfItems
                    [ requiredItem "Evil Route" (evilFlagParser EvilMaybe)
                    , requiredItem "Evil" (evilFlagParser EvilYes)
                    , optionalItem nonexistentKey EvilNo (\_ -> Ok EvilNo)
                    ]
                |> optionalItem "Repeatable" False boolParser
                |> requiredItem "Threat" maybeIntParser
                |> requiredItem "Conflict" maybeIntParser
                |> requiredItem "Reward" intParser
                |> maybeItem "Requires" Ok
                |> maybeItem "Faction" Ok
                |> parseSection
           )
        |= paragraphs True
        |= many
            (Parser.succeed identity
                |. sectionHeader "###" "Sidebar"
                |= paragraphs True
            )


type alias Complication =
    { name : String
    , class : Maybe String
    , isTiered : Bool
    , category : Maybe String
    , content : Content ()
    }


complication : Parser Complication
complication =
    (section "##" "Complication" Complication
        |> maybeItem "Class" Ok
        |> flagItem "Tiered"
        |> maybeItem "Category" Ok
        |> parseSection
    )
        |= oneOf
            [ succeed Single
                |= listItem "Gain" intParser
                |= paragraphs True
            , succeed WithCosts
                |= listItem "Gains" intListParser
                |= paragraphs True
            , succeed (WithChoices ())
                |= paragraphs False
                |= many tierParser
                |= paragraphs False
            ]


type alias Class =
    { name : String
    , color : Int
    , description : String
    }


class : Parser Class
class =
    (section "##" "Class" Class
        |> requiredItem "Color" hexParser
        |> parseSection
    )
        |= paragraphs True
        |. spaces


type alias GameMode =
    { name : String
    , description : String
    }


gameMode : Parser GameMode
gameMode =
    (section "##" "Game Mode" GameMode
        |> parseSection
    )
        |= paragraphs True
        |. spaces



-- Generic parsers --


sectionHeader : String -> String -> Parser ()
sectionHeader level name =
    symbol level
        |. spaces
        |. keyword name
        |. spaces


nonexistentKey : String
nonexistentKey =
    "!@#$%^&*()"


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


boolParser : String -> ResultME String Bool
boolParser raw =
    case raw of
        "True" ->
            Ok True

        "False" ->
            Ok False

        _ ->
            ResultME.error (raw ++ " is not a valid boolean")


hexParser : String -> ResultME String Int
hexParser raw =
    Hex.fromString (String.toLower raw)
        |> Result.mapError (\e -> raw ++ " is not a valid hex number: " ++ e)
        |> ResultME.fromResult


intParser : String -> ResultME String Int
intParser raw =
    case String.toInt raw of
        Nothing ->
            ResultME.error (raw ++ " is not a valid number")

        Just n ->
            Ok n


intListParser : String -> ResultME String (List Int)
intListParser raw =
    case
        raw
            |> String.split ","
            |> Maybe.Extra.combineMap
                (\piece ->
                    case String.toInt (String.trim piece) of
                        Just p ->
                            Just [ p ]

                        Nothing ->
                            let
                                pieces : Maybe (List Int)
                                pieces =
                                    piece
                                        |> String.trim
                                        |> String.split "-"
                                        |> Maybe.Extra.combineMap String.toInt
                            in
                            case pieces of
                                Just [] ->
                                    Nothing

                                Just [ n ] ->
                                    Just [ n ]

                                Just [ l, h ] ->
                                    Just (List.range l h)

                                Just [ l, m, h ] ->
                                    let
                                        step : Int
                                        step =
                                            m - l
                                    in
                                    List.range 0 ((h - l) // step)
                                        |> List.map (\i -> i * step + l)
                                        |> Just

                                _ ->
                                    Nothing
                )
    of
        Nothing ->
            ResultME.error (raw ++ " is not a valid list of numbers")

        Just n ->
            Ok (List.sort (List.concat n))


stringListParser : String -> ResultME String (List String)
stringListParser raw =
    raw
        |> String.split ","
        |> List.map String.trim
        |> Ok


listItem : String -> (String -> ResultME String a) -> Parser a
listItem key continuation =
    succeed String.trim
        |. backtrackable
            (symbol "-"
                |. spaces
                |. keyword key
                |. symbol ":"
            )
        |. spaces
        |= getChompedString (chompUntilAfter "\n")
        |. spaces
        |> andThen
            (\raw ->
                continuation raw
                    |> resultToParser
            )


oneOfItems : List (Section a -> Section b) -> Section a -> Section b
oneOfItems options (Section s) =
    let
        mapped :
            List
                { key : String
                , name : String
                , parser : String -> Dict String (List String) -> ResultME String b
                , items : Set String
                }
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
                                        Err (Nonempty.toList ee ++ eacc)
                    )
                    (Err [])
                    mapped
                    |> Result.mapError (\es -> "Expected one of:" ++ String.join "\n" es)
                    |> ResultME.fromResult
        }


tierParser : Parser ( String, Int )
tierParser =
    succeed (\c d -> ( String.trim d, c ))
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
