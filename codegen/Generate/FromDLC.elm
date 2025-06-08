module Generate.FromDLC exposing (files)

import Dict
import Dict.Extra
import Elm
import Elm.Annotation
import Elm.Arg
import Elm.Case
import Elm.Declare
import Elm.Op
import Gen.CodeGen.Generate as Generate
import Gen.Data.Affinity
import Gen.Data.Companion
import Gen.Data.Complication
import Gen.Data.Magic
import Gen.Data.Perk
import Gen.Data.Race
import Gen.Types
import Generate.Classes
import Generate.Relics
import Generate.TypePerks
import Generate.Types
import Generate.Utils exposing (valueFromTypes, yassify)
import List.Extra
import Parser
import Parsers exposing (DLCItem(..))
import Result.Extra
import String.Extra


files : List ( String, String, String ) -> Result (List Generate.Error) (List Elm.File)
files inputs =
    inputs
        |> Result.Extra.combineMap parseDLC
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
                            , items = List.concatMap .items grouped
                            }
                                :: acc
                        )
                        []
                    |> files_
            )


parseDLC : ( String, String, String ) -> Result (List Generate.Error) Parsers.DLC
parseDLC ( folder, filename, content ) =
    Parser.run Parsers.dlc content
        |> Result.mapError
            (\deadEnds ->
                [ { title = "Error parsing DLC file"
                  , description =
                        "Could not parse " ++ folder ++ "/" ++ filename ++ "\n" ++ errorToString deadEnds
                  }
                ]
            )


errorToString : List Parser.DeadEnd -> String
errorToString deadEnds =
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


files_ : List Parsers.DLC -> List Elm.File
files_ dlcList =
    let
        { dlcAffinities, dlcClasses, dlcCompanions, dlcComplications, dlcMagics, dlcPerks, dlcRaces, dlcRelics } =
            List.foldr
                (\( dlcName, item ) acc ->
                    case item of
                        Parsers.DLCAffinity affinity ->
                            { acc | dlcAffinities = ( dlcName, affinity ) :: acc.dlcAffinities }

                        Parsers.DLCClass class ->
                            { acc | dlcClasses = ( dlcName, class ) :: acc.dlcClasses }

                        Parsers.DLCCompanion companion ->
                            { acc | dlcCompanions = ( dlcName, companion ) :: acc.dlcCompanions }

                        Parsers.DLCComplication complication ->
                            { acc | dlcComplications = ( dlcName, complication ) :: acc.dlcComplications }

                        Parsers.DLCMagic magic ->
                            { acc | dlcMagics = ( dlcName, magic ) :: acc.dlcMagics }

                        Parsers.DLCPerk perk ->
                            { acc | dlcPerks = ( dlcName, perk ) :: acc.dlcPerks }

                        Parsers.DLCRace race ->
                            { acc | dlcRaces = ( dlcName, race ) :: acc.dlcRaces }

                        Parsers.DLCRelic relic ->
                            { acc | dlcRelics = ( dlcName, relic ) :: acc.dlcRelics }
                )
                { dlcAffinities = []
                , dlcClasses = []
                , dlcCompanions = []
                , dlcComplications = []
                , dlcMagics = []
                , dlcPerks = []
                , dlcRaces = []
                , dlcRelics = []
                }
                (List.concatMap (\dlc -> List.map (Tuple.pair dlc.name) dlc.items) dlcList)
    in
    [ affinitiesFile dlcAffinities
    , Elm.Declare.toFile (Generate.Classes.module_ dlcClasses)
    , companionsFile dlcCompanions
    , complicationsFile dlcComplications
    , magicsFile dlcMagics
    , perksFile dlcPerks
    , racesFile dlcRaces
    , Elm.Declare.toFile (Generate.Relics.relicsFile dlcRelics)
    , Elm.Declare.toFile (Generate.TypePerks.typePerksFile dlcRaces)
    , Elm.Declare.toFile (Generate.Types.module_ dlcList)
    ]


racesFile : List ( Maybe String, Parsers.Race ) -> Elm.File
racesFile dlcRaces =
    Elm.file [ "Generated", "Race" ]
        (Elm.expose (Elm.declaration "all" (allRaces dlcRaces))
            :: dlcToRaces dlcRaces
        )


allRaces : List ( Maybe String, Parsers.Race ) -> Elm.Expression
allRaces dlcRaces =
    Elm.fn
        (Elm.Arg.varWith "races"
            (Elm.Annotation.list (Elm.Annotation.named [ "Generated", "Types" ] "Race"))
        )
    <|
        \races ->
            Elm.Op.append
                (dlcRaces
                    |> List.map (\( _, race ) -> Elm.val (String.Extra.decapitalize race.name))
                    |> Elm.list
                )
                (Gen.Data.Race.call_.all races)
                |> Elm.withType (Elm.Annotation.list Gen.Data.Race.annotation_.details)


dlcToRaces : List ( Maybe String, Parsers.Race ) -> List Elm.Declaration
dlcToRaces races =
    List.map
        (\( dlcName, race ) ->
            Gen.Data.Race.make_.details
                { name = valueFromTypes race.name
                , content = Elm.string race.description
                , tank = valueFromTypes race.manaCapacity
                , affinities = Elm.list (List.map valueFromTypes race.elements)
                , charge = valueFromTypes race.manaRate
                , dlc = Elm.maybe (Maybe.map Elm.string dlcName)
                }
                |> Elm.declaration (yassify race.name)
                |> Elm.expose
        )
        races


perksFile : List ( Maybe String, Parsers.Perk ) -> Elm.File
perksFile dlcPerks =
    Elm.file [ "Generated", "Perk" ]
        (Elm.expose (Elm.declaration "all" (allPerks dlcPerks))
            :: dlcToPerks dlcPerks
        )


allPerks : List ( Maybe String, Parsers.Perk ) -> Elm.Expression
allPerks dlcPerks =
    Elm.fn
        (Elm.Arg.varWith "perks"
            (Elm.Annotation.list Gen.Types.annotation_.rankedPerk)
        )
    <|
        \perks ->
            Elm.Op.append
                (Gen.Data.Perk.call_.all perks)
                (dlcPerks
                    |> List.map (\( _, perk ) -> Elm.val (String.Extra.decapitalize (yassify perk.name)))
                    |> Elm.list
                )
                |> Elm.withType (Elm.Annotation.list Gen.Data.Perk.annotation_.details)


dlcToPerks : List ( Maybe String, Parsers.Perk ) -> List Elm.Declaration
dlcToPerks perks =
    List.map
        (\( dlcName, perk ) ->
            Gen.Data.Perk.make_.details
                { name = valueFromTypes perk.name
                , class = valueFromTypes perk.class
                , affinity = valueFromTypes perk.element
                , isMeta = Elm.bool perk.isMeta
                , content =
                    case perk.content of
                        Parsers.Single cost description ->
                            Gen.Data.Perk.make_.single (Elm.int cost) (Elm.string description)

                        Parsers.WithCosts costs description ->
                            Gen.Data.Perk.make_.withCosts (Elm.list (List.map Elm.int costs)) (Elm.string description)

                        Parsers.WithChoices () before choices after ->
                            Gen.Data.Perk.make_.withChoices
                                (Elm.string before)
                                (choices
                                    |> List.map
                                        (\( choice, cost ) -> Elm.tuple (Elm.string choice) (Elm.int cost))
                                    |> Elm.list
                                )
                                (Elm.string after)
                , dlc = Elm.maybe (Maybe.map Elm.string dlcName)
                }
                |> Elm.declaration (yassify perk.name)
                |> Elm.expose
        )
        perks


magicsFile : List ( Maybe String, Parsers.Magic ) -> Elm.File
magicsFile dlcMagics =
    Elm.file [ "Generated", "Magic" ]
        (Elm.expose (Elm.declaration "all" (allMagics dlcMagics))
            :: dlcToMagics dlcMagics
        )


allMagics : List ( Maybe String, Parsers.Magic ) -> Elm.Expression
allMagics dlcMagics =
    dlcMagics
        |> List.map (\( _, magic ) -> Elm.val (String.Extra.decapitalize (yassify magic.name)))
        |> Elm.list
        |> Elm.withType (Elm.Annotation.list Gen.Data.Magic.annotation_.details)


dlcToMagics : List ( Maybe String, Parsers.Magic ) -> List Elm.Declaration
dlcToMagics magics =
    List.map
        (\( dlcName, magic ) ->
            let
                maxRank : Int
                maxRank =
                    magic.ranks
                        |> Dict.keys
                        |> List.maximum
                        |> Maybe.withDefault 5
            in
            Gen.Data.Magic.make_.details
                { name = valueFromTypes magic.name
                , class = Elm.maybe (Maybe.map valueFromTypes magic.class)
                , faction = Elm.maybe (Maybe.map valueFromTypes magic.faction)
                , hasRankZero = Elm.bool magic.hasRankZero
                , isElementalism = Elm.bool magic.isElementalism
                , affinities = affinitiesToExpression magic.elements
                , description = Elm.string magic.description
                , dlc = Elm.maybe (Maybe.map Elm.string dlcName)
                , ranks =
                    List.range 1 maxRank
                        |> List.map
                            (\rank ->
                                Dict.get rank magic.ranks
                                    |> Maybe.withDefault ""
                                    |> Elm.string
                            )
                        |> Elm.list
                }
                |> Elm.declaration (yassify magic.name)
                |> Elm.expose
        )
        magics


affinitiesToExpression : Parsers.MagicAffinity -> Elm.Expression
affinitiesToExpression affinity =
    case affinity of
        Parsers.Regular alternatives ->
            alternatives
                |> List.map valueFromTypes
                |> Elm.list
                |> Gen.Data.Magic.make_.regular

        Parsers.Alternative alternatives ->
            alternatives
                |> List.map
                    (\alternative ->
                        alternative
                            |> List.map valueFromTypes
                            |> Elm.list
                    )
                |> Elm.list
                |> Gen.Data.Magic.make_.alternative


affinitiesFile : List ( Maybe String, Parsers.Affinity ) -> Elm.File
affinitiesFile dlcAffinities =
    Elm.file [ "Generated", "Affinity" ]
        (Elm.expose (Elm.declaration "all" (allAffinities dlcAffinities))
            :: Elm.expose (affinityToColor dlcAffinities)
            :: dlcToAffinities dlcAffinities
        )


allAffinities : List ( Maybe String, Parsers.Affinity ) -> Elm.Expression
allAffinities dlcAffinities =
    dlcAffinities
        |> List.map (\( _, affinity ) -> Elm.val (affinityToVarName affinity.name))
        |> Elm.list
        |> Elm.withType (Elm.Annotation.list Gen.Data.Affinity.annotation_.details)


affinityToColor : List ( Maybe String, Parsers.Affinity ) -> Elm.Declaration
affinityToColor dlcAffinities =
    Elm.fn (Elm.Arg.var "affinity")
        (\affinity ->
            dlcAffinities
                |> List.map
                    (\( _, affinityData ) ->
                        Elm.Case.branch
                            (Elm.Arg.customType affinityData.name ())
                            (\() -> Elm.hex affinityData.color)
                    )
                |> Elm.Case.custom affinity (Elm.Annotation.named [ "Generated", "Types" ] "Affinity")
        )
        |> Elm.declaration "affinityToColor"


dlcToAffinities : List ( Maybe String, Parsers.Affinity ) -> List Elm.Declaration
dlcToAffinities affinities =
    List.map
        (\( dlcName, affinity ) ->
            Gen.Data.Affinity.make_.details
                { name = valueFromTypes affinity.name
                , dlc = Elm.maybe (Maybe.map Elm.string dlcName)
                }
                |> Elm.declaration (affinityToVarName affinity.name)
                |> Elm.expose
        )
        affinities


affinityToVarName : String -> String
affinityToVarName affinity =
    case affinity of
        "All" ->
            "all_"

        _ ->
            String.Extra.decapitalize (yassify affinity)


companionsFile : List ( Maybe String, Parsers.Companion ) -> Elm.File
companionsFile dlcCompanions =
    Elm.file [ "Generated", "Companion" ]
        (Elm.expose (Elm.declaration "all" (allCompanions dlcCompanions))
            :: dlcToCompanions dlcCompanions
        )


allCompanions : List ( Maybe String, Parsers.Companion ) -> Elm.Expression
allCompanions dlcCompanions =
    let
        toOrder faction =
            case faction of
                Just "The College of Arcadia" ->
                    0

                Just "Hawthorne Academia" ->
                    1

                Just "The Watchers" ->
                    2

                Just "The Hespatian Coven" ->
                    3

                Just "Lunabella" ->
                    4

                Just "AlfheimrAlliance" ->
                    5

                Just "The O.R.C." ->
                    6

                Just "Alphazon Industries" ->
                    7

                Nothing ->
                    8

                Just "The Outsiders" ->
                    9

                _ ->
                    10
    in
    Elm.Op.append
        (dlcCompanions
            |> List.Extra.gatherEqualsBy (\( _, companion ) -> companion.faction)
            |> List.sortBy (\( ( _, { faction } ), _ ) -> toOrder faction)
            |> List.map
                (\( ( _, { faction } ) as head, tail ) ->
                    Elm.tuple
                        (Elm.maybe (Maybe.map valueFromTypes faction))
                        ((head :: tail)
                            |> List.map
                                (\( _, companion ) ->
                                    Elm.val
                                        (String.Extra.decapitalize (yassify companion.name))
                                )
                            |> Elm.list
                        )
                )
            |> Elm.list
        )
        Gen.Data.Companion.all
        |> Elm.withType
            (Elm.Annotation.list
                (Elm.Annotation.tuple
                    (Elm.Annotation.maybe (Elm.Annotation.named [ "Generated", "Types" ] "Faction"))
                    (Elm.Annotation.list Gen.Data.Companion.annotation_.details)
                )
            )


dlcToCompanions : List ( Maybe String, Parsers.Companion ) -> List Elm.Declaration
dlcToCompanions companions =
    List.map
        (\( dlcName, companion ) ->
            let
                class : Elm.Expression
                class =
                    case companion.class of
                        Just "Any" ->
                            Gen.Data.Companion.make_.classAny

                        Just "Special" ->
                            Gen.Data.Companion.make_.classSpecial

                        Just c ->
                            Gen.Data.Companion.make_.classOne (valueFromTypes c)

                        Nothing ->
                            Gen.Data.Companion.make_.classNone

                score : Parsers.Score -> Elm.Expression
                score value =
                    case value of
                        Parsers.NormalScore s ->
                            Gen.Data.Companion.make_.normalScore (Elm.int s)

                        Parsers.SpecialEffect { worse, better } ->
                            Gen.Data.Companion.make_.specialEffect
                                (Elm.record
                                    [ ( "worse", Elm.maybe (Maybe.map Elm.int worse) )
                                    , ( "better", Elm.int better )
                                    ]
                                )
            in
            Gen.Data.Companion.make_.details
                { name = valueFromTypes companion.name
                , class = class
                , races =
                    Elm.list
                        (List.map
                            (\race ->
                                case
                                    race
                                        |> String.split "-"
                                        |> List.map valueFromTypes
                                of
                                    [ x ] ->
                                        x

                                    h :: t ->
                                        Elm.apply h t

                                    [] ->
                                        valueFromTypes "Empty list?"
                            )
                            companion.races
                        )
                , hasPerk = Elm.bool companion.hasPerk
                , cost = Elm.maybe (Maybe.map Elm.int companion.cost)
                , power = score companion.power
                , teamwork = score companion.teamwork
                , sociability = score companion.sociability
                , morality = score companion.morality
                , quote = Elm.string companion.quote
                , description = Elm.string companion.description
                , positives = Elm.list (List.map Elm.string companion.positives)
                , negatives = Elm.list (List.map Elm.string companion.negatives)
                , mixed = Elm.list (List.map Elm.string companion.mixed)
                , has = Elm.string companion.has
                , dlc = Elm.maybe (Maybe.map Elm.string dlcName)
                }
                |> Elm.declaration (yassify companion.name)
                |> Elm.expose
        )
        companions


complicationsFile : List ( Maybe String, Parsers.Complication ) -> Elm.File
complicationsFile dlcComplications =
    Elm.file [ "Generated", "Complication" ]
        (Elm.expose (Elm.declaration "all" (allComplications dlcComplications))
            :: dlcToComplications dlcComplications
        )


allComplications : List ( Maybe String, Parsers.Complication ) -> Elm.Expression
allComplications dlcComplications =
    dlcComplications
        |> List.map (\( _, complication ) -> Elm.val (String.Extra.decapitalize (yassify complication.name)))
        |> Elm.list
        |> Elm.withType (Elm.Annotation.list Gen.Data.Complication.annotation_.details)


dlcToComplications : List ( Maybe String, Parsers.Complication ) -> List Elm.Declaration
dlcToComplications complications =
    List.map
        (\( dlcName, complication ) ->
            Gen.Data.Complication.make_.details
                { name = valueFromTypes complication.name
                , class = Elm.maybe (Maybe.map valueFromTypes complication.class)
                , category = Elm.maybe (Maybe.map valueFromTypes complication.category)
                , content =
                    case complication.content of
                        Parsers.Single cost description ->
                            Gen.Data.Complication.make_.single (Elm.int cost) (Elm.string description)

                        Parsers.WithCosts costs description ->
                            Gen.Data.Complication.make_.withGains (Elm.list (List.map Elm.int costs)) (Elm.string description)

                        Parsers.WithChoices () before choices after ->
                            if complication.isTiered then
                                Gen.Data.Complication.make_.withTiers
                                    (Elm.string before)
                                    (choices
                                        |> List.map
                                            (\( choice, cost ) -> Elm.tuple (Elm.string choice) (Elm.int cost))
                                        |> Elm.list
                                    )
                                    (Elm.string after)

                            else
                                Gen.Data.Complication.make_.withChoices
                                    (Elm.string before)
                                    (choices
                                        |> List.map
                                            (\( choice, cost ) -> Elm.tuple (Elm.string choice) (Elm.int cost))
                                        |> Elm.list
                                    )
                                    (Elm.string after)
                , dlc = Elm.maybe (Maybe.map Elm.string dlcName)
                }
                |> Elm.declaration (yassify complication.name)
                |> Elm.expose
        )
        complications
