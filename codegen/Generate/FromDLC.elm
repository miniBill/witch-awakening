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
import Generate.Classes
import Generate.Magics
import Generate.Perks
import Generate.Races
import Generate.Relics
import Generate.TypePerks
import Generate.Types
import Generate.Utils exposing (yassify)
import List.Extra
import Parser
import Parser.Error
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
                        "Could not parse " ++ folder ++ "/" ++ filename ++ "\n" ++ Parser.Error.toString deadEnds
                  }
                ]
            )


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
    , Elm.Declare.toFile (Generate.Classes.file dlcClasses)
    , companionsFile dlcCompanions
    , complicationsFile dlcComplications
    , Elm.Declare.toFile (Generate.Magics.file dlcMagics)
    , Elm.Declare.toFile (Generate.Perks.file dlcPerks)
    , Elm.Declare.toFile (Generate.Races.file dlcRaces)
    , Elm.Declare.toFile (Generate.Relics.file dlcRelics)
    , Elm.Declare.toFile (Generate.TypePerks.file dlcRaces)
    , Elm.Declare.toFile (Generate.Types.file dlcList)
    ]


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
                { name = Generate.Types.value affinity.name
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
                        (Elm.maybe (Maybe.map Generate.Types.value faction))
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
                            Gen.Data.Companion.make_.classOne (Generate.Types.value c)

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
                { name = Generate.Types.value companion.name
                , class = class
                , races =
                    Elm.list
                        (List.map
                            (\race ->
                                case
                                    race
                                        |> String.split "-"
                                        |> List.map Generate.Types.value
                                of
                                    [ x ] ->
                                        x

                                    h :: t ->
                                        Elm.apply h t

                                    [] ->
                                        Generate.Types.value "Empty list?"
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
                { name = Generate.Types.value complication.name
                , class = Elm.maybe (Maybe.map Generate.Types.value complication.class)
                , category = Elm.maybe (Maybe.map Generate.Types.value complication.category)
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
