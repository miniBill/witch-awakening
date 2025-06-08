module Generate.FromDLC exposing (files)

import Dict
import Elm
import Elm.Annotation
import Elm.Arg
import Elm.Case
import Elm.Op
import Gen.Data.Affinity
import Gen.Data.Class
import Gen.Data.Companion
import Gen.Data.Complication
import Gen.Data.Magic
import Gen.Data.Perk
import Gen.Data.Race
import Gen.Data.Relic
import Gen.Data.TypePerk
import Gen.Types
import Generate.Utils exposing (yassify)
import List.Extra
import Parsers exposing (DLCItem(..))
import String.Extra


files : List Parsers.DLC -> List Elm.File
files dlcList =
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
    , classesFile dlcClasses
    , companionsFile dlcCompanions
    , complicationsFile dlcComplications
    , magicsFile dlcMagics
    , perksFile dlcPerks
    , racesFile dlcRaces
    , relicsFile dlcRelics
    , typePerksFile dlcRaces
    ]


racesFile : List ( Maybe String, Parsers.Race ) -> Elm.File
racesFile dlcRaces =
    Elm.file [ "Generated", "Races" ]
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
                { name = fromTypes race.name
                , content = Elm.string race.description
                , tank = fromTypes race.manaCapacity
                , affinities = Elm.list (List.map fromTypes race.elements)
                , charge = fromTypes race.manaRate
                , dlc = Elm.maybe (Maybe.map Elm.string dlcName)
                }
                |> Elm.declaration (yassify race.name)
                |> Elm.expose
        )
        races


perksFile : List ( Maybe String, Parsers.Perk ) -> Elm.File
perksFile dlcPerks =
    Elm.file [ "Generated", "Perks" ]
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
                { name = fromTypes perk.name
                , class = fromTypes perk.class
                , affinity = fromTypes perk.element
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


fromTypes : String -> Elm.Expression
fromTypes name =
    Elm.value
        { importFrom = [ "Generated", "Types" ]
        , name = yassify name
        , annotation = Nothing
        }


typePerksFile : List ( Maybe String, Parsers.Race ) -> Elm.File
typePerksFile dlcRaces =
    Elm.file [ "Generated", "TypePerks" ]
        (Elm.expose (Elm.declaration "all" (allTypePerks dlcRaces))
            :: dlcToTypePerks dlcRaces
        )


allTypePerks : List ( Maybe String, Parsers.Race ) -> Elm.Expression
allTypePerks dlcRaces =
    Elm.Op.append
        (dlcRaces
            |> List.filterMap
                (\( _, race ) ->
                    if race.perk == Nothing then
                        Nothing

                    else
                        Just (Elm.val (String.Extra.decapitalize race.name))
                )
            |> Elm.list
        )
        Gen.Data.TypePerk.all
        |> Elm.withType (Elm.Annotation.list Gen.Data.TypePerk.annotation_.details)


dlcToTypePerks : List ( Maybe String, Parsers.Race ) -> List Elm.Declaration
dlcToTypePerks races =
    List.filterMap
        (\( dlcName, race ) ->
            race.perk
                |> Maybe.map
                    (\perk ->
                        Gen.Data.TypePerk.make_.details
                            { race = fromTypes race.name
                            , content = Elm.string perk.description
                            , cost = Elm.int perk.cost
                            , dlc = Elm.maybe (Maybe.map Elm.string dlcName)
                            }
                            |> Elm.declaration (yassify race.name)
                            |> Elm.expose
                    )
        )
        races


magicsFile : List ( Maybe String, Parsers.Magic ) -> Elm.File
magicsFile dlcMagics =
    Elm.file [ "Generated", "Magics" ]
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
                { name = fromTypes magic.name
                , class = Elm.maybe (Maybe.map fromTypes magic.class)
                , faction = Elm.maybe (Maybe.map fromTypes magic.faction)
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
                |> List.map fromTypes
                |> Elm.list
                |> Gen.Data.Magic.make_.regular

        Parsers.Alternative alternatives ->
            alternatives
                |> List.map
                    (\alternative ->
                        alternative
                            |> List.map fromTypes
                            |> Elm.list
                    )
                |> Elm.list
                |> Gen.Data.Magic.make_.alternative


affinitiesFile : List ( Maybe String, Parsers.Affinity ) -> Elm.File
affinitiesFile dlcAffinities =
    Elm.file [ "Generated", "Affinities" ]
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
                { name = fromTypes affinity.name
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


relicsFile : List ( Maybe String, Parsers.Relic ) -> Elm.File
relicsFile dlcRelics =
    Elm.file [ "Generated", "Relics" ]
        (Elm.expose (Elm.declaration "all" (allRelics dlcRelics))
            :: dlcToRelics dlcRelics
        )


allRelics : List ( Maybe String, Parsers.Relic ) -> Elm.Expression
allRelics dlcRelics =
    dlcRelics
        |> List.map (\( _, relic ) -> Elm.val (String.Extra.decapitalize (yassify relic.name)))
        |> Elm.list
        |> Elm.withType (Elm.Annotation.list Gen.Data.Relic.annotation_.details)


dlcToRelics : List ( Maybe String, Parsers.Relic ) -> List Elm.Declaration
dlcToRelics relics =
    List.map
        (\( dlcName, relic ) ->
            Gen.Data.Relic.make_.details
                { name = fromTypes relic.name
                , classes = Elm.list (List.map fromTypes relic.classes)
                , dlc = Elm.maybe (Maybe.map Elm.string dlcName)
                , content =
                    case relic.content of
                        Parsers.Single cost description ->
                            if relic.name == "Cosmic Pearl" then
                                Gen.Data.Relic.make_.cosmicPearlContent (Elm.int cost) (Elm.string description)

                            else
                                Gen.Data.Relic.make_.single (Elm.int cost) (Elm.string description)

                        Parsers.WithCosts costs description ->
                            Gen.Data.Relic.make_.withChoices (Elm.list (List.map Elm.int costs)) (Elm.string description)

                        Parsers.WithChoices ever _ _ _ ->
                            never ever
                }
                |> Elm.declaration (yassify relic.name)
                |> Elm.expose
        )
        relics


companionsFile : List ( Maybe String, Parsers.Companion ) -> Elm.File
companionsFile dlcCompanions =
    Elm.file [ "Generated", "Companions" ]
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
                        (Elm.maybe (Maybe.map fromTypes faction))
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
                            Gen.Data.Companion.make_.classOne (fromTypes c)

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
                { name = fromTypes companion.name
                , class = class
                , races =
                    Elm.list
                        (List.map
                            (\race ->
                                case
                                    race
                                        |> String.split "-"
                                        |> List.map fromTypes
                                of
                                    [ x ] ->
                                        x

                                    h :: t ->
                                        Elm.apply h t

                                    [] ->
                                        fromTypes "Empty list?"
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
    Elm.file [ "Generated", "Complications" ]
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
                { name = fromTypes complication.name
                , class = Elm.maybe (Maybe.map fromTypes complication.class)
                , category = Elm.maybe (Maybe.map fromTypes complication.category)
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


classesFile : List ( Maybe String, Parsers.Class ) -> Elm.File
classesFile dlcClasses =
    Elm.file [ "Generated", "Classes" ]
        (Elm.expose (Elm.declaration "all" (allClasses dlcClasses))
            :: Elm.expose (classToColor dlcClasses)
            :: dlcToClasses dlcClasses
        )


allClasses : List ( Maybe String, Parsers.Class ) -> Elm.Expression
allClasses dlcClasses =
    dlcClasses
        |> List.map (\( _, class ) -> Elm.val (String.Extra.decapitalize (yassify class.name)))
        |> Elm.list
        |> Elm.withType (Elm.Annotation.list Gen.Data.Class.annotation_.details)


classToColor : List ( Maybe String, Parsers.Class ) -> Elm.Declaration
classToColor dlcClasses =
    Elm.fn (Elm.Arg.var "class")
        (\class ->
            dlcClasses
                |> List.map
                    (\( _, classData ) ->
                        Elm.Case.branch
                            (Elm.Arg.customType classData.name ())
                            (\() -> Elm.hex classData.color)
                    )
                |> Elm.Case.custom class (Elm.Annotation.named [ "Generated", "Types" ] "Class")
        )
        |> Elm.declaration "classToColor"


dlcToClasses : List ( Maybe String, Parsers.Class ) -> List Elm.Declaration
dlcToClasses classes =
    List.map
        (\( dlcName, class ) ->
            Gen.Data.Class.make_.details
                { name = fromTypes class.name
                , content = Elm.string class.description
                , color = Elm.hex class.color
                , dlc = Elm.maybe (Maybe.map Elm.string dlcName)
                }
                |> Elm.declaration (yassify class.name)
                |> Elm.expose
        )
        classes
