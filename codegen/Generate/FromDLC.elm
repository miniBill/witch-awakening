module Generate.FromDLC exposing (files)

import Dict
import Elm
import Elm.Annotation
import Elm.Arg
import Elm.Op
import Gen.Data.Magic
import Gen.Data.Perk
import Gen.Data.Race
import Gen.Data.TypePerk
import Gen.Types
import Generate.Utils exposing (yassify)
import Parsers exposing (DLCItem(..))
import String.Extra


files : List Parsers.DLC -> List Elm.File
files dlcList =
    let
        { dlcRaces, dlcPerks, dlcMagics } =
            List.foldr
                (\( dlcName, item ) acc ->
                    case item of
                        Parsers.DLCRace race ->
                            { acc | dlcRaces = ( dlcName, race ) :: acc.dlcRaces }

                        Parsers.DLCPerk perk ->
                            { acc | dlcPerks = ( dlcName, perk ) :: acc.dlcPerks }

                        Parsers.DLCMagic magic ->
                            { acc | dlcMagics = ( dlcName, magic ) :: acc.dlcMagics }
                )
                { dlcRaces = [], dlcPerks = [], dlcMagics = [] }
                (List.concatMap (\dlc -> List.map (Tuple.pair dlc.name) dlc.items) dlcList)
    in
    [ racesFile dlcRaces
    , typePerksFile dlcRaces
    , perksFile dlcPerks
    , magicsFile dlcMagics
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

                        Parsers.WithCosts description costs ->
                            Gen.Data.Perk.make_.withCosts (Elm.string description) (Elm.list (List.map Elm.int costs))

                        Parsers.WithChoices before choices after ->
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
    Elm.Op.append
        Gen.Data.Magic.all
        (dlcMagics
            |> List.map (\( _, magic ) -> Elm.val (String.Extra.decapitalize (yassify magic.name)))
            |> Elm.list
        )
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
