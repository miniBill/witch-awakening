module Generate.FromDLC exposing (files)

import Elm
import Elm.Annotation
import Elm.Arg
import Elm.Op
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
        { dlcRaces, dlcPerks } =
            List.foldr
                (\( dlcName, item ) acc ->
                    case item of
                        Parsers.DLCRace race ->
                            { acc | dlcRaces = ( dlcName, race ) :: acc.dlcRaces }

                        Parsers.DLCPerk perk ->
                            { acc | dlcPerks = ( dlcName, perk ) :: acc.dlcPerks }
                )
                { dlcRaces = [], dlcPerks = [] }
                (List.concatMap (\dlc -> List.map (Tuple.pair dlc.name) dlc.items) dlcList)
    in
    [ racesFile dlcRaces
    , typePerksFile dlcRaces
    , perksFile dlcPerks
    ]


racesFile : List ( String, Parsers.Race ) -> Elm.File
racesFile dlcRaces =
    Elm.file [ "Generated", "Races" ]
        (Elm.expose (Elm.declaration "all" (allRaces dlcRaces))
            :: dlcToRaces dlcRaces
        )


allRaces : List ( String, Parsers.Race ) -> Elm.Expression
allRaces dlcRaces =
    Elm.fn
        (Elm.Arg.varWith "races"
            (Elm.Annotation.list (Elm.Annotation.named [ "Generated", "Types" ] "Race"))
        )
    <|
        \races ->
            dlcRaces
                |> List.map (\( _, race ) -> Elm.val (String.Extra.decapitalize race.name))
                |> Elm.list
                |> Elm.Op.append (Gen.Data.Race.call_.all races)
                |> Elm.withType (Elm.Annotation.list Gen.Data.Race.annotation_.details)


dlcToRaces : List ( String, Parsers.Race ) -> List Elm.Declaration
dlcToRaces races =
    List.map
        (\( dlcName, race ) ->
            Gen.Data.Race.make_.details
                { name = fromTypes race.name
                , content = Elm.string race.description
                , tank = fromTypes race.manaCapacity
                , affinities = Elm.list (List.map fromTypes race.elements)
                , charge = fromTypes race.manaRate
                , dlc = Elm.maybe (Just (Elm.string dlcName))
                }
                |> Elm.declaration (yassify race.name)
                |> Elm.expose
        )
        races


perksFile : List ( String, Parsers.Perk ) -> Elm.File
perksFile dlcPerks =
    Elm.file [ "Generated", "Perks" ]
        (Elm.expose (Elm.declaration "all" (allPerks dlcPerks))
            :: dlcToPerks dlcPerks
        )


allPerks : List ( String, Parsers.Perk ) -> Elm.Expression
allPerks dlcPerks =
    Elm.fn
        (Elm.Arg.varWith "perks"
            (Elm.Annotation.list Gen.Types.annotation_.rankedPerk)
        )
    <|
        \perks ->
            dlcPerks
                |> List.map (\( _, perk ) -> Elm.val (String.Extra.decapitalize (yassify perk.name)))
                |> Elm.list
                |> Elm.Op.append (Gen.Data.Perk.call_.all perks)
                |> Elm.withType (Elm.Annotation.list Gen.Data.Perk.annotation_.details)


dlcToPerks : List ( String, Parsers.Perk ) -> List Elm.Declaration
dlcToPerks perks =
    List.map
        (\( dlcName, perk ) ->
            Gen.Data.Perk.make_.details
                { name = fromTypes perk.name
                , class = fromTypes perk.class
                , affinity = fromTypes perk.element
                , isMeta = Elm.bool False
                , content = Gen.Data.Perk.make_.single (Elm.int perk.cost) (Elm.string perk.description)
                , dlc = Elm.maybe (Just (Elm.string dlcName))
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


typePerksFile : List ( String, Parsers.Race ) -> Elm.File
typePerksFile dlcRaces =
    Elm.file [ "Generated", "TypePerks" ]
        (Elm.expose (Elm.declaration "all" (allTypePerks dlcRaces))
            :: dlcToTypePerks dlcRaces
        )


allTypePerks : List ( String, Parsers.Race ) -> Elm.Expression
allTypePerks dlcRaces =
    dlcRaces
        |> List.map (\( _, race ) -> Elm.val (String.Extra.decapitalize race.name))
        |> Elm.list
        |> Elm.Op.append Gen.Data.TypePerk.all
        |> Elm.withType (Elm.Annotation.list Gen.Data.TypePerk.annotation_.details)


dlcToTypePerks : List ( String, Parsers.Race ) -> List Elm.Declaration
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
                            , dlc = Elm.maybe (Just (Elm.string dlcName))
                            }
                            |> Elm.declaration (yassify race.name)
                            |> Elm.expose
                    )
        )
        races
