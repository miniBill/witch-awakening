module Generate.FromDLC exposing (..)

import Elm
import Elm.Annotation
import Elm.Arg
import Elm.Op
import Gen.Data.Race
import Parsers exposing (DLCItem(..))
import String.Extra


files : List Parsers.DLC -> List Elm.File
files dlcList =
    let
        { dlcRaces, dlcPerks } =
            List.foldl
                (\( dlcName, item ) acc ->
                    case item of
                        Parsers.DLCRace race ->
                            { acc | dlcRaces = ( dlcName, race ) :: acc.dlcRaces }

                        Parsers.DLCPerk perk ->
                            { acc | dlcPerks = ( dlcName, perk ) :: acc.dlcPerks }
                )
                { dlcRaces = [], dlcPerks = [] }
                (List.concatMap (\dlc -> List.map (Tuple.pair dlc.name) dlc.items) dlcList)

        racesFile : Elm.File
        racesFile =
            let
                allRaces : Elm.Expression
                allRaces =
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
            in
            Elm.file [ "Generated", "Races" ]
                (Elm.expose (Elm.declaration "all" allRaces)
                    :: dlcToRaces dlcRaces
                )
    in
    [ racesFile ]


dlcToRaces : List ( String, Parsers.Race ) -> List Elm.Declaration
dlcToRaces races =
    let
        fromTypes : String -> Elm.Expression
        fromTypes name =
            Elm.value
                { importFrom = [ "Generated", "Types" ]
                , name = name
                , annotation = Nothing
                }
    in
    List.map
        (\( dlcName, race ) ->
            Gen.Data.Race.make_.details
                { name = fromTypes race.name
                , content = Elm.string race.description
                , tank = fromTypes race.manaCapacity
                , affinities = Elm.list (List.map fromTypes race.elements)
                , charge = fromTypes race.manaRate
                }
                |> Elm.declaration race.name
                |> Elm.expose
        )
        races
