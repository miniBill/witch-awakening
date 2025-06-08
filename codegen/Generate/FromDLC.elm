module Generate.FromDLC exposing (files)

import Dict
import Dict.Extra
import Elm
import Elm.Declare
import Gen.CodeGen.Generate as Generate
import Generate.Affinities
import Generate.Classes
import Generate.Companions
import Generate.Complications
import Generate.Magics
import Generate.Perks
import Generate.Races
import Generate.Relics
import Generate.TypePerks
import Generate.Types
import Parser
import Parser.Error
import Parsers exposing (DLCItem(..))
import Result.Extra


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
    [ Generate.Affinities.file dlcAffinities
    , Elm.Declare.toFile (Generate.Classes.file dlcClasses)
    , Generate.Companions.file dlcCompanions
    , Generate.Complications.file dlcComplications
    , Elm.Declare.toFile (Generate.Magics.file dlcMagics)
    , Elm.Declare.toFile (Generate.Perks.file dlcPerks)
    , Elm.Declare.toFile (Generate.Races.file dlcRaces)
    , Elm.Declare.toFile (Generate.Relics.file dlcRelics)
    , Elm.Declare.toFile (Generate.TypePerks.file dlcRaces)
    , Elm.Declare.toFile (Generate.Types.file dlcList)
    ]
