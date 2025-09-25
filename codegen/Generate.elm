module Generate exposing (main)

{-| -}

import Dict
import Elm
import Elm.Declare
import Gen.CodeGen.Generate as Generate exposing (Directory)
import Generate.Affinities
import Generate.Attributions exposing (DLCAttribution)
import Generate.Classes
import Generate.Companions
import Generate.Complications
import Generate.Factions
import Generate.GameModes
import Generate.Gradients
import Generate.Images exposing (ImagesModule)
import Generate.Magics
import Generate.Perks
import Generate.Quests
import Generate.Races
import Generate.Relics
import Generate.TypePerks
import Generate.Types
import Json.Decode exposing (Decoder, Value)
import List.Nonempty as Nonempty
import Parsers
import ResultME exposing (ResultME)
import Triple.Extra


main : Program Value () ()
main =
    Platform.worker
        { init = init
        , update = \_ model -> ( model, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }


init : Value -> ( (), Cmd () )
init flags =
    case Json.Decode.decodeValue directoryDecoder flags of
        Ok input ->
            ( ()
            , case toFiles input of
                Ok result ->
                    Cmd.batch <|
                        List.map Generate.info result.info
                            ++ [ Generate.files result.files ]

                Err errors ->
                    Generate.error (Nonempty.toList errors)
            )

        Err e ->
            ( ()
            , Generate.error
                [ { title = "Error decoding flags"
                  , description = Json.Decode.errorToString e
                  }
                ]
            )


directoryDecoder : Decoder Generate.Directory
directoryDecoder =
    Json.Decode.lazy
        (\_ ->
            Json.Decode.oneOf
                [ Json.Decode.map Ok Json.Decode.string
                , Json.Decode.map Err directoryDecoder
                ]
                |> Json.Decode.dict
                |> Json.Decode.map
                    (\entries ->
                        entries
                            |> Dict.toList
                            |> List.foldl
                                (\( name, entry ) ( dirAcc, fileAcc ) ->
                                    case entry of
                                        Ok file ->
                                            ( dirAcc, ( name, file ) :: fileAcc )

                                        Err directory ->
                                            ( ( name, directory ) :: dirAcc, fileAcc )
                                )
                                ( [], [] )
                            |> (\( dirAcc, fileAcc ) ->
                                    Generate.Directory
                                        { directories = Dict.fromList dirAcc
                                        , files = Dict.fromList fileAcc
                                        }
                               )
                    )
        )


toFiles :
    Directory
    -> ResultME Generate.Error { info : List String, files : List Elm.File }
toFiles root =
    let
        go : String -> Generate.Directory -> List ( String, String, String )
        go folder (Generate.Directory { files, directories }) =
            List.map
                (\( fileName, fileContent ) ->
                    ( folder, fileName, fileContent )
                )
                (Dict.toList files)
                ++ List.concatMap
                    (\( directoryName, dir ) ->
                        go
                            (if String.isEmpty folder then
                                directoryName

                             else
                                folder ++ "/" ++ directoryName
                            )
                            dir
                    )
                    (Dict.toList directories)
    in
    go "" root
        |> ResultME.combineMap
            (\( folder, fileName, fileContent ) ->
                case fileName of
                    "sizes" ->
                        Ok ( [ fileContent ], [], [] )

                    _ ->
                        if String.endsWith "_gradient.ppm" fileName then
                            Ok ( [], [ ( fileName, fileContent ) ], [] )

                        else if String.endsWith ".md" fileName then
                            Ok ( [], [], [ ( folder, fileName, fileContent ) ] )

                        else
                            ResultME.error
                                { title = "Unexpected file"
                                , description = "File " ++ fileName ++ " unexpected, don’t know how to handle it"
                                }
            )
        |> Result.andThen
            (\list ->
                List.concatMap Triple.Extra.first list
                    |> Generate.Images.images
                    |> Result.andThen
                        (\images ->
                            ResultME.map2
                                (\gradientsFile dlcFiles ->
                                    { info = []
                                    , files = gradientsFile :: Elm.Declare.toFile images :: dlcFiles
                                    }
                                )
                                (List.concatMap Triple.Extra.second list
                                    |> Generate.Gradients.gradients
                                )
                                (List.concatMap Triple.Extra.third list
                                    |> Parsers.parseFiles
                                    |> ResultME.andThen (dlcToFiles images.call)
                                )
                        )
            )


dlcToFiles : ImagesModule -> List Parsers.DLC -> ResultME Generate.Error (List Elm.File)
dlcToFiles images dlcList =
    let
        { dlcAffinities, dlcClasses, dlcGameModes, dlcCompanions, dlcQuests, dlcComplications, dlcMagics, dlcPerks, dlcRaces, dlcRelics, dlcFactions } =
            List.foldr
                (\( dlcName, item ) acc ->
                    case item of
                        Parsers.DLCAffinity affinity ->
                            { acc | dlcAffinities = ( dlcName, affinity ) :: acc.dlcAffinities }

                        Parsers.DLCClass class ->
                            { acc | dlcClasses = ( dlcName, class ) :: acc.dlcClasses }

                        Parsers.DLCGameMode gameMode ->
                            { acc | dlcGameModes = ( dlcName, gameMode ) :: acc.dlcGameModes }

                        Parsers.DLCCompanion companion ->
                            { acc | dlcCompanions = ( dlcName, companion ) :: acc.dlcCompanions }

                        Parsers.DLCQuest quest ->
                            { acc | dlcQuests = ( dlcName, quest ) :: acc.dlcQuests }

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

                        Parsers.DLCFaction faction ->
                            { acc | dlcFactions = ( dlcName, faction ) :: acc.dlcFactions }
                )
                { dlcAffinities = []
                , dlcClasses = []
                , dlcGameModes = []
                , dlcCompanions = []
                , dlcQuests = []
                , dlcComplications = []
                , dlcMagics = []
                , dlcPerks = []
                , dlcRaces = []
                , dlcRelics = []
                , dlcFactions = []
                }
                (List.concatMap (\dlc -> List.map (Tuple.pair dlc.name) dlc.items) dlcList)

        dlcAttributions : List DLCAttribution
        dlcAttributions =
            dlcList
                |> List.filterMap
                    (\{ name, author, link } ->
                        Maybe.map2
                            (\n a ->
                                { name = n
                                , author = a
                                , link = link
                                }
                            )
                            name
                            author
                    )

        types : Elm.Declare.Module Generate.Types.TypesModule
        types =
            Generate.Types.file images dlcList
    in
    ResultME.map2
        (\racesFile typePerksFile ->
            [ Elm.Declare.toFile (Generate.Affinities.file types.call dlcAffinities)
            , Elm.Declare.toFile (Generate.Classes.file types.call dlcClasses)
            , Elm.Declare.toFile (Generate.GameModes.file types.call dlcGameModes)
            , Elm.Declare.toFile (Generate.Companions.file types.call dlcCompanions)
            , Elm.Declare.toFile (Generate.Quests.file types.call dlcQuests)
            , Elm.Declare.toFile (Generate.Complications.file types.call dlcComplications)
            , Elm.Declare.toFile (Generate.Magics.file types.call dlcMagics)
            , Elm.Declare.toFile (Generate.Perks.file types.call dlcPerks)
            , Elm.Declare.toFile racesFile
            , Elm.Declare.toFile (Generate.Relics.file types.call dlcRelics)
            , Elm.Declare.toFile typePerksFile
            , Elm.Declare.toFile types
            , Elm.Declare.toFile (Generate.Attributions.file dlcAttributions)
            , Elm.Declare.toFile (Generate.Factions.file types.call dlcFactions)
            ]
        )
        (Generate.Races.file types.call dlcRaces)
        (Generate.TypePerks.file types.call dlcRaces)
