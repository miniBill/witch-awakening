module Generate exposing (main)

{-| -}

import Dict
import Elm
import Elm.Declare
import Gen.CodeGen.Generate as Generate exposing (Directory)
import Generate.Affinity
import Generate.Attribution exposing (DLCAttribution)
import Generate.Class
import Generate.Companion
import Generate.Complication
import Generate.ComplicationCategory
import Generate.Faction
import Generate.GameMode
import Generate.Gradient
import Generate.Image exposing (ImageModule)
import Generate.Magic
import Generate.Perk
import Generate.Quest
import Generate.Race
import Generate.Relic
import Generate.Size
import Generate.Slot
import Generate.TypePerk
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
                    |> Generate.Image.file
                    |> Result.andThen
                        (\image ->
                            ResultME.map2
                                (\gradientsFile dlcFiles ->
                                    { info = []
                                    , files = gradientsFile :: Elm.Declare.toFile image :: dlcFiles
                                    }
                                )
                                (List.concatMap Triple.Extra.second list
                                    |> Generate.Gradient.gradients
                                )
                                (List.concatMap Triple.Extra.third list
                                    |> Parsers.parseFiles
                                    |> ResultME.andThen (dlcToFiles image.call)
                                )
                        )
            )


dlcToFiles : ImageModule -> List Parsers.DLC -> ResultME Generate.Error (List Elm.File)
dlcToFiles image dlcList =
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

        ( types, enums ) =
            Generate.Types.file image dlcList
    in
    ResultME.map5
        (\racesFile typePerksFile relicsFile perksFile companionsFile ->
            [ Elm.Declare.toFile (Generate.Affinity.file types.call enums.affinity dlcAffinities)
            , Elm.Declare.toFile (Generate.Attribution.file dlcAttributions)
            , Elm.Declare.toFile (Generate.Class.file types.call enums.class dlcClasses)
            , Elm.Declare.toFile companionsFile
            , Elm.Declare.toFile (Generate.Complication.file types.call enums.complication dlcComplications)
            , Elm.Declare.toFile (Generate.ComplicationCategory.file enums.complicationCategory)
            , Elm.Declare.toFile (Generate.Faction.file types.call image enums.faction dlcFactions)
            , Elm.Declare.toFile (Generate.GameMode.file types.call enums.gameMode dlcGameModes)
            , Elm.Declare.toFile (Generate.Magic.file types.call enums.magic dlcMagics)
            , Elm.Declare.toFile (Generate.Quest.file types.call enums.quest dlcQuests)
            , Elm.Declare.toFile (Generate.Size.file enums.size)
            , Elm.Declare.toFile (Generate.Slot.file enums.slot)
            , Elm.Declare.toFile racesFile
            , Elm.Declare.toFile typePerksFile
            , Elm.Declare.toFile relicsFile
            , Elm.Declare.toFile perksFile
            , Elm.Declare.toFile types
            ]
        )
        (Generate.Race.file types.call enums.race dlcRaces)
        (Generate.TypePerk.file types.call dlcRaces)
        (Generate.Relic.file types.call enums.relic dlcRelics)
        (Generate.Perk.file types.call enums.perk dlcPerks)
        (Generate.Companion.file types.call enums.companion dlcCompanions)
