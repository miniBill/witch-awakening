module WitchAwakeningBuildfile exposing (buildAction, buildFile, getInputs)

import BackendTask exposing (BackendTask)
import BackendTask.File.Extra
import BackendTask.Glob as Glob
import Build exposing (BuildFile)
import BuildTask exposing (BuildTask, FileOrDirectory)
import BuildTask.Do as Do
import BuildTask.Elm as Elm
import BuildTask.Font as Font
import BuildTask.Image as Image
import BuildTask.Unsafe
import BuildTask.Unsafe.Do
import Elm
import Elm.Declare
import Example
import FatalError exposing (FatalError)
import Gen.CodeGen.Generate
import Gen.Html.Attributes
import Generate
import Generate.Gradient
import Generate.Image
import Generate.Utils as Utils
import List.Extra
import List.Nonempty
import Maybe.Extra
import Parsers exposing (DLC)
import Path.Posix as Path exposing (Path)
import Utils


buildFile : BuildFile { inputPath : String } Inputs
buildFile =
    { getInputs = getInputs
    , buildAction = buildAction
    }


type ProcessedFile
    = ProcessedImage
        { original : HashedFileWith { width : Int, height : Int }
        , converted : List (HashedFileWith { width : Int })
        }
    | ProcessedCss HashedFile
    | ProcessedSvg (HashedFileWith { width : Int, height : Int })
    | ProcessedFont (HashedFileWith Font.Data)
    | ProcessedDLC (HashedFileWith { data : DLC })


type alias HashedFile =
    { filename : Path Path.Relative Path.File
    , hash : FileOrDirectory
    }


type alias HashedFileWith a =
    { a
        | filename : Path Path.Relative Path.File
        , hash : FileOrDirectory
    }


type alias Inputs =
    { inputPath : Path Path.Absolute Path.Directory
    , images : List ( Path Path.Absolute Path.File, BuildTask FatalError FileOrDirectory )
    , gradients : List ( Path Path.Absolute Path.File, BuildTask FatalError FileOrDirectory )
    }


getInputs :
    { config
        | inputPath : String
        , buildPath : String
        , debug : Bool
    }
    -> BackendTask FatalError Inputs
getInputs config =
    BackendTask.File.Extra.resolveDirectory config.inputPath
        |> BackendTask.andThen
            (\inputPath ->
                let
                    glob : String -> BackendTask FatalError (List (Path Path.Absolute Path.File))
                    glob path =
                        Glob.fromStringWithOptions
                            (let
                                defaultOptions : Glob.Options
                                defaultOptions =
                                    Glob.defaultOptions
                             in
                             { defaultOptions | include = Glob.OnlyFiles }
                            )
                            path
                            |> BackendTask.andThen
                                (\paths ->
                                    paths
                                        |> List.map
                                            (\pathString ->
                                                case Path.parseAbsoluteFile pathString of
                                                    Nothing ->
                                                        BackendTask.fail (FatalError.fromString ("Invalid path: " ++ path))

                                                    Just parsed ->
                                                        BackendTask.succeed parsed
                                            )
                                        |> BackendTask.combine
                                )
                in
                BackendTask.map2 Tuple.pair
                    (glob (Path.toString inputPath ++ "/**"))
                    (glob (Path.toString inputPath ++ "../DLCs/**"))
                    |> BackendTask.andThen
                        (\( found1, found2 ) ->
                            let
                                ( gradients, notGradients ) =
                                    (found1 ++ found2)
                                        |> List.sortBy Path.toString
                                        |> List.partition isGradient
                            in
                            BackendTask.map2 (Inputs inputPath)
                                (notGradients
                                    |> List.Extra.removeWhen
                                        (\p ->
                                            String.contains "/raw/" (Path.toString p)
                                                || String.contains "/originals/" (Path.toString p)
                                        )
                                    |> BuildTask.inputs config
                                )
                                (gradients
                                    |> BuildTask.inputs config
                                )
                        )
            )


isGradient : Path Path.Absolute Path.File -> Bool
isGradient path =
    Path.toString path
        |> String.endsWith Generate.Gradient.suffix


buildAction : Inputs -> BuildTask FatalError FileOrDirectory
buildAction inputs =
    BuildTask.do Example.getTools <| \tools ->
    BuildTask.andThen2
        (\gradients i ->
            BuildTask.combineInto
                ((gradients :: i.generated)
                    ++ i.other
                )
        )
        (buildGradients tools inputs)
        (buildImages tools inputs)


buildGradients : Example.Tools -> Inputs -> BuildTask FatalError HashedFile
buildGradients tools inputs =
    Do.all
        (\( path, file ) ->
            BuildTask.do file <| \gradientPng ->
            BuildTask.do (BuildTask.which "magick") <| \magick ->
            Do.allowFatal (BuildTask.Unsafe.pipeThrough magick [ "-", "-compress", "none", "ppm:-" ] gradientPng) <| \gradientPpm ->
            BuildTask.withFileFatal gradientPpm <| \content ->
            case
                Generate.Gradient.gradient
                    { path = path
                    , content = content
                    }
            of
                Ok result ->
                    BuildTask.succeed result

                Err errs ->
                    errs
                        |> List.Nonempty.toList
                        |> String.join ", "
                        |> FatalError.fromString
                        |> BuildTask.fail
        )
        inputs.gradients
    <| \declarations ->
    elmCodegen tools (Elm.file [ "Generated", "Gradient" ] declarations)


buildImages :
    Example.Tools
    -> Inputs
    ->
        BuildTask
            FatalError
            { generated : List HashedFile
            , other : List HashedFile
            }
buildImages tools inputs =
    let
        inputSize : Int
        inputSize =
            List.length inputs.images
    in
    BuildTask.do
        (Do.jobs <| \parallelism ->
        inputs.images
            |> List.indexedMap (processFile inputs tools inputSize)
            |> BuildTask.combineBy parallelism
            |> BuildTask.map Maybe.Extra.values
        )
    <| \processedFiles ->
    let
        fontFiles : List (HashedFileWith Font.Data)
        fontFiles =
            List.filterMap asFont processedFiles

        imageFiles :
            List
                { original : HashedFileWith { width : Int, height : Int }
                , converted : List (HashedFileWith { width : Int })
                }
        imageFiles =
            List.filterMap asImage processedFiles

        dlcFiles : List DLC
        dlcFiles =
            List.filterMap asDLC processedFiles

        publicFolder : BuildTask FatalError FileOrDirectory
        publicFolder =
            Do.allowFatal (BuildTask.writeFile (Font.toCssFile fontFiles)) <| \fontsCssHash ->
            ({ filename = Path.parseRelativeFile "fonts.css" |> orDie
             , hash = fontsCssHash
             }
                :: List.concatMap processedFileToFileList processedFiles
            )
                |> BuildTask.combineInto
                |> BuildTask.withPrefix ("[" ++ String.fromInt inputSize ++ "/" ++ String.fromInt inputSize ++ "]")
    in
    imagesElmFile tools processedFiles
        |> BuildTask.andThen
            (\imagesElm ->
                let
                    generateTask : BuildTask FatalError (List Elm.File)
                    generateTask =
                        dlcFiles
                            |> Parsers.combineDLCs
                            |> Generate.dlcToFiles imagesElm.module_
                            |> Result.mapError
                                (\errors ->
                                    errors
                                        |> List.Nonempty.toList
                                        |> List.map .description
                                        |> String.join ", "
                                        |> FatalError.fromString
                                )
                            |> BuildTask.fromResult
                in
                BuildTask.do generateTask <| \generated ->
                Do.all (elmCodegen tools) generated <| \dlcs ->
                BuildTask.succeed ( imagesElm, dlcs )
            )
        |> BuildTask.map4
            (\fontsElm imageSizes public ( imagesElm, dlcs ) ->
                { generated = [ imagesElm.file, fontsElm ]
                , other =
                    let
                        common : List HashedFile
                        common =
                            [ { filename = Path.parseRelativeFile "image-sizes" |> orDie, hash = imageSizes }
                            , { filename = Path.parseRelativeFile "public" |> orDie, hash = public }
                            ]
                    in
                    common ++ dlcs
                }
            )
            (elmCodegen tools (fontsElmFile fontFiles))
            (imagesSizesFile imageFiles)
            publicFolder


orDie : Maybe (Path base kind) -> Path base kind
orDie path =
    case path of
        Just p ->
            p

        Nothing ->
            let
                _ =
                    modBy 0 0
            in
            orDie path


asImage :
    ProcessedFile
    ->
        Maybe
            { original : HashedFileWith { width : Int, height : Int }
            , converted : List (HashedFileWith { width : Int })
            }
asImage file =
    case file of
        ProcessedImage data ->
            Just data

        ProcessedCss _ ->
            Nothing

        ProcessedSvg _ ->
            Nothing

        ProcessedFont _ ->
            Nothing

        ProcessedDLC _ ->
            Nothing


asFont : ProcessedFile -> Maybe (HashedFileWith Font.Data)
asFont file =
    case file of
        ProcessedFont data ->
            Just data

        ProcessedImage _ ->
            Nothing

        ProcessedCss _ ->
            Nothing

        ProcessedSvg _ ->
            Nothing

        ProcessedDLC _ ->
            Nothing


asDLC : ProcessedFile -> Maybe DLC
asDLC file =
    case file of
        ProcessedDLC { data } ->
            Just data

        ProcessedFont _ ->
            Nothing

        ProcessedImage _ ->
            Nothing

        ProcessedCss _ ->
            Nothing

        ProcessedSvg _ ->
            Nothing


imagesSizesFile :
    List
        { a
            | original : HashedFileWith { width : Int, height : Int }
        }
    -> BuildTask FatalError FileOrDirectory
imagesSizesFile processedFiles =
    let
        content : String
        content =
            processedFiles
                |> List.map
                    (\{ original } ->
                        let
                            name : String
                            name =
                                Path.toString original.filename
                                    |> String.replace " " "_"
                        in
                        name
                            ++ ": "
                            ++ String.fromInt original.width
                            ++ "x"
                            ++ String.fromInt original.height
                    )
                |> String.join "\n"
    in
    BuildTask.writeFile content
        |> BuildTask.allowFatal


fontsElmFile : List (HashedFileWith Font.Data) -> Elm.File
fontsElmFile files =
    files
        |> List.map .family
        |> List.Extra.unique
        |> List.map
            (\family ->
                Elm.declaration (Utils.yassify family) (Gen.Html.Attributes.style "font-family" family)
                    |> Elm.expose
            )
        |> Elm.file [ "Generated", "Fonts" ]


imagesElmFile :
    Example.Tools
    -> List ProcessedFile
    ->
        BuildTask
            FatalError
            { module_ : Generate.Image.ImageModule
            , file : HashedFile
            }
imagesElmFile tools list =
    let
        asImage_ :
            ProcessedFile
            ->
                Maybe
                    { svg : Bool
                    , filename : Path Path.Relative Path.File
                    , hash : FileOrDirectory
                    , width : Int
                    , height : Int
                    }
        asImage_ processedFile =
            case processedFile of
                ProcessedImage { original } ->
                    Just
                        { svg = False
                        , filename = original.filename
                        , hash = original.hash
                        , width = original.width
                        , height = original.height
                        }

                ProcessedSvg original ->
                    Just
                        { svg = True
                        , filename = original.filename
                        , hash = original.hash
                        , width = original.width
                        , height = original.height
                        }

                ProcessedCss _ ->
                    Nothing

                ProcessedFont _ ->
                    Nothing

                ProcessedDLC _ ->
                    Nothing

        imagesList : List { svg : Bool, filename : Path Path.Relative Path.File, hash : FileOrDirectory, width : Int, height : Int }
        imagesList =
            List.filterMap asImage_ list

        errorsToString : List.Nonempty.Nonempty Gen.CodeGen.Generate.Error -> String
        errorsToString errors =
            errors
                |> List.Nonempty.toList
                |> List.map .description
                |> String.join ", "
    in
    BuildTask.do
        (Generate.Image.file imagesList
            |> Result.mapError (\e -> e |> errorsToString |> FatalError.fromString)
            |> BuildTask.fromResult
        )
    <| \module_ ->
    let
        elmFile : Elm.File
        elmFile =
            Elm.Declare.toFile module_
    in
    BuildTask.do (elmCodegen tools elmFile) <| \file ->
    { module_ = module_.call
    , file = file
    }
        |> BuildTask.succeed


elmCodegen :
    { tools | elm_format : BuildTask.Command }
    -> Elm.File
    -> BuildTask FatalError HashedFile
elmCodegen tools elmFile =
    case Path.parseRelativeFile elmFile.path of
        Just path ->
            { warnings = elmFile.warnings
            , contents = elmFile.contents
            , path = path
            }
                |> Elm.codegen tools
                |> BuildTask.allowFatal

        Nothing ->
            BuildTask.fail (FatalError.fromString ("Invalid path: " ++ Utils.escape elmFile.path))


processedFileToFileList :
    ProcessedFile
    ->
        List
            { filename : Path Path.Relative Path.File
            , hash : FileOrDirectory
            }
processedFileToFileList file =
    let
        extract : HashedFileWith a -> HashedFile
        extract original =
            { filename = original.filename
            , hash = original.hash
            }
    in
    case file of
        ProcessedImage image ->
            extract image.original
                :: List.map extract image.converted

        ProcessedCss data ->
            [ extract data ]

        ProcessedSvg data ->
            [ extract data ]

        ProcessedFont data ->
            [ extract data ]

        ProcessedDLC _ ->
            []


processFile :
    { config | inputPath : Path Path.Absolute Path.Directory }
    -> Example.Tools
    -> Int
    -> Int
    -> ( Path Path.Absolute Path.File, BuildTask FatalError FileOrDirectory )
    -> BuildTask FatalError (Maybe ProcessedFile)
processFile config tools total index ( path, copyFile ) =
    let
        relative : Path Path.Relative Path.File
        relative =
            Path.relativeTo config.inputPath path
                |> Path.replace " " "_"

        prefix : String
        prefix =
            "["
                ++ String.padLeft (String.length (String.fromInt total)) '0' (String.fromInt index)
                ++ "/"
                ++ String.fromInt total
                ++ "]"

        doImage : () -> BuildTask FatalError (Maybe ProcessedFile)
        doImage () =
            BuildTask.do copyFile <| \hash ->
            BuildTask.do (Example.image tools relative hash) <| \data ->
            data
                |> ProcessedImage
                |> Just
                |> BuildTask.succeed

        doSvg : () -> BuildTask FatalError (Maybe ProcessedFile)
        doSvg () =
            BuildTask.do copyFile <| \hash ->
            Do.allowFatal (Image.getSvgSize hash) <| \size ->
            { filename = relative
            , hash = hash
            , width = size.width
            , height = size.height
            }
                |> ProcessedSvg
                |> Just
                |> BuildTask.succeed

        doFont : () -> BuildTask FatalError (Maybe ProcessedFile)
        doFont () =
            BuildTask.do copyFile <| \hash ->
            Do.allowFatal (Font.parse tools hash) <| \fontData ->
            { style = fontData.style
            , weight = fontData.weight
            , family = fontData.family
            , filename = relative
            , hash = hash
            }
                |> ProcessedFont
                |> Just
                |> BuildTask.succeed

        doDlc : () -> BuildTask FatalError (Maybe ProcessedFile)
        doDlc () =
            BuildTask.do copyFile <| \hash ->
            BuildTask.withFileFatal hash <| \content ->
            case Parsers.parseDLC { path = path, content = content } of
                Ok parsed ->
                    { data = parsed
                    , filename = relative
                    , hash = hash
                    }
                        |> ProcessedDLC
                        |> Just
                        |> BuildTask.succeed

                Err e ->
                    BuildTask.fail (FatalError.fromString e)
    in
    (case Path.fileExtension path of
        Just "webp" ->
            doImage ()

        Just "jpg" ->
            doImage ()

        Just "jpeg" ->
            doImage ()

        Just "png" ->
            doImage ()

        Just "ttf" ->
            doFont ()

        Just "otf" ->
            doFont ()

        Just "svg" ->
            doSvg ()

        Just "zip" ->
            -- Ignore
            BuildTask.succeed Nothing

        Just "txt" ->
            -- Ignore
            BuildTask.succeed Nothing

        Just "md" ->
            case Path.toString (Path.filename path) of
                "attribution.md" ->
                    BuildTask.succeed Nothing

                _ ->
                    doDlc ()

        Just "css" ->
            BuildTask.do copyFile <| \hash ->
            BuildTask.succeed (Just (ProcessedCss { filename = relative, hash = hash }))

        _ ->
            -- Cache.fail ("Don't know how to process " ++ Path.toString path)
            BuildTask.succeed Nothing
    )
        |> BuildTask.timed
            ("Processing " ++ Path.toString path)
            ("Processed  " ++ Path.toString path)
        |> BuildTask.withPrefix prefix
