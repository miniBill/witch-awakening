module WitchAwakeningBuildfile exposing (buildAction, getInputs)

import BackendTask exposing (BackendTask)
import BackendTask.Glob as Glob
import BuildTask exposing (BuildTask, FileOrDirectory)
import BuildTask.Do as Do
import BuildTask.Elm as Elm
import BuildTask.Font as Font
import BuildTask.Image as Image
import BuildTask.Unsafe as Unsafe
import BuildTask.Unsafe.Do
import Buildfile
import Elm
import Elm.Annotation
import Elm.Arg
import Elm.Declare
import Elm.Let
import Elm.Op
import Elm.Op.Extra
import FatalError exposing (FatalError)
import Gen.Html
import Gen.Html.Attributes
import Gen.Html.Picture
import Gen.Html.Source
import Gen.List
import Gen.String
import Generate.Gradient
import List.Extra
import List.Nonempty
import Maybe.Extra
import Path exposing (Path)
import String.Extra


type ProcessedFile
    = ProcessedImage
        { original : HashedFileWith { width : Int, height : Int }
        , converted : List (HashedFileWith { width : Int })
        }
    | ProcessedCss (HashedFileWith {})
    | ProcessedSvg (HashedFileWith { width : Int, height : Int })
    | ProcessedFont (HashedFileWith Font.Data)


type alias HashedFileWith a =
    { a
        | filename : Path
        , hash : FileOrDirectory
    }


type alias Inputs =
    { images : List ( Path, BuildTask FileOrDirectory )
    , gradients : List ( Path, BuildTask FileOrDirectory )
    }


getInputs : { a | inputDirectory : Path } -> BackendTask FatalError Inputs
getInputs config =
    Glob.fromStringWithOptions
        (let
            defaultOptions : Glob.Options
            defaultOptions =
                Glob.defaultOptions
         in
         { defaultOptions | include = Glob.OnlyFiles }
        )
        (Path.toString config.inputDirectory ++ "/**")
        |> BackendTask.andThen
            (\found ->
                let
                    ( gradients, notGradients ) =
                        found
                            |> List.sort
                            |> Debug.log "all"
                            |> List.partition (String.endsWith Generate.Gradient.suffix)
                in
                BackendTask.map2 Inputs
                    (notGradients
                        |> List.Extra.removeWhen
                            (\p ->
                                String.contains "/raw/" p
                                    || String.contains "/originals/" p
                            )
                        |> List.map Path.path
                        |> BuildTask.inputs
                    )
                    (gradients
                        |> List.map Path.path
                        |> BuildTask.inputs
                    )
            )


type T4 a b c d
    = T4 a b c d


buildAction : { config | inputDirectory : Path } -> Inputs -> BuildTask FileOrDirectory
buildAction config inputs =
    BuildTask.andThen2
        (\gradients i ->
            BuildTask.combineInto
                ((gradients :: i.generated)
                    ++ i.other
                )
        )
        (buildGradients config inputs.gradients)
        (buildImages config inputs.images)


buildGradients : { config | inputDirectory : Path } -> List ( Path, BuildTask FileOrDirectory ) -> BuildTask { filename : Path, hash : FileOrDirectory }
buildGradients config inputs =
    Do.all
        (\( path, file ) ->
            BuildTask.do file <| \gradientPng ->
            BuildTask.Unsafe.Do.pipeThrough "magick" [ "-", "-compress", "none", "ppm:-" ] gradientPng <| \gradientPpm ->
            BuildTask.withFile gradientPpm <| \content ->
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
                        |> BuildTask.fail
        )
        inputs
    <| \declarations ->
    Elm.codegen (Elm.file [ "Gradient" ] declarations)


buildImages :
    { config | inputDirectory : Path }
    -> List ( Path, BuildTask FileOrDirectory )
    ->
        BuildTask
            { generated : List { filename : Path, hash : FileOrDirectory }
            , other : List { filename : Path, hash : FileOrDirectory }
            }
buildImages config inputs =
    let
        inputSize : Int
        inputSize =
            List.length inputs
    in
    BuildTask.do
        (Do.jobs <| \parallelism ->
        inputs
            |> List.indexedMap (processFile config inputSize)
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

        publicFolder : BuildTask FileOrDirectory
        publicFolder =
            Do.writeFile (Font.toCssFile fontFiles) <| \fontsCssHash ->
            ({ filename = Path.path "fonts.css"
             , hash = fontsCssHash
             }
                :: List.concatMap processedFileToFileList processedFiles
            )
                |> BuildTask.combineInto
                |> BuildTask.withPrefix ("[" ++ String.fromInt inputSize ++ "/" ++ String.fromInt inputSize ++ "]")
    in
    BuildTask.map4
        (\imagesElm fontsElm imageSizes public ->
            { generated = [ imagesElm, fontsElm ]
            , other =
                [ { filename = Path.path "image-sizes", hash = imageSizes }
                , { filename = Path.path "public", hash = public }
                ]
            }
        )
        (imagesElmFile processedFiles)
        (Elm.codegen (fontsElmFile fontFiles))
        (imagesSizesFile imageFiles)
        publicFolder


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


imagesSizesFile :
    List
        { a
            | original : HashedFileWith { width : Int, height : Int }
        }
    -> BuildTask FileOrDirectory
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


fontsElmFile : List (HashedFileWith Font.Data) -> Elm.File
fontsElmFile files =
    files
        |> List.map .family
        |> List.Extra.unique
        |> List.map
            (\family ->
                Elm.declaration (String.replace " " "_" family) (Gen.Html.Attributes.style "font-family" family)
            )
        |> Elm.file [ "Fonts" ]


imagesElmFile : List ProcessedFile -> BuildTask { filename : Path, hash : FileOrDirectory }
imagesElmFile list =
    list
        |> List.filterMap
            (\processedFile ->
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
            )
        |> Unsafe.named "imagesElmFile"
            encodeProcessedFiles
            (\processedFiles ->
                let
                    file : Elm.File
                    file =
                        processedFiles
                            |> List.map
                                (\processedFile ->
                                    if processedFile.svg then
                                        processedSvgToDeclaration processedFile

                                    else
                                        processedImageToDeclaration processedFile
                                )
                            |> (::) Buildfile.standardFormats.declaration
                            |> (::) getSizesDeclaration.declaration
                            |> (::) toSources.declaration
                            |> (::) (toPicture.declaration |> Elm.expose)
                            |> Elm.file [ "Images" ]
                in
                Do.writeFile file.contents <| \hash ->
                Elm.format hash
            )
        |> BuildTask.map (\hash -> { filename = Path.path "generated/Images.elm", hash = hash })


encodeProcessedFiles :
    List
        { svg : Bool
        , filename : Path
        , hash : FileOrDirectory
        , width : Int
        , height : Int
        }
    -> { files : List FileOrDirectory, additionalData : List String }
encodeProcessedFiles processedFiles =
    let
        ( files, additionalData ) =
            processedFiles
                |> List.map
                    (\{ filename, hash } ->
                        -- CORRECTNESS: if svg, width or height change then hash
                        -- will change too so we don't need to track them separately
                        ( hash, Path.toString filename )
                    )
                |> List.unzip
    in
    { files = files, additionalData = additionalData }


processedFileToFileList : ProcessedFile -> List { filename : Path, hash : FileOrDirectory }
processedFileToFileList file =
    let
        extract : HashedFileWith a -> HashedFileWith {}
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


processFile : { config | inputDirectory : Path } -> Int -> Int -> ( Path, BuildTask FileOrDirectory ) -> BuildTask (Maybe ProcessedFile)
processFile config total index ( path, copyFile ) =
    let
        relative : Path
        relative =
            Path.relativeTo config.inputDirectory path
                |> Path.replaceAll " " "_"

        prefix : String
        prefix =
            "["
                ++ String.padLeft (String.length (String.fromInt total)) '0' (String.fromInt index)
                ++ "/"
                ++ String.fromInt total
                ++ "]"

        doImage : () -> BuildTask (Maybe ProcessedFile)
        doImage () =
            BuildTask.do copyFile <| \hash ->
            BuildTask.do (Buildfile.image relative hash) <| \data ->
            data
                |> ProcessedImage
                |> Just
                |> BuildTask.succeed

        doSvg : () -> BuildTask (Maybe ProcessedFile)
        doSvg () =
            BuildTask.do copyFile <| \hash ->
            BuildTask.do (Image.getSvgSize hash) <| \size ->
            { filename = relative
            , hash = hash
            , width = size.width
            , height = size.height
            }
                |> ProcessedSvg
                |> Just
                |> BuildTask.succeed

        doFont : () -> BuildTask (Maybe ProcessedFile)
        doFont () =
            BuildTask.do copyFile <| \hash ->
            BuildTask.do (Font.parse hash) <| \fontData ->
            { style = fontData.style
            , weight = fontData.weight
            , family = fontData.family
            , filename = relative
            , hash = hash
            }
                |> ProcessedFont
                |> Just
                |> BuildTask.succeed
    in
    (case Path.extension path of
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
            -- Ignore
            BuildTask.succeed Nothing

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


minSize : number
minSize =
    50


getSizesDeclaration : Elm.Declare.Function (Elm.Expression -> Elm.Expression)
getSizesDeclaration =
    Elm.Declare.fn "getSizes" (Elm.Arg.varWith "width" Elm.Annotation.int) <| \width ->
    Elm.Let.letIn identity
        |> Elm.Let.fn2 "go"
            (Elm.Arg.varWith "factor" Elm.Annotation.int)
            (Elm.Arg.varWith "acc" (Elm.Annotation.list Elm.Annotation.int))
            (\factor acc ->
                Elm.Let.letIn identity
                    |> Elm.Let.value "w" (Elm.Op.intDivide width factor)
                    |> Elm.Let.withBody
                        (\w ->
                            Elm.ifThen (Elm.Op.gte w (Elm.int minSize))
                                (Elm.apply (Elm.val "go") [ Elm.Op.multiply factor (Elm.int 2), Elm.Op.cons w acc ])
                                (Gen.List.call_.reverse acc)
                        )
                    |> Elm.withType (Elm.Annotation.list Elm.Annotation.int)
            )
        |> Elm.Let.withBody
            (\go ->
                go (Elm.int 1) (Elm.list [])
                    |> Elm.withType (Elm.Annotation.list Elm.Annotation.int)
            )


processedImageToDeclaration :
    HashedFileWith { a | width : Int, height : Int }
    -> Elm.Declaration
processedImageToDeclaration original =
    let
        name : String
        name =
            toVariableName original.filename

        attrsAnnotation : Elm.Annotation.Annotation
        attrsAnnotation =
            Elm.Annotation.list (Gen.Html.annotation_.attribute (Elm.Annotation.var "msg"))
    in
    Elm.declaration name
        (Elm.fn
            (Elm.Arg.varWith "attrs" attrsAnnotation)
            (\attrs ->
                let
                    dir : String
                    dir =
                        Path.toString (Path.directory original.filename)
                in
                toPicture.call attrs
                    (Elm.string
                        (if String.isEmpty dir then
                            Path.filenameWithoutExtension original.filename

                         else
                            dir ++ "/" ++ Path.filenameWithoutExtension original.filename
                        )
                    )
                    (Path.extension original.filename
                        |> Maybe.withDefault ""
                        |> Elm.string
                    )
                    (Elm.int original.width)
                    (Elm.int original.height)
            )
            |> Elm.withType Elm.Annotation.unit
            |> Elm.withType
                (Elm.Annotation.function
                    [ attrsAnnotation ]
                    (Gen.Html.annotation_.html (Elm.Annotation.var "msg"))
                )
        )
        |> Elm.expose


processedSvgToDeclaration :
    HashedFileWith { a | width : Int, height : Int }
    -> Elm.Declaration
processedSvgToDeclaration original =
    let
        name : String
        name =
            toVariableName original.filename

        attrsAnnotation : Elm.Annotation.Annotation
        attrsAnnotation =
            Elm.Annotation.list (Gen.Html.annotation_.attribute (Elm.Annotation.var "msg"))
    in
    Elm.declaration name
        (Elm.fn
            (Elm.Arg.varWith "attrs" attrsAnnotation)
            (\attrs ->
                Gen.Html.call_.img
                    (Elm.Op.cons (Gen.Html.Attributes.src (Path.toString original.filename))
                        (Elm.Op.cons (Gen.Html.Attributes.width original.width)
                            (Elm.Op.cons (Gen.Html.Attributes.height original.height)
                                attrs
                            )
                        )
                    )
                    (Elm.list [])
            )
            |> Elm.withType
                (Elm.Annotation.function
                    [ attrsAnnotation ]
                    (Gen.Html.annotation_.html (Elm.Annotation.var "msg"))
                )
        )
        |> Elm.expose


toVariableName : Path -> String
toVariableName path =
    let
        dir : String
        dir =
            Path.toString (Path.directory path)

        nameWithoutExtension : String
        nameWithoutExtension =
            if String.isEmpty dir then
                Path.filenameWithoutExtension path

            else
                dir ++ "/" ++ Path.filenameWithoutExtension path

        stripLeadingUnderscores : String -> String
        stripLeadingUnderscores i =
            if String.startsWith "_" i then
                stripLeadingUnderscores (String.dropLeft 1 i)

            else
                i
    in
    nameWithoutExtension
        |> String.replace "/" "_"
        |> String.replace " " "_"
        |> String.replace "-" "_"
        |> stripLeadingUnderscores
        |> String.Extra.decapitalize


toSources : Elm.Declare.Function (Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression)
toSources =
    Elm.Declare.fn3 "toSources"
        (Elm.Arg.varWith "base" Elm.Annotation.string)
        (Elm.Arg.varWith "originalWidth" Elm.Annotation.int)
        (Elm.Arg.varWith "config"
            (Elm.Annotation.record
                [ ( "extension", Elm.Annotation.string )
                , ( "format", Gen.Html.Source.annotation_.imageType )
                ]
            )
        )
    <| \base originalWidth config ->
    getSizesDeclaration.call originalWidth
        |> Gen.List.call_.map
            (Elm.fn (Elm.Arg.varWith "w" Elm.Annotation.int) <| \w ->
            Elm.record
                [ ( "url"
                  , Elm.Op.Extra.appendStrings
                        [ base
                        , Elm.string "-"
                        , Gen.String.call_.fromInt w
                        , Elm.string "."
                        , Elm.get "extension" config
                        ]
                  )
                , ( "width", Elm.maybe (Just w) )
                ]
            )
        |> Gen.Html.Source.call_.fromImagesAndWidths
        |> Gen.Html.Source.withType (Elm.get "format" config)
        |> Elm.withType (Gen.Html.Source.annotation_.source Gen.Html.Source.annotation_.withWidths)


toPicture : Elm.Declare.Function (Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression)
toPicture =
    Elm.Declare.fn5 "toPicture"
        (Elm.Arg.varWith "attrs"
            (Elm.Annotation.list (Gen.Html.annotation_.attribute (Elm.Annotation.var "msg")))
        )
        (Elm.Arg.varWith "base" Elm.Annotation.string)
        (Elm.Arg.varWith "originalExtension" Elm.Annotation.string)
        (Elm.Arg.varWith "originalWidth" Elm.Annotation.int)
        (Elm.Arg.varWith "originalHeight" Elm.Annotation.int)
    <| \attrs base originalExtension originalWidth originalHeight ->
    Gen.Html.Picture.call_.picture
        (Elm.Op.cons
            (Gen.Html.Attributes.call_.width originalWidth)
            (Elm.Op.cons
                (Gen.Html.Attributes.call_.height originalHeight)
                attrs
            )
        )
        (Elm.record
            [ ( "sources"
              , Buildfile.standardFormats.value
                    |> Gen.List.call_.map
                        (Elm.functionReduced "format"
                            (toSources.call
                                base
                                originalWidth
                            )
                        )
              )
            , ( "src", Elm.Op.append (Elm.Op.append base (Elm.string ".")) originalExtension )
            , ( "alt", Elm.maybe Nothing )
            ]
        )
        |> Elm.withType (Gen.Html.annotation_.html (Elm.Annotation.var "msg"))
