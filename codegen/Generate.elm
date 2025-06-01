module Generate exposing (main)

{-| -}

import Data exposing (Enum, Variant)
import Dict exposing (Dict)
import Dict.Extra
import Elm
import Elm.Annotation
import Elm.Arg
import Elm.Case
import Elm.Op
import Gen.CodeGen.Generate as Generate exposing (Directory)
import Gen.Maybe
import Gen.Parser
import Gen.Result
import Json.Decode exposing (Decoder, Value)
import Parser exposing ((|.), (|=), Parser)
import Result.Extra
import String.Extra
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
                    Generate.error errors
            )

        Err e ->
            ( ()
            , Generate.error
                [ { title = "Error decoding flags"
                  , description = Json.Decode.errorToString e
                  }
                ]
            )


toFiles :
    Directory
    -> Result (List Generate.Error) { info : List String, files : List Elm.File }
toFiles (Generate.Directory { files }) =
    files
        |> Dict.toList
        |> Result.Extra.combineMap
            (\( fileName, fileContent ) ->
                case fileName of
                    "sizes" ->
                        images fileContent
                            |> Result.map
                                (\file ->
                                    ( [ file ], [] )
                                )

                    _ ->
                        if String.endsWith "_gradient.ppm" fileName then
                            gradient (String.dropRight (String.length "_gradient.ppm") fileName) fileContent
                                |> Result.map (\declaration -> ( [], [ declaration ] ))

                        else
                            Err
                                [ { title = "Unexpected file"
                                  , description = "File " ++ fileName ++ " unexpected, don’t know how to handle it"
                                  }
                                ]
            )
        |> Result.map
            (\list ->
                let
                    gradientsFile : Elm.File
                    gradientsFile =
                        Elm.file [ "Gradients" ]
                            (List.concatMap Tuple.second list)

                    enumsFile : Elm.File
                    enumsFile =
                        Elm.file [ "Generated", "Types" ]
                            (List.map enumToDeclarations Data.enums)
                in
                { info = []
                , files = gradientsFile :: enumsFile :: List.concatMap Tuple.first list
                }
            )


enumToDeclarations : Enum -> Elm.Declaration
enumToDeclarations { name, variants, toImage } =
    let
        type_ : Elm.Annotation.Annotation
        type_ =
            Elm.Annotation.named [] name

        lowerName : String
        lowerName =
            String.Extra.decapitalize name

        typeDeclaration : Elm.Declaration
        typeDeclaration =
            Dict.toList variants
                |> List.map
                    (\( variantName, variant ) ->
                        variant.arguments
                            |> List.map (Elm.Annotation.named [])
                            |> Elm.variantWith (yassify variantName)
                    )
                |> Elm.customType name

        toStringDeclaration : Elm.Declaration
        toStringDeclaration =
            (\value ->
                let
                    variantToBranch : ( String, Variant ) -> Elm.Case.Branch
                    variantToBranch ( variantName, variant ) =
                        let
                            variantString : Elm.Expression
                            variantString =
                                variant.toStringException
                                    |> Maybe.withDefault variantName
                                    |> Elm.string

                            toStrings : List ( String, Elm.Expression ) -> Elm.Expression
                            toStrings =
                                List.foldl
                                    (\( arg, val ) acc ->
                                        Elm.Op.append
                                            (Elm.Op.append
                                                acc
                                                (Elm.string "-")
                                            )
                                            (Elm.apply
                                                (Elm.val <|
                                                    String.Extra.decapitalize arg
                                                        ++ "ToString"
                                                )
                                                [ val ]
                                            )
                                    )
                                    variantString
                        in
                        Elm.Case.branch
                            (Elm.Arg.customType (yassify variantName) identity
                                |> Elm.Arg.items (List.map Elm.Arg.var variant.arguments)
                            )
                        <|
                            \vals ->
                                toStrings (List.map2 Tuple.pair variant.arguments vals)
                in
                Dict.toList variants
                    |> List.map variantToBranch
                    |> Elm.Case.custom value type_
            )
                |> Elm.fn (Elm.Arg.varWith lowerName type_)
                |> Elm.declaration (lowerName ++ "ToString")

        parserDeclaration : Elm.Declaration
        parserDeclaration =
            variants
                |> Dict.toList
                |> List.map
                    (\( variantName, variant ) ->
                        let
                            init : Elm.Expression
                            init =
                                Elm.Op.skip
                                    (Gen.Parser.succeed (Elm.val <| yassify variantName))
                                    (variant.toStringException
                                        |> Maybe.withDefault variantName
                                        |> String.replace "\"" "\\\""
                                        |> Gen.Parser.symbol
                                    )
                        in
                        List.foldl
                            (\arg acc ->
                                Elm.Op.keep
                                    (Elm.Op.skip
                                        acc
                                        (Gen.Parser.symbol "-")
                                    )
                                    (Elm.val <| String.Extra.decapitalize arg ++ "Parser")
                            )
                            init
                            variant.arguments
                    )
                |> Gen.Parser.oneOf
                |> Elm.withType (Gen.Parser.annotation_.parser type_)
                |> Elm.declaration (lowerName ++ "Parser")

        fromStringDeclaration : Elm.Declaration
        fromStringDeclaration =
            (\value ->
                (if isEnum variants then
                    Elm.Case.string value
                        { cases =
                            List.map
                                (\( variantName, variant ) ->
                                    ( variant.toStringException
                                        |> Maybe.withDefault variantName
                                        |> String.replace "\"" "\\\""
                                    , Gen.Maybe.make_.just <| Elm.val <| yassify variantName
                                    )
                                )
                                (Dict.toList variants)
                        , otherwise = Gen.Maybe.make_.nothing
                        }

                 else
                    value
                        |> Gen.Parser.call_.run
                            (Elm.Op.skip
                                (Elm.val (lowerName ++ "Parser"))
                                Gen.Parser.end
                            )
                        |> Gen.Result.toMaybe
                )
                    |> Elm.withType (Elm.Annotation.maybe type_)
            )
                |> Elm.fn (Elm.Arg.varWith lowerName Elm.Annotation.string)
                |> Elm.declaration (lowerName ++ "FromString")

        toImageDeclaration : Elm.Declaration
        toImageDeclaration =
            (\value ->
                let
                    baseBranches : List Elm.Case.Branch
                    baseBranches =
                        variants
                            |> Dict.toList
                            |> List.map
                                (\( variantName, variant ) ->
                                    let
                                        constructor : String
                                        constructor =
                                            yassify variantName

                                        image : Elm.Expression
                                        image =
                                            Elm.value
                                                { importFrom = [ "Images" ]
                                                , name = lowerName ++ constructor
                                                , annotation = Nothing
                                                }
                                    in
                                    Elm.Case.branch
                                        (Elm.Arg.customType constructor identity
                                            |> Elm.Arg.items (List.map (always Elm.Arg.ignore) variant.arguments)
                                        )
                                    <|
                                        \_ -> image
                                )
                in
                baseBranches
                    |> Elm.Case.custom value (Elm.Annotation.named [] name)
                    |> Elm.withType (Elm.Annotation.named [ "Images" ] "Image")
            )
                |> Elm.fn (Elm.Arg.varWith lowerName <| Elm.Annotation.named [] name)
                |> Elm.declaration (lowerName ++ "ToImage")
    in
    (if toImage then
        [ typeDeclaration
        , toStringDeclaration
        , parserDeclaration
        , fromStringDeclaration
        , toImageDeclaration
        ]

     else
        [ typeDeclaration
        , toStringDeclaration
        , parserDeclaration
        , fromStringDeclaration
        ]
    )
        |> List.map Elm.exposeConstructor
        |> (::) (Elm.docs ("# " ++ name))
        |> Elm.group


isEnum : Dict String Variant -> Bool
isEnum variants =
    Dict.Extra.all (\_ variant -> List.isEmpty variant.arguments) variants


gradient :
    String
    -> String
    -> Result (List Generate.Error) Elm.Declaration
gradient name content =
    case
        content
            |> String.dropLeft 1
            |> String.lines
            |> List.map
                (\line ->
                    line
                        |> String.split " "
                        |> List.filter (\number -> not (String.isEmpty number))
                        |> List.filterMap String.toInt
                )
            |> List.filter (\line -> not (List.isEmpty line))
    of
        [ 3 ] :: [ 1, _ {- height -} ] :: [ 255 ] :: rows ->
            rows
                |> Result.Extra.combineMap
                    (\row ->
                        case row of
                            [ r, g, b ] ->
                                Ok ( r, g, b )

                            _ ->
                                Err
                                    [ { title = "Invalid row"
                                      , description =
                                            "Row \""
                                                ++ String.join " " (List.map String.fromInt row)
                                                ++ "\" is not valid"
                                      }
                                    ]
                    )
                |> Result.map
                    (\triples ->
                        triples
                            |> List.map
                                (\( r, g, b ) ->
                                    Elm.triple
                                        (Elm.int r)
                                        (Elm.int g)
                                        (Elm.int b)
                                )
                            |> Elm.list
                            |> Elm.declaration (name ++ "Gradient")
                            |> Elm.expose
                    )

        _ ->
            Err [ { title = "Invalid file", description = "Could not parse file" } ]


images : String -> Result (List Generate.Error) Elm.File
images sizes =
    let
        fromLine :
            String
            -> String
            ->
                Result
                    String
                    { name : String
                    , section : String
                    , declaration : Elm.Declaration
                    , group : Maybe { name : String, index : Int }
                    }
        fromLine filePath size =
            let
                fileName : Maybe String
                fileName =
                    filePath
                        |> String.split "/"
                        |> List.drop 1
                        |> List.concatMap (String.split ".")
                        |> List.head
            in
            case
                ( fileName
                , List.map String.toInt <| String.split "x" size
                )
            of
                ( Just name, [ Just width, Just height ] ) ->
                    let
                        declaration : Elm.Declaration
                        declaration =
                            [ ( "width", Elm.int width )
                            , ( "height", Elm.int height )
                            , ( "src", Elm.string filePath )
                            ]
                                |> Elm.record
                                |> Elm.withType (Elm.Annotation.named [] "Image")
                                |> Elm.declaration name

                        section : String
                        section =
                            name
                                |> String.Extra.humanize
                                |> String.split " "
                                |> List.take 1
                                |> String.join " "
                    in
                    { name = name
                    , section = section
                    , declaration = declaration
                    , group =
                        Parser.run imageGroupParser name
                            |> Result.toMaybe
                            |> Maybe.map
                                (\( groupName, indexInGroup ) ->
                                    { name = groupName
                                    , index = indexInGroup
                                    }
                                )
                    }
                        |> Ok

                _ ->
                    Err <| "Unexpected size: " ++ size
    in
    sizes
        |> String.lines
        |> List.filter (\line -> not (String.isEmpty line))
        |> Result.Extra.combineMap
            (\line ->
                case String.split " " line of
                    filePath :: "PNG" :: size :: _ ->
                        fromLine filePath size

                    filePath :: "JPEG" :: size :: _ ->
                        fromLine filePath size

                    filePath :: "WEBP" :: size :: _ ->
                        fromLine filePath size

                    _ ->
                        Err <| "Wrong line: " ++ line
            )
        |> Result.mapError
            (\e ->
                [ { title = "Error"
                  , description = e
                  }
                ]
            )
        |> Result.map
            (\declarations ->
                let
                    groupedDeclarations : List Elm.Declaration
                    groupedDeclarations =
                        declarations
                            |> Dict.Extra.groupBy .section
                            |> Dict.map
                                (\section decls ->
                                    Elm.group (Elm.docs ("## " ++ section) :: List.map .declaration decls)
                                )
                            |> Dict.values

                    declarationsForGroups : Elm.Declaration
                    declarationsForGroups =
                        declarations
                            |> List.filterMap
                                (\{ name, group } ->
                                    Maybe.map (\g -> ( name, g.name, g.index )) group
                                )
                            |> Dict.Extra.groupBy Triple.Extra.second
                            |> Dict.map groupDeclaration
                            |> Dict.values
                            |> (::) (Elm.docs "## Groups")
                            |> Elm.group

                    groupDeclaration : String -> List ( String, String, Int ) -> Elm.Declaration
                    groupDeclaration groupName decls =
                        decls
                            |> List.map
                                (\( name, _, index ) ->
                                    ( "image" ++ String.fromInt index
                                    , Elm.val name
                                        |> Elm.withType
                                            (Elm.Annotation.named [] "Image")
                                    )
                                )
                            |> Elm.record
                            |> Elm.declaration groupName
                in
                (Elm.alias "Image"
                    (Elm.Annotation.record
                        [ ( "width", Elm.Annotation.int )
                        , ( "height", Elm.Annotation.int )
                        , ( "src", Elm.Annotation.string )
                        ]
                    )
                    :: groupedDeclarations
                    ++ [ declarationsForGroups ]
                )
                    |> List.map Elm.expose
                    |> Elm.file [ "Images" ]
            )


imageGroupParser : Parser ( String, Int )
imageGroupParser =
    Parser.succeed Tuple.pair
        |= Parser.getChompedString
            (Parser.chompIf Char.isAlpha
                |. Parser.chompWhile Char.isAlpha
            )
        |= Parser.int
        |. Parser.end


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


yassify : String -> String
yassify str =
    str
        |> String.replace "Æ" "Ae"
        |> String.replace "æ" "ae"
        |> String.replace "\"" ""
        |> String.Extra.classify
