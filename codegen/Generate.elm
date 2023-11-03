module Generate exposing (main)

{-| -}

import Data exposing (Enum)
import Dict exposing (Dict)
import Elm
import Elm.Annotation
import Elm.Case
import Gen.CodeGen.Generate as Generate
import Gen.Maybe
import Json.Decode exposing (Decoder, Value)
import List.Extra
import Parser exposing ((|.), (|=), Parser)
import Result.Extra
import String.Extra


main : Program Value () ()
main =
    Generate.withFeedback toFiles


toFiles :
    Value
    ->
        Result
            (List Generate.Error)
            { info : List String, files : List Elm.File }
toFiles flags =
    case Json.Decode.decodeValue directoryDecoder flags of
        Err _ ->
            Err [ { title = "Invalid flags", description = "Could not decode flags" } ]

        Ok (Generate.Directory { files }) ->
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
                                    (List.concatMap enumToDeclarations Data.enums)
                        in
                        { info = []
                        , files = gradientsFile :: enumsFile :: List.concatMap Tuple.first list
                        }
                    )


enumToDeclarations : Enum -> List Elm.Declaration
enumToDeclarations { name, exceptions, variants, toImage } =
    let
        type_ : Elm.Annotation.Annotation
        type_ =
            Elm.Annotation.named [] name

        exceptionsDict : Dict String String
        exceptionsDict =
            Dict.fromList exceptions

        lowerName : String
        lowerName =
            String.Extra.decapitalize name

        typeDeclaration : Elm.Declaration
        typeDeclaration =
            Elm.customType name
                (List.map
                    (\variant -> Elm.variant <| yassify variant)
                    variants
                )

        toStringDeclaration : Elm.Declaration
        toStringDeclaration =
            (\value ->
                Elm.Case.custom value
                    type_
                    (List.map
                        (\variant ->
                            Dict.get variant exceptionsDict
                                |> Maybe.withDefault variant
                                |> Elm.string
                                |> Elm.Case.branch0 (yassify variant)
                        )
                        variants
                    )
            )
                |> Elm.fn ( lowerName, Just type_ )
                |> Elm.declaration (lowerName ++ "ToString")

        fromStringDeclaration : Elm.Declaration
        fromStringDeclaration =
            (\value ->
                Elm.Case.string value
                    { cases =
                        List.map
                            (\variant ->
                                ( Dict.get variant exceptionsDict
                                    |> Maybe.withDefault variant
                                    |> String.replace "\"" "\\\""
                                , Gen.Maybe.make_.just <| Elm.val <| yassify variant
                                )
                            )
                            variants
                    , otherwise = Gen.Maybe.make_.nothing
                    }
                    |> Elm.withType (Elm.Annotation.maybe type_)
            )
                |> Elm.fn ( lowerName, Just Elm.Annotation.string )
                |> Elm.declaration (lowerName ++ "FromString")

        toImageDeclaration : Elm.Declaration
        toImageDeclaration =
            (\value ->
                variants
                    |> List.map
                        (\variant ->
                            let
                                constructor : String
                                constructor =
                                    yassify variant
                            in
                            Elm.Case.branch0 constructor
                                (Elm.value
                                    { importFrom = [ "Images" ]
                                    , name =
                                        (lowerName ++ constructor)
                                            |> String.replace "XiaoLiena" "XiaoLiena肖列娜"
                                    , annotation =
                                        Just
                                            (Elm.Annotation.named [ "Images" ] "Image")
                                    }
                                )
                        )
                    |> Elm.Case.custom value (Elm.Annotation.named [] name)
            )
                |> Elm.fn
                    ( lowerName
                    , Just <| Elm.Annotation.named [] name
                    )
                |> Elm.declaration (lowerName ++ "ToImage")
                |> Elm.exposeWith { group = Just name, exposeConstructor = False }
    in
    (if toImage then
        [ typeDeclaration
        , toStringDeclaration
        , fromStringDeclaration
        , toImageDeclaration
        ]

     else
        [ typeDeclaration
        , toStringDeclaration
        , fromStringDeclaration
        ]
    )
        |> List.map
            (Elm.exposeWith
                { exposeConstructor = True
                , group = Just name
                }
            )


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
        fromLine : String -> String -> Result String ( Elm.Declaration, Maybe ( String, Int, String ) )
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
                                |> Elm.exposeWith
                                    { exposeConstructor = True
                                    , group = Just group
                                    }

                        group : String
                        group =
                            name
                                |> String.Extra.humanize
                                |> String.split " "
                                |> List.take 1
                                |> String.join " "
                    in
                    case Parser.run imageGroupParser name of
                        Ok ( groupName, index ) ->
                            Ok ( declaration, Just ( groupName, index, name ) )

                        Err _ ->
                            Ok ( declaration, Nothing )

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
                    groupDeclarations : List Elm.Declaration
                    groupDeclarations =
                        declarations
                            |> List.filterMap Tuple.second
                            |> List.Extra.gatherEqualsBy
                                (\( groupName, _, _ ) -> groupName)
                            |> List.map groupDeclaration

                    groupDeclaration :
                        ( ( String, Int, String )
                        , List ( String, Int, String )
                        )
                        -> Elm.Declaration
                    groupDeclaration ( ( groupName, _, _ ) as head, tail ) =
                        (head :: tail)
                            |> List.map
                                (\( _, imageIndex, imageName ) ->
                                    ( imageIndex, imageName )
                                )
                            |> List.map
                                (\( index, name ) ->
                                    ( "image" ++ String.fromInt index
                                    , Elm.val name
                                        |> Elm.withType
                                            (Elm.Annotation.named [] "Image")
                                    )
                                )
                            |> Elm.record
                            |> Elm.declaration groupName
                            |> Elm.exposeWith
                                { exposeConstructor = True
                                , group = Just "Groups"
                                }
                in
                Elm.file [ "Images" ]
                    (Elm.expose
                        (Elm.alias "Image"
                            (Elm.Annotation.record
                                [ ( "width", Elm.Annotation.int )
                                , ( "height", Elm.Annotation.int )
                                , ( "src", Elm.Annotation.string )
                                ]
                            )
                        )
                        :: List.map Tuple.first declarations
                        ++ groupDeclarations
                    )
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
