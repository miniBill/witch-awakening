module Generate.Enums exposing (enumToDeclarations)

import Data exposing (Enum, Variant)
import Elm
import Elm.Annotation
import Elm.Arg
import Elm.Case
import Elm.Op
import Gen.Maybe
import Gen.Parser
import Gen.Result
import Generate.Utils exposing (yassify)
import String.Extra


enumToDeclarations : Enum -> Elm.Declaration
enumToDeclarations enum =
    let
        config : Config
        config =
            { lowerName = String.Extra.decapitalize enum.name
            , type_ = Elm.Annotation.named [] enum.name
            }
    in
    (if enum.toImage then
        [ typeDeclaration enum
        , toStringDeclaration config enum
        , parserDeclaration config enum
        , fromStringDeclaration config enum
        , toImageDeclaration config enum
        ]

     else
        [ typeDeclaration enum
        , toStringDeclaration config enum
        , parserDeclaration config enum
        , fromStringDeclaration config enum
        ]
    )
        |> List.map Elm.exposeConstructor
        |> (::) (Elm.docs ("# " ++ enum.name))
        |> Elm.group


isEnum : List Variant -> Bool
isEnum variants =
    List.all (\variant -> List.isEmpty variant.arguments) variants


type alias Config =
    { lowerName : String
    , type_ : Elm.Annotation.Annotation
    }


parserDeclaration : Config -> Enum -> Elm.Declaration
parserDeclaration { lowerName, type_ } { variants } =
    variants
        |> List.map
            (\variant ->
                let
                    start : Elm.Expression
                    start =
                        Elm.Op.skip
                            (Gen.Parser.succeed (Elm.val <| yassify variant.name))
                            (variant.toStringException
                                |> Maybe.withDefault variant.name
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
                    start
                    variant.arguments
            )
        |> Gen.Parser.oneOf
        |> Elm.withType (Gen.Parser.annotation_.parser type_)
        |> Elm.declaration (lowerName ++ "Parser")


toImageDeclaration : Config -> Enum -> Elm.Declaration
toImageDeclaration { lowerName } { name, variants } =
    (\value ->
        variants
            |> List.map
                (\variant ->
                    let
                        constructor : String
                        constructor =
                            yassify variant.name

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
            |> Elm.Case.custom value (Elm.Annotation.named [] name)
            |> Elm.withType (Elm.Annotation.named [ "Images" ] "Image")
    )
        |> Elm.fn (Elm.Arg.varWith lowerName <| Elm.Annotation.named [] name)
        |> Elm.declaration (lowerName ++ "ToImage")


typeDeclaration : Enum -> Elm.Declaration
typeDeclaration { name, variants } =
    variants
        |> List.map
            (\variant ->
                variant.arguments
                    |> List.map (Elm.Annotation.named [])
                    |> Elm.variantWith (yassify variant.name)
            )
        |> Elm.customType name


toStringDeclaration : Config -> Enum -> Elm.Declaration
toStringDeclaration { lowerName, type_ } { variants } =
    (\value ->
        let
            variantToBranch : Variant -> Elm.Case.Branch
            variantToBranch variant =
                let
                    variantString : Elm.Expression
                    variantString =
                        variant.toStringException
                            |> Maybe.withDefault variant.name
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
                    (Elm.Arg.customType (yassify variant.name) identity
                        |> Elm.Arg.items (List.map Elm.Arg.var variant.arguments)
                    )
                <|
                    \vals ->
                        toStrings (List.map2 Tuple.pair variant.arguments vals)
        in
        variants
            |> List.map variantToBranch
            |> Elm.Case.custom value type_
    )
        |> Elm.fn (Elm.Arg.varWith lowerName type_)
        |> Elm.declaration (lowerName ++ "ToString")


fromStringDeclaration : Config -> Enum -> Elm.Declaration
fromStringDeclaration { lowerName, type_ } { variants } =
    (\value ->
        (if isEnum variants then
            Elm.Case.string value
                { cases =
                    List.map
                        (\variant ->
                            ( variant.toStringException
                                |> Maybe.withDefault variant.name
                                |> String.replace "\"" "\\\""
                            , Gen.Maybe.make_.just <| Elm.val <| yassify variant.name
                            )
                        )
                        variants
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
