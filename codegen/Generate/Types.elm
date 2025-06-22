module Generate.Types exposing (TypesModule, file)

import Data exposing (Enum, Variant)
import Elm
import Elm.Annotation
import Elm.Arg
import Elm.Case
import Elm.Declare
import Elm.Declare.Extra
import Elm.Op
import Elm.Op.Extra
import Gen.Maybe
import Gen.Parser
import Gen.Result
import Generate.Images exposing (ImagesModule)
import Generate.Utils exposing (yassify)
import Parsers
import String.Extra


type alias TypesModule =
    { valueFrom : String -> Elm.Expression
    , affinity : EnumModule
    , class : EnumModule
    , faction : EnumModule
    , race : EnumModule
    , relic : EnumModule
    }


file : ImagesModule -> List Parsers.DLC -> Elm.Declare.Module TypesModule
file images dlcList =
    let
        moduleName : List String
        moduleName =
            [ "Generated", "Types" ]

        enums : Data.Enums
        enums =
            Data.enums dlcList

        valueFrom : String -> Elm.Expression
        valueFrom name =
            Elm.value
                { importFrom = moduleName
                , name = yassify name
                , annotation = Nothing
                }

        module_ : Elm.Declare.Module TypesModule
        module_ =
            Elm.Declare.module_ moduleName (TypesModule valueFrom)
                |> Elm.Declare.withSubmodule (enumToDeclarations moduleName images enums.affinity)
                |> Elm.Declare.withSubmodule (enumToDeclarations moduleName images enums.class)
                |> Elm.Declare.withSubmodule (enumToDeclarations moduleName images enums.faction)
                |> Elm.Declare.withSubmodule (enumToDeclarations moduleName images enums.race)
                |> Elm.Declare.withSubmodule (enumToDeclarations moduleName images enums.relic)
                |> Elm.Declare.Extra.withDeclarations
                    (List.concatMap
                        (\enum -> (enumToDeclarations moduleName images enum).declarations)
                        enums.others
                    )
    in
    { name = module_.name
    , declarations = module_.declarations
    , call = module_.call
    }


type alias EnumModule =
    { annotation : Elm.Annotation.Annotation
    , toString : Elm.Expression -> Elm.Expression
    , parser : Elm.Expression
    , fromString : Elm.Expression -> Elm.Expression
    , argWith : String -> List (Elm.Arg Elm.Expression) -> Elm.Arg (List Elm.Expression)
    }


enumToDeclarations : List String -> ImagesModule -> Enum -> Elm.Declare.Module EnumModule
enumToDeclarations moduleName images enum =
    let
        type_ : Elm.Declare.Annotation
        type_ =
            typeDeclaration enum

        config : Config
        config =
            { lowerName = String.Extra.decapitalize enum.name
            , type_ = type_.annotation
            }

        argWith : String -> List (Elm.Arg a) -> Elm.Arg (List a)
        argWith variantName args =
            Elm.Arg.customTypeWith
                { importFrom = moduleName
                , typeName = enum.name
                , variantName = variantName
                }
                identity
                |> Elm.Arg.items args

        common : Elm.Declare.Module EnumModule
        common =
            let
                inner : Elm.Declare.Module ((String -> List (Elm.Arg Elm.Expression) -> Elm.Arg (List Elm.Expression)) -> EnumModule)
                inner =
                    Elm.Declare.module_ moduleName EnumModule
                        |> Elm.Declare.Extra.withDeclarations [ Elm.docs ("# " ++ enum.name) ]
                        |> Elm.Declare.with type_
                        |> Elm.Declare.with (toStringDeclaration config enum)
                        |> Elm.Declare.with (parserDeclaration config enum)
                        |> Elm.Declare.with (fromStringDeclaration config enum)
            in
            { name = inner.name
            , declarations = inner.declarations
            , call = inner.call argWith
            }

        withToImage =
            if enum.toImage then
                common
                    |> Elm.Declare.Extra.withDeclarations
                        [ (toImageDeclaration images config enum).declaration
                            |> Elm.expose
                        ]

            else
                common
    in
    if enum.isSame then
        withToImage
            |> Elm.Declare.Extra.withDeclarations
                [ (isSameDeclaration config enum).declaration
                    |> Elm.expose
                ]

    else
        withToImage


isEnum : List Variant -> Bool
isEnum variants =
    List.all (\variant -> List.isEmpty variant.arguments) variants


type alias Config =
    { lowerName : String
    , type_ : Elm.Annotation.Annotation
    }


parserDeclaration : Config -> Enum -> Elm.Declare.Value
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
        |> Elm.Declare.value (lowerName ++ "Parser")


toImageDeclaration : ImagesModule -> Config -> Enum -> Elm.Declare.Function (Elm.Expression -> Elm.Expression)
toImageDeclaration images { lowerName, type_ } { name, variants } =
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
                            Generate.Images.valueFrom (lowerName ++ constructor)
                    in
                    Elm.Case.branch
                        (Elm.Arg.customType constructor identity
                            |> Elm.Arg.items (List.map (always Elm.Arg.ignore) variant.arguments)
                        )
                    <|
                        \_ -> image
                )
            |> Elm.Case.custom value type_
            |> Elm.withType images.image
    )
        |> Elm.Declare.fn (lowerName ++ "ToImage") (Elm.Arg.varWith lowerName <| type_)


isSameDeclaration : Config -> Enum -> Elm.Declare.Function (Elm.Expression -> Elm.Expression -> Elm.Expression)
isSameDeclaration { lowerName, type_ } { variants } =
    (\var1 var2 ->
        let
            branchesForVariants : List Elm.Case.Branch
            branchesForVariants =
                List.map variantToBranch variants

            variantToBranch : Variant -> Elm.Case.Branch
            variantToBranch variant =
                let
                    constructor : String
                    constructor =
                        yassify variant.name
                in
                Elm.Case.branch
                    (Elm.Arg.tuple
                        (Elm.Arg.customType constructor identity
                            |> Elm.Arg.items (List.map (always Elm.Arg.ignore) variant.arguments)
                        )
                        (Elm.Arg.customType constructor identity
                            |> Elm.Arg.items (List.map (always Elm.Arg.ignore) variant.arguments)
                        )
                    )
                <|
                    \_ -> Elm.bool True
        in
        (branchesForVariants
            ++ [ Elm.Case.branch Elm.Arg.ignore (\_ -> Elm.bool False) ]
        )
            |> Elm.Case.custom (Elm.tuple var1 var2) type_
            |> Elm.withType Elm.Annotation.bool
    )
        |> Elm.Declare.fn2 ("isSame" ++ String.Extra.toSentenceCase lowerName)
            (Elm.Arg.varWith (lowerName ++ "1") type_)
            (Elm.Arg.varWith (lowerName ++ "2") type_)


typeDeclaration : Enum -> Elm.Declare.Annotation
typeDeclaration { name, variants } =
    variants
        |> List.map
            (\variant ->
                variant.arguments
                    |> List.map (Elm.Annotation.named [])
                    |> Elm.variantWith (yassify variant.name)
            )
        |> Elm.Declare.customType name
        |> Elm.Declare.Extra.exposeConstructor


toStringDeclaration : Config -> Enum -> Elm.Declare.Function (Elm.Expression -> Elm.Expression)
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
                in
                Elm.Case.branch
                    (Elm.Arg.customType (yassify variant.name) identity
                        |> Elm.Arg.items (List.map Elm.Arg.var variant.arguments)
                    )
                <|
                    \vals ->
                        (variantString
                            :: List.map2
                                (\arg val ->
                                    Elm.apply
                                        (Elm.val <|
                                            String.Extra.decapitalize arg
                                                ++ "ToString"
                                        )
                                        [ val ]
                                )
                                variant.arguments
                                vals
                        )
                            |> List.intersperse (Elm.string "-")
                            |> Elm.Op.Extra.appendsStrings
        in
        variants
            |> List.map variantToBranch
            |> Elm.Case.custom value type_
    )
        |> Elm.Declare.fn (lowerName ++ "ToString") (Elm.Arg.varWith lowerName type_)


fromStringDeclaration : Config -> Enum -> Elm.Declare.Function (Elm.Expression -> Elm.Expression)
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
        |> Elm.Declare.fn (lowerName ++ "FromString") (Elm.Arg.varWith lowerName Elm.Annotation.string)
