module Generate.Types exposing (TypesModule, file)

import Data exposing (Enums)
import Elm
import Elm.Annotation
import Elm.Arg
import Elm.Case
import Elm.Declare
import Elm.Declare.Extra
import Elm.Op
import Gen.Maybe
import Gen.Parser
import Gen.Result
import Generate.Enum as Enum exposing (Enum, Variant)
import Generate.Image exposing (ImageModule)
import Generate.Utils exposing (yassify)
import Parsers
import String.Extra


type alias TypesModule =
    { affinity : EnumModule
    , class : EnumModule
    , companion : EnumModule
    , complication : EnumModule
    , complicationCategory : EnumModule
    , faction : EnumModule
    , gameMode : EnumModule
    , magic : EnumModule
    , perk : EnumModule
    , quest : EnumModule
    , race : EnumModule
    , relic : EnumModule
    , size : EnumModule
    , slot : EnumModule
    }


file : ImageModule -> List Parsers.DLC -> ( Elm.Declare.Module TypesModule, Enums )
file images dlcList =
    let
        moduleName : List String
        moduleName =
            [ "Generated", "Types" ]

        enums : Data.Enums
        enums =
            Data.enums dlcList

        module_ : Elm.Declare.Module TypesModule
        module_ =
            Elm.Declare.module_ moduleName TypesModule
                |> Elm.Declare.withSubmodule (enumToDeclarations moduleName images enums.affinity)
                |> Elm.Declare.withSubmodule (enumToDeclarations moduleName images enums.class)
                |> Elm.Declare.withSubmodule (enumToDeclarations moduleName images enums.companion)
                |> Elm.Declare.withSubmodule (enumToDeclarations moduleName images enums.complication)
                |> Elm.Declare.withSubmodule (enumToDeclarations moduleName images enums.complicationCategory)
                |> Elm.Declare.withSubmodule (enumToDeclarations moduleName images enums.faction)
                |> Elm.Declare.withSubmodule (enumToDeclarations moduleName images enums.gameMode)
                |> Elm.Declare.withSubmodule (enumToDeclarations moduleName images enums.magic)
                |> Elm.Declare.withSubmodule (enumToDeclarations moduleName images enums.perk)
                |> Elm.Declare.withSubmodule (enumToDeclarations moduleName images enums.quest)
                |> Elm.Declare.withSubmodule (enumToDeclarations moduleName images enums.race)
                |> Elm.Declare.withSubmodule (enumToDeclarations moduleName images enums.relic)
                |> Elm.Declare.withSubmodule (enumToDeclarations moduleName images enums.size)
                |> Elm.Declare.withSubmodule (enumToDeclarations moduleName images enums.slot)
    in
    ( { name = module_.name
      , declarations = module_.declarations
      , call = module_.call
      }
    , enums
    )


type alias EnumModule =
    { value : String -> Elm.Expression
    , annotation : Elm.Annotation.Annotation
    , parser : Elm.Expression
    , fromString : Elm.Expression -> Elm.Expression
    , argWith : String -> List (Elm.Arg Elm.Expression) -> Elm.Arg (List Elm.Expression)
    }


enumToDeclarations : List String -> ImageModule -> Enum -> Elm.Declare.Module EnumModule
enumToDeclarations moduleName images enum =
    let
        type_ : Elm.Declare.Annotation
        type_ =
            Enum.toAnnotation enum

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
                , variantName = enum.name ++ yassify variantName
                }
                identity
                |> Elm.Arg.items args

        valueFrom : String -> Elm.Expression
        valueFrom name =
            Elm.value
                { importFrom = moduleName
                , name = enum.name ++ yassify name
                , annotation = Nothing
                }

        common : Elm.Declare.Module EnumModule
        common =
            let
                inner : Elm.Declare.Module ((String -> List (Elm.Arg Elm.Expression) -> Elm.Arg (List Elm.Expression)) -> EnumModule)
                inner =
                    Elm.Declare.module_ moduleName (EnumModule valueFrom)
                        |> Elm.Declare.Extra.withDeclarations [ Elm.docs ("# " ++ enum.name) ]
                        |> Elm.Declare.with type_
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
parserDeclaration { lowerName, type_ } { name, variants } =
    variants
        |> List.map
            (\variant ->
                let
                    start : Elm.Expression
                    start =
                        Elm.Op.skip
                            (Gen.Parser.succeed (Elm.val <| name ++ yassify variant.name))
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


toImageDeclaration : ImageModule -> Config -> Enum -> Elm.Declare.Function (Elm.Expression -> Elm.Expression)
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
                            Generate.Image.valueFrom (lowerName ++ constructor)
                    in
                    Elm.Case.branch
                        (Elm.Arg.customType (name ++ constructor) identity
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
isSameDeclaration { lowerName, type_ } { name, variants } =
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
                        name ++ yassify variant.name
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


fromStringDeclaration : Config -> Enum -> Elm.Declare.Function (Elm.Expression -> Elm.Expression)
fromStringDeclaration { lowerName, type_ } { name, variants } =
    (\value ->
        (if isEnum variants then
            Elm.Case.string value
                { cases =
                    List.map
                        (\variant ->
                            ( variant.toStringException
                                |> Maybe.withDefault variant.name
                                |> String.replace "\"" "\\\""
                            , Gen.Maybe.make_.just <| Elm.val <| name ++ yassify variant.name
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
