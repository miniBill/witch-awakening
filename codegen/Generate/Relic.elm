module Generate.Relic exposing (RelicsModule, file)

import Elm
import Elm.Annotation
import Elm.Arg
import Elm.Case
import Elm.Declare
import Elm.Declare.Extra
import Elm.Let
import Elm.Op
import Gen.CodeGen.Generate as Generate
import Gen.Data.Relic
import Gen.List.Extra
import Gen.Maybe
import Gen.Types
import Generate.Enum as Enum exposing (Argument(..), Enum)
import Generate.Types exposing (TypesModule)
import Generate.Utils exposing (yassify)
import Parsers
import ResultME exposing (ResultME)
import String.Extra


type alias RelicsModule =
    { all : Elm.Expression -> Elm.Expression
    , toString : Elm.Expression -> Elm.Expression
    , details : Elm.Annotation.Annotation
    }


file :
    TypesModule
    -> Enum
    -> List ( Maybe String, Parsers.Relic )
    -> ResultME Generate.Error (Elm.Declare.Module RelicsModule)
file types enum dlcRelics =
    ResultME.map
        (\declarations ->
            Elm.Declare.module_ [ "Generated", "Relic" ] RelicsModule
                |> Elm.Declare.with (all types dlcRelics)
                |> Elm.Declare.with (Enum.toString enum)
                |> Elm.Declare.with (details types)
                |> Elm.Declare.Extra.withDeclarations declarations
        )
        (dlcToRelics types dlcRelics)


all : TypesModule -> List ( Maybe String, Parsers.Relic ) -> Elm.Declare.Function (Elm.Expression -> Elm.Expression)
all types dlcRelics =
    Elm.Declare.fn "all"
        (Elm.Arg.varWith "relics"
            (Elm.Annotation.list Gen.Types.annotation_.rankedRelic)
        )
    <|
        \relics ->
            dlcRelics
                |> List.sortBy (\( dlc, _ ) -> Maybe.withDefault "" dlc)
                |> List.map
                    (\( _, relic ) ->
                        let
                            simple : Elm.Expression
                            simple =
                                yassify relic.name
                                    |> String.Extra.decapitalize
                                    |> Elm.val
                        in
                        if List.isEmpty relic.arguments then
                            simple

                        else
                            Elm.apply simple [ relics ]
                    )
                |> Elm.list
                |> Elm.withType (Elm.Annotation.list (details types).annotation)


details :
    TypesModule
    ->
        { annotation : Elm.Annotation.Annotation
        , declaration : Elm.Declaration
        , internal : Elm.Declare.Internal Elm.Annotation.Annotation
        , make :
            { content : Elm.Expression
            , dlc : Elm.Expression
            , requires : Elm.Expression
            , classes : Elm.Expression
            , name : Elm.Expression
            }
            -> Elm.Expression
        }
details types =
    Elm.Declare.Extra.customRecord "Details"
        |> Elm.Declare.Extra.withField "name" .name types.relic.annotation
        |> Elm.Declare.Extra.withField "classes" .classes (Elm.Annotation.list types.class.annotation)
        |> Elm.Declare.Extra.withField "dlc" .dlc (Elm.Annotation.maybe Elm.Annotation.string)
        |> Elm.Declare.Extra.withField "requires" .requires (Elm.Annotation.maybe Elm.Annotation.string)
        |> Elm.Declare.Extra.withField "content" .content Gen.Data.Relic.annotation_.content
        |> Elm.Declare.Extra.buildCustomRecord


dlcToRelics : TypesModule -> List ( Maybe String, Parsers.Relic ) -> ResultME Generate.Error (List Elm.Declaration)
dlcToRelics types relics =
    ResultME.combineMap
        (\( dlcName, relic ) ->
            relicToDeclaration types dlcName relic
                |> Result.map
                    (\expr ->
                        expr
                            |> Elm.declaration (yassify relic.name)
                            |> Elm.expose
                    )
        )
        relics


relicToDeclaration : TypesModule -> Maybe String -> Parsers.Relic -> ResultME Generate.Error Elm.Expression
relicToDeclaration types dlcName relic =
    case relic.arguments of
        [] ->
            (details types).make
                { name = types.relic.value relic.name
                , classes = Elm.list (List.map types.class.value relic.classes)
                , dlc = Elm.maybe (Maybe.map Elm.string dlcName)
                , requires = Elm.maybe (Maybe.map Elm.string relic.requires)
                , content =
                    case relic.content of
                        Parsers.Single cost description ->
                            Gen.Data.Relic.make_.single (Elm.int cost) (Elm.string description)

                        Parsers.WithCosts costs description ->
                            Gen.Data.Relic.make_.withChoices (Elm.list (List.map Elm.int costs)) (Elm.string description)

                        Parsers.WithChoices ever _ _ _ ->
                            never ever
                }
                |> Ok

        [ ValueArgument "CosmicPearlData" ] ->
            Elm.fn (Elm.Arg.varWith "relics" (Elm.Annotation.list Gen.Types.annotation_.rankedRelic))
                (\relics ->
                    Elm.Let.letIn identity
                        |> Elm.Let.value "pearlData"
                            (Gen.List.Extra.call_.findMap
                                (Elm.fn (Elm.Arg.var "relic") <|
                                    \r ->
                                        Elm.Case.custom (r |> Elm.get "name")
                                            types.relic.annotation
                                            [ Elm.Case.branch (types.relic.argWith relic.name [ Elm.Arg.var "data" ])
                                                (\data ->
                                                    Elm.maybe (List.head data)
                                                )
                                            , Elm.Case.branch Elm.Arg.ignore (\_ -> Elm.maybe Nothing)
                                            ]
                                )
                                relics
                                |> Elm.Op.pipe
                                    (Elm.functionReduced "data"
                                        (Gen.Maybe.withDefault
                                            (Elm.record
                                                [ ( "add", Elm.list [] )
                                                , ( "change", Elm.list [] )
                                                ]
                                            )
                                        )
                                    )
                            )
                        |> Elm.Let.withBody
                            (\pearlData ->
                                (details types).make
                                    { name = Elm.apply (types.relic.value relic.name) [ pearlData ]
                                    , classes = Elm.list (List.map types.class.value relic.classes)
                                    , dlc = Elm.maybe (Maybe.map Elm.string dlcName)
                                    , requires = Elm.maybe (Maybe.map Elm.string relic.requires)
                                    , content =
                                        case relic.content of
                                            Parsers.Single cost description ->
                                                Gen.Data.Relic.make_.single (Elm.int cost) (Elm.string description)

                                            Parsers.WithCosts costs description ->
                                                Gen.Data.Relic.make_.withChoices (Elm.list (List.map Elm.int costs)) (Elm.string description)

                                            Parsers.WithChoices ever _ _ _ ->
                                                never ever
                                    }
                            )
                        |> Elm.withType (details types).annotation
                )
                |> Ok

        _ ->
            ResultME.error
                { title = "Error parsing relics file"
                , description = "Unexpected extra arguments list, expected at most one"
                }
