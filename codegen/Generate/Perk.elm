module Generate.Perk exposing (PerkModule, file)

import Elm
import Elm.Annotation
import Elm.Arg
import Elm.Case
import Elm.Declare
import Elm.Declare.Extra
import Elm.Let
import Elm.Op
import Gen.CodeGen.Generate as Generate
import Gen.Data.Perk
import Gen.List
import Gen.List.Extra
import Gen.Maybe
import Gen.Types
import Generate.Enum as Enum exposing (Argument(..), Enum)
import Generate.Types exposing (TypesModule)
import Generate.Utils exposing (yassify)
import Parsers
import ResultME exposing (ResultME)
import String.Extra


type alias PerkModule =
    { all : Elm.Expression -> Elm.Expression
    , toString : Elm.Expression -> Elm.Expression
    , details : Elm.Annotation.Annotation
    , containsDash : Elm.Expression -> Elm.Expression
    }


file : TypesModule -> Enum -> List ( Maybe String, Parsers.Perk ) -> ResultME Generate.Error (Elm.Declare.Module PerkModule)
file types enum dlcPerks =
    ResultME.map
        (\declarations ->
            Elm.Declare.module_ [ "Generated", "Perk" ] PerkModule
                |> Elm.Declare.with (all types dlcPerks)
                |> Elm.Declare.with (Enum.toString enum)
                |> Elm.Declare.with (details types)
                |> Elm.Declare.with (containsDash types dlcPerks)
                |> Elm.Declare.withDeclarations declarations
        )
        (dlcToPerks types dlcPerks)


all : TypesModule -> List ( Maybe String, Parsers.Perk ) -> Elm.Declare.Function (Elm.Expression -> Elm.Expression)
all types dlcPerks =
    Elm.Declare.fn "all"
        (Elm.Arg.varWith "perks"
            (Elm.Annotation.list Gen.Types.annotation_.rankedPerk)
        )
    <|
        \perks ->
            (dlcPerks
                |> List.sortBy (\( dlc, _ ) -> Maybe.withDefault "" dlc)
                |> List.map
                    (\( _, perk ) ->
                        let
                            simple : Elm.Expression
                            simple =
                                yassify perk.name
                                    |> String.Extra.decapitalize
                                    |> Elm.val
                        in
                        if List.isEmpty perk.arguments then
                            simple

                        else
                            Elm.apply simple [ perks ]
                    )
                |> Elm.list
            )
                |> Gen.List.call_.sortBy
                    (Elm.fn
                        (Elm.Arg.record identity
                            |> Elm.Arg.field "dlc"
                        )
                        (\dlc -> Gen.Maybe.withDefault (Elm.string "") dlc)
                    )
                |> Elm.withType (Elm.Annotation.list (details types).annotation)


containsDash : TypesModule -> List ( Maybe String, Parsers.Perk ) -> Elm.Declare.Function (Elm.Expression -> Elm.Expression)
containsDash types dlcPerks =
    Elm.Declare.fn "containsDash"
        (Elm.Arg.varWith "perk" types.perk.annotation)
    <|
        \perk ->
            dlcPerks
                |> List.map
                    (\( _, dlcPerk ) ->
                        ( dlcPerk.name
                        , List.map (\_ -> Elm.Arg.ignore) dlcPerk.arguments
                        )
                    )
                |> List.map
                    (\( name, args ) ->
                        Elm.Case.branch (types.perk.argWith name args)
                            (\_ -> Elm.bool (String.contains "-" name))
                    )
                |> Elm.Case.custom perk types.perk.annotation


details :
    TypesModule
    ->
        Elm.Declare.Extra.Record
            { name : Elm.Expression
            , class : Elm.Expression
            , affinity : Elm.Expression
            , isMeta : Elm.Expression
            , requires : Elm.Expression
            , content : Elm.Expression
            , dlc : Elm.Expression
            }
details types =
    Elm.Declare.record "Details"
        |> Elm.Declare.withField "name" .name types.perk.annotation
        |> Elm.Declare.withField "class" .class types.class.annotation
        |> Elm.Declare.withField "requires" .requires (Elm.Annotation.maybe Elm.Annotation.string)
        |> Elm.Declare.withField "affinity" .affinity (Elm.Annotation.list types.affinity.annotation)
        |> Elm.Declare.withField "isMeta" .isMeta Elm.Annotation.bool
        |> Elm.Declare.withField "content" .content Gen.Data.Perk.annotation_.content
        |> Elm.Declare.withField "dlc" .dlc (Elm.Annotation.maybe Elm.Annotation.string)
        |> Elm.Declare.buildRecord


dlcToPerks : TypesModule -> List ( Maybe String, Parsers.Perk ) -> ResultME Generate.Error (List Elm.Declaration)
dlcToPerks types perks =
    ResultME.combineMap
        (\( dlcName, perk ) ->
            perkToDeclaration types dlcName perk
                |> Result.map
                    (\expr ->
                        expr
                            |> Elm.declaration (yassify perk.name)
                            |> Elm.expose
                    )
        )
        perks


perkToDeclaration : TypesModule -> Maybe String -> Parsers.Perk -> ResultME Generate.Error Elm.Expression
perkToDeclaration types dlcName perk =
    case perk.arguments of
        [] ->
            (details types).make
                { name = types.perk.value perk.name
                , class = types.class.value perk.class
                , affinity = Elm.list (List.map types.affinity.value perk.elements)
                , isMeta = Elm.bool perk.isMeta
                , requires = Elm.maybe (Maybe.map Elm.string perk.requires)
                , content =
                    case perk.content of
                        Parsers.Single cost description ->
                            Gen.Data.Perk.make_.single (Elm.int cost) (Elm.string description)

                        Parsers.WithCosts costs description ->
                            Gen.Data.Perk.make_.withCosts (Elm.list (List.map Elm.int costs)) (Elm.string description)

                        Parsers.WithChoices () before choices after ->
                            Gen.Data.Perk.make_.withChoices
                                (Elm.string before)
                                (choices
                                    |> List.map
                                        (\( choice, cost ) -> Elm.tuple (Elm.string choice) (Elm.int cost))
                                    |> Elm.list
                                )
                                (Elm.string after)
                , dlc = Elm.maybe (Maybe.map Elm.string dlcName)
                }
                |> Ok

        [ ValueArgument "Race" ] ->
            Elm.fn (Elm.Arg.varWith "perks" (Elm.Annotation.list Gen.Types.annotation_.rankedPerk))
                (\perks ->
                    Elm.Let.letIn identity
                        |> Elm.Let.value "race"
                            (Gen.List.Extra.call_.findMap
                                (Elm.fn (Elm.Arg.var "per") <|
                                    \p ->
                                        Elm.Case.custom (p |> Elm.get "name")
                                            types.perk.annotation
                                            [ Elm.Case.branch (types.perk.argWith perk.name [ Elm.Arg.var "race" ])
                                                (\race ->
                                                    Elm.maybe (List.head race)
                                                )
                                            , Elm.Case.branch Elm.Arg.ignore (\_ -> Elm.maybe Nothing)
                                            ]
                                )
                                perks
                                |> Elm.Op.pipe
                                    (Elm.functionReduced "race"
                                        (Gen.Maybe.withDefault (types.race.value "Neutral"))
                                    )
                            )
                        |> Elm.Let.withBody
                            (\race ->
                                (details types).make
                                    { name = Elm.apply (types.perk.value perk.name) [ race ]
                                    , class = types.class.value perk.class
                                    , affinity = Elm.list (List.map types.affinity.value perk.elements)
                                    , isMeta = Elm.bool perk.isMeta
                                    , requires = Elm.maybe (Maybe.map Elm.string perk.requires)
                                    , content =
                                        case perk.content of
                                            Parsers.Single cost description ->
                                                Gen.Data.Perk.make_.single (Elm.int cost) (Elm.string description)

                                            Parsers.WithCosts costs description ->
                                                Gen.Data.Perk.make_.withCosts (Elm.list (List.map Elm.int costs)) (Elm.string description)

                                            Parsers.WithChoices () before choices after ->
                                                Gen.Data.Perk.make_.withChoices
                                                    (Elm.string before)
                                                    (choices
                                                        |> List.map
                                                            (\( choice, cost ) -> Elm.tuple (Elm.string choice) (Elm.int cost))
                                                        |> Elm.list
                                                    )
                                                    (Elm.string after)
                                    , dlc = Elm.maybe (Maybe.map Elm.string dlcName)
                                    }
                            )
                        |> Elm.withType (details types).annotation
                )
                |> Ok

        _ ->
            ResultME.error
                { title = "Error parsing perks file"
                , description = "Unexpected extra arguments list, expected at most one"
                }
