module Generate.Race exposing (RacesModule, file)

import Elm
import Elm.Annotation
import Elm.Arg
import Elm.Case
import Elm.Declare
import Elm.Declare.Extra
import Gen.CodeGen.Generate as Generate
import Gen.Data.Race
import Gen.List
import Gen.Maybe
import Generate.Enum as Enum exposing (Enum)
import Generate.Types exposing (TypesModule)
import Generate.Utils exposing (yassify)
import Parsers
import ResultME exposing (ResultME)
import String.Extra


type alias RacesModule =
    { all : Elm.Expression -> Elm.Expression
    , toString : Elm.Expression -> Elm.Expression
    }


file : TypesModule -> Enum -> List ( Maybe String, Parsers.Race ) -> ResultME Generate.Error (Elm.Declare.Module RacesModule)
file types enum dlcRaces =
    Result.map
        (\declarations ->
            Elm.Declare.module_ [ "Generated", "Race" ] RacesModule
                |> Elm.Declare.with (all types dlcRaces)
                |> Elm.Declare.with (Enum.toString enum)
                |> Elm.Declare.Extra.withDeclarations declarations
        )
        (dlcToRaces types dlcRaces)


all : TypesModule -> List ( Maybe String, Parsers.Race ) -> Elm.Declare.Function (Elm.Expression -> Elm.Expression)
all types dlcRaces =
    Elm.Declare.fn "all"
        (Elm.Arg.varWith "races"
            (Elm.Annotation.list types.race.annotation)
        )
    <|
        \races ->
            dlcRaces
                |> List.sortBy (\( dlc, _ ) -> Maybe.withDefault "" dlc)
                |> List.map
                    (\( _, race ) ->
                        let
                            simple : Elm.Expression
                            simple =
                                yassify race.name
                                    |> String.Extra.decapitalize
                                    |> Elm.val
                        in
                        case race.elements of
                            [ _, _ ] ->
                                simple

                            _ ->
                                Elm.apply simple [ races ]
                    )
                |> Elm.list
                |> Gen.List.call_.sortBy
                    (Elm.fn
                        (Elm.Arg.record identity
                            |> Elm.Arg.field "dlc"
                        )
                        (\dlc -> Gen.Maybe.withDefault (Elm.string "") dlc)
                    )
                |> Elm.withType (Elm.Annotation.list Gen.Data.Race.annotation_.details)


dlcToRaces : TypesModule -> List ( Maybe String, Parsers.Race ) -> ResultME Generate.Error (List Elm.Declaration)
dlcToRaces types races =
    ResultME.combineMap
        (\( dlcName, race ) ->
            raceToDeclaration types dlcName race
                |> Result.map
                    (\expr ->
                        expr
                            |> Elm.declaration (yassify race.name)
                            |> Elm.expose
                    )
        )
        races


raceToDeclaration : TypesModule -> Maybe String -> Parsers.Race -> ResultME Generate.Error Elm.Expression
raceToDeclaration types dlcName race =
    case race.elements of
        [ _, _ ] ->
            Gen.Data.Race.make_.details
                { name = types.race.value race.name
                , content = Elm.string race.description
                , tank = types.size.value race.manaCapacity
                , affinities = Elm.list (List.map types.affinity.value race.elements)
                , charge = types.size.value race.manaRate
                , dlc = Elm.maybe (Maybe.map Elm.string dlcName)
                }
                |> Ok

        [ _ ] ->
            Elm.fn (Elm.Arg.varWith "affinities" (Elm.Annotation.list types.race.annotation))
                (\affinities ->
                    Gen.Data.Race.call_.withVariantAffinity1
                        (Elm.fn (Elm.Arg.var "r") <|
                            \r ->
                                Elm.Case.custom r
                                    types.race.annotation
                                    [ Elm.Case.branch (types.race.argWith race.name [ Elm.Arg.var "aff" ])
                                        (\affs ->
                                            Elm.maybe (List.head affs)
                                        )
                                    , Elm.Case.branch Elm.Arg.ignore (\_ -> Elm.maybe Nothing)
                                    ]
                        )
                        (Elm.record
                            [ ( "name", Elm.functionReduced "aff" <| \aff -> Elm.apply (types.race.value race.name) [ aff ] )
                            , ( "content", Elm.string race.description )
                            , ( "tank", types.size.value race.manaCapacity )
                            , ( "affinities", Elm.list (List.map types.affinity.value race.elements) )
                            , ( "charge", types.size.value race.manaRate )
                            , ( "dlc", Elm.maybe (Maybe.map Elm.string dlcName) )
                            ]
                        )
                        affinities
                        |> Elm.withType Gen.Data.Race.annotation_.details
                )
                |> Ok

        [] ->
            Elm.fn (Elm.Arg.varWith "affinities" (Elm.Annotation.list types.race.annotation))
                (\affinities ->
                    Gen.Data.Race.call_.withVariantAffinity2
                        (Elm.fn (Elm.Arg.var "r") <|
                            \r ->
                                Elm.Case.custom r
                                    types.race.annotation
                                    [ Elm.Case.branch (types.race.argWith race.name [ Elm.Arg.var "aff1", Elm.Arg.var "aff2" ])
                                        (\affs ->
                                            case affs of
                                                [ aff1, aff2 ] ->
                                                    Just (Elm.tuple aff1 aff2)
                                                        |> Elm.maybe

                                                _ ->
                                                    Elm.maybe Nothing
                                        )
                                    , Elm.Case.branch Elm.Arg.ignore (\_ -> Elm.maybe Nothing)
                                    ]
                        )
                        (Elm.record
                            [ ( "name", Elm.functionReduced "aff" <| \aff -> Elm.apply (types.race.value race.name) [ aff ] )
                            , ( "content", Elm.string race.description )
                            , ( "tank", types.size.value race.manaCapacity )
                            , ( "affinities", Elm.list (List.map types.affinity.value race.elements) )
                            , ( "charge", types.size.value race.manaRate )
                            , ( "dlc", Elm.maybe (Maybe.map Elm.string dlcName) )
                            ]
                        )
                        affinities
                        |> Elm.withType Gen.Data.Race.annotation_.details
                )
                |> Ok

        _ ->
            ResultME.error
                { title = "Error parsing races file"
                , description = "Unexpected elements list, expected at most two"
                }
