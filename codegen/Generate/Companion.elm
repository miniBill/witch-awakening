module Generate.Companion exposing (CompanionModule, file)

import Elm
import Elm.Annotation
import Elm.Declare
import Elm.Declare.Extra
import Gen.CodeGen.Generate as Generate
import Gen.Data.Companion
import Generate.Enum as Enum exposing (Enum)
import Generate.Types exposing (TypesModule)
import Generate.Utils exposing (yassify)
import List.Extra
import Parsers
import ResultME exposing (ResultME)
import String.Extra


type alias CompanionModule =
    { all : Elm.Expression
    , toString : Elm.Expression -> Elm.Expression
    , details : Elm.Annotation.Annotation
    }


file : TypesModule -> Enum -> List ( Maybe String, Parsers.Companion ) -> ResultME Generate.Error (Elm.Declare.Module CompanionModule)
file types enum dlcCompanions =
    Result.map
        (\allDeclaration ->
            Elm.Declare.module_ [ "Generated", "Companion" ] CompanionModule
                |> Elm.Declare.with allDeclaration
                |> Elm.Declare.with (Enum.toString enum)
                |> Elm.Declare.with (details types)
                |> Elm.Declare.Extra.withDeclarations (dlcToCompanions types dlcCompanions)
        )
        (all types dlcCompanions)


all : TypesModule -> List ( Maybe String, Parsers.Companion ) -> ResultME Generate.Error Elm.Declare.Value
all types dlcCompanions =
    dlcCompanions
        |> List.Extra.gatherEqualsBy (\( _, companion ) -> companion.faction)
        |> ResultME.combineMap
            (\(( ( _, { faction } ), _ ) as original) ->
                Result.map (Tuple.pair original) (factionToOrder faction)
            )
        |> Result.map
            (\list ->
                list
                    |> List.sortBy Tuple.second
                    |> List.map
                        (\( ( ( _, { faction } ) as head, tail ), _ ) ->
                            Elm.tuple
                                (Elm.maybe (Maybe.map types.faction.value faction))
                                ((head :: tail)
                                    |> List.map
                                        (\( _, companion ) ->
                                            yassify companion.name
                                                |> String.Extra.decapitalize
                                                |> Elm.val
                                        )
                                    |> Elm.list
                                )
                        )
                    |> Elm.list
                    |> Elm.withType
                        (Elm.Annotation.list
                            (Elm.Annotation.tuple
                                (Elm.Annotation.maybe types.faction.annotation)
                                (Elm.Annotation.list (details types).annotation)
                            )
                        )
                    |> Elm.Declare.value "all"
            )


details :
    TypesModule
    ->
        { annotation : Elm.Annotation.Annotation
        , declaration : Elm.Declaration
        , internal : Elm.Declare.Internal Elm.Annotation.Annotation
        , make :
            { name : Elm.Expression
            , class : Elm.Expression
            , races : Elm.Expression
            , hasPerk : Elm.Expression
            , cost : Elm.Expression
            , power : Elm.Expression
            , teamwork : Elm.Expression
            , sociability : Elm.Expression
            , morality : Elm.Expression
            , quote : Elm.Expression
            , description : Elm.Expression
            , positives : Elm.Expression
            , negatives : Elm.Expression
            , mixed : Elm.Expression
            , has : Elm.Expression
            , dlc : Elm.Expression
            , requires : Elm.Expression
            }
            -> Elm.Expression
        }
details types =
    Elm.Declare.Extra.customRecord "Details"
        |> Elm.Declare.Extra.withField "name" .name types.companion.annotation
        |> Elm.Declare.Extra.withField "class" .class Gen.Data.Companion.annotation_.maybeClass
        |> Elm.Declare.Extra.withField "races" .races (Elm.Annotation.list types.race.annotation)
        |> Elm.Declare.Extra.withField "hasPerk" .hasPerk Elm.Annotation.bool
        |> Elm.Declare.Extra.withField "cost" .cost (Elm.Annotation.maybe Elm.Annotation.int)
        |> Elm.Declare.Extra.withField "power" .power Gen.Data.Companion.annotation_.score
        |> Elm.Declare.Extra.withField "teamwork" .teamwork Gen.Data.Companion.annotation_.score
        |> Elm.Declare.Extra.withField "sociability" .sociability Gen.Data.Companion.annotation_.score
        |> Elm.Declare.Extra.withField "morality" .morality Gen.Data.Companion.annotation_.score
        |> Elm.Declare.Extra.withField "quote" .quote Elm.Annotation.string
        |> Elm.Declare.Extra.withField "description" .description Elm.Annotation.string
        |> Elm.Declare.Extra.withField "positives" .positives (Elm.Annotation.list Elm.Annotation.string)
        |> Elm.Declare.Extra.withField "negatives" .negatives (Elm.Annotation.list Elm.Annotation.string)
        |> Elm.Declare.Extra.withField "mixed" .mixed (Elm.Annotation.list Elm.Annotation.string)
        |> Elm.Declare.Extra.withField "has" .has Elm.Annotation.string
        |> Elm.Declare.Extra.withField "dlc" .dlc (Elm.Annotation.maybe Elm.Annotation.string)
        |> Elm.Declare.Extra.withField "requires" .requires (Elm.Annotation.maybe Elm.Annotation.string)
        |> Elm.Declare.Extra.buildCustomRecord


factionToOrder : Maybe String -> ResultME Generate.Error Int
factionToOrder faction =
    case faction of
        Just "The College of Arcadia" ->
            Ok 0

        Just "Hawthorne Academia" ->
            Ok 1

        Just "The Watchers" ->
            Ok 2

        Just "The Hespatian Coven" ->
            Ok 3

        Just "Lunabella" ->
            Ok 4

        Just "Alfheimr Alliance" ->
            Ok 5

        Just "The Lodge" ->
            Ok 6

        Just "The Lydian Sisterhood" ->
            Ok 7

        Just "The Seekers' Guild" ->
            Ok 8

        Just "The O.R.C." ->
            Ok 1001

        Just "Alphazon Industries" ->
            Ok 1001

        Nothing ->
            Ok 2000

        Just "The Outsiders" ->
            Ok 3000

        Just factionName ->
            -- Ok 500
            ResultME.error
                { title = "Unsorted faction"
                , description = "We don't know the order for " ++ factionName
                }


dlcToCompanions : TypesModule -> List ( Maybe String, Parsers.Companion ) -> List Elm.Declaration
dlcToCompanions types companions =
    List.map
        (\( dlcName, companion ) ->
            let
                class : Elm.Expression
                class =
                    case companion.class of
                        Just "Any" ->
                            Gen.Data.Companion.make_.classAny

                        Just "Special" ->
                            Gen.Data.Companion.make_.classSpecial

                        Just c ->
                            Gen.Data.Companion.make_.classOne (types.class.value c)

                        Nothing ->
                            Gen.Data.Companion.make_.classNone

                score : Parsers.Score -> Elm.Expression
                score value =
                    case value of
                        Parsers.NormalScore s ->
                            Gen.Data.Companion.make_.normalScore (Elm.int s)

                        Parsers.SpecialEffect { worse, better } ->
                            Gen.Data.Companion.make_.specialEffect
                                (Elm.record
                                    [ ( "worse", Elm.maybe (Maybe.map Elm.int worse) )
                                    , ( "better", Elm.int better )
                                    ]
                                )
            in
            (details types).make
                { name = types.companion.value companion.name
                , class = class
                , races =
                    Elm.list
                        (List.map
                            (\race ->
                                case
                                    race
                                        |> String.split "-"
                                of
                                    [ x ] ->
                                        types.race.value x

                                    h :: t ->
                                        Elm.apply (types.race.value h) (List.map types.affinity.value t)

                                    [] ->
                                        -- Impossible case
                                        types.race.value "Empty list?"
                            )
                            companion.races
                        )
                , hasPerk = Elm.bool companion.hasPerk
                , cost = Elm.maybe (Maybe.map Elm.int companion.cost)
                , power = score companion.power
                , teamwork = score companion.teamwork
                , sociability = score companion.sociability
                , morality = score companion.morality
                , quote = Elm.string companion.quote
                , description = Elm.string companion.description
                , positives = Elm.list (List.map Elm.string companion.positives)
                , negatives = Elm.list (List.map Elm.string companion.negatives)
                , mixed = Elm.list (List.map Elm.string companion.mixed)
                , has = Elm.string companion.has
                , dlc = Elm.maybe (Maybe.map Elm.string dlcName)
                , requires = Elm.maybe (Maybe.map Elm.string companion.requires)
                }
                |> Elm.declaration (yassify companion.name)
                |> Elm.expose
        )
        companions
