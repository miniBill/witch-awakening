module Generate.Companions exposing (CompanionModule, file)

import Elm
import Elm.Annotation
import Elm.Declare
import Elm.Declare.Extra
import Gen.Data.Companion
import Generate.Types exposing (TypesModule)
import Generate.Utils exposing (yassify)
import List.Extra
import Parsers exposing (Score(..))
import String.Extra


type alias CompanionModule =
    { all : Elm.Expression
    }


file : TypesModule -> List ( Maybe String, Parsers.Companion ) -> Elm.Declare.Module CompanionModule
file types dlcCompanions =
    Elm.Declare.module_ [ "Generated", "Companion" ] CompanionModule
        |> Elm.Declare.with (all types dlcCompanions)
        |> Elm.Declare.Extra.withDeclarations (dlcToCompanions types dlcCompanions)


all : TypesModule -> List ( Maybe String, Parsers.Companion ) -> Elm.Declare.Value
all types dlcCompanions =
    dlcCompanions
        |> List.Extra.gatherEqualsBy (\( _, companion ) -> companion.faction)
        |> List.sortBy (\( ( _, { faction } ), _ ) -> factionToOrder faction)
        |> List.map
            (\( ( _, { faction } ) as head, tail ) ->
                Elm.tuple
                    (Elm.maybe (Maybe.map types.valueFrom faction))
                    ((head :: tail)
                        |> List.map
                            (\( _, companion ) ->
                                Elm.val
                                    (String.Extra.decapitalize (yassify companion.name))
                            )
                        |> Elm.list
                    )
            )
        |> Elm.list
        |> Elm.withType
            (Elm.Annotation.list
                (Elm.Annotation.tuple
                    (Elm.Annotation.maybe types.faction.annotation)
                    (Elm.Annotation.list Gen.Data.Companion.annotation_.details)
                )
            )
        |> Elm.Declare.value "all"


factionToOrder : Maybe String -> Int
factionToOrder faction =
    case faction of
        Just "The College of Arcadia" ->
            0

        Just "Hawthorne Academia" ->
            1

        Just "The Watchers" ->
            2

        Just "The Hespatian Coven" ->
            3

        Just "Lunabella" ->
            4

        Just "AlfheimrAlliance" ->
            5

        Just "The O.R.C." ->
            6

        Just "Alphazon Industries" ->
            7

        Nothing ->
            8

        Just "The Outsiders" ->
            9

        _ ->
            10


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
                            Gen.Data.Companion.make_.classOne (types.valueFrom c)

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
            Gen.Data.Companion.make_.details
                { name = types.valueFrom companion.name
                , class = class
                , races =
                    Elm.list
                        (List.map
                            (\race ->
                                case
                                    race
                                        |> String.split "-"
                                        |> List.map types.valueFrom
                                of
                                    [ x ] ->
                                        x

                                    h :: t ->
                                        Elm.apply h t

                                    [] ->
                                        types.valueFrom "Empty list?"
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
                }
                |> Elm.declaration (yassify companion.name)
                |> Elm.expose
        )
        companions
