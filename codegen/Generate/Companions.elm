module Generate.Companions exposing (file)

import Elm
import Elm.Annotation
import Elm.Op
import Gen.Data.Companion
import Generate.Types
import Generate.Utils exposing (yassify)
import List.Extra
import Parsers exposing (Score(..))
import String.Extra


file : List ( Maybe String, Parsers.Companion ) -> Elm.File
file dlcCompanions =
    Elm.file [ "Generated", "Companion" ]
        (all dlcCompanions
            :: dlcToCompanions dlcCompanions
        )


all : List ( Maybe String, Parsers.Companion ) -> Elm.Declaration
all dlcCompanions =
    Elm.Op.append
        (dlcCompanions
            |> List.Extra.gatherEqualsBy (\( _, companion ) -> companion.faction)
            |> List.sortBy (\( ( _, { faction } ), _ ) -> factionToOrder faction)
            |> List.map
                (\( ( _, { faction } ) as head, tail ) ->
                    Elm.tuple
                        (Elm.maybe (Maybe.map Generate.Types.valueFrom faction))
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
        )
        Gen.Data.Companion.all
        |> Elm.withType
            (Elm.Annotation.list
                (Elm.Annotation.tuple
                    (Elm.Annotation.maybe (Elm.Annotation.named [ "Generated", "Types" ] "Faction"))
                    (Elm.Annotation.list Gen.Data.Companion.annotation_.details)
                )
            )
        |> Elm.declaration "all"
        |> Elm.expose


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


dlcToCompanions : List ( Maybe String, Parsers.Companion ) -> List Elm.Declaration
dlcToCompanions companions =
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
                            Gen.Data.Companion.make_.classOne (Generate.Types.valueFrom c)

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
                { name = Generate.Types.valueFrom companion.name
                , class = class
                , races =
                    Elm.list
                        (List.map
                            (\race ->
                                case
                                    race
                                        |> String.split "-"
                                        |> List.map Generate.Types.valueFrom
                                of
                                    [ x ] ->
                                        x

                                    h :: t ->
                                        Elm.apply h t

                                    [] ->
                                        Generate.Types.valueFrom "Empty list?"
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
