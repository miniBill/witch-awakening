module Generate.Complications exposing (file)

import Elm
import Elm.Annotation
import Gen.Data.Complication
import Generate.Types
import Generate.Utils exposing (yassify)
import Parsers exposing (Content(..))
import String.Extra


file : List ( Maybe String, Parsers.Complication ) -> Elm.File
file dlcComplications =
    Elm.file [ "Generated", "Complication" ]
        (all dlcComplications
            :: dlcToComplications dlcComplications
        )


all : List ( Maybe String, Parsers.Complication ) -> Elm.Declaration
all dlcComplications =
    dlcComplications
        |> List.map (\( _, complication ) -> Elm.val (String.Extra.decapitalize (yassify complication.name)))
        |> Elm.list
        |> Elm.withType (Elm.Annotation.list Gen.Data.Complication.annotation_.details)
        |> Elm.declaration "all"
        |> Elm.expose


dlcToComplications : List ( Maybe String, Parsers.Complication ) -> List Elm.Declaration
dlcToComplications complications =
    List.map
        (\( dlcName, complication ) ->
            Gen.Data.Complication.make_.details
                { name = Generate.Types.value complication.name
                , class = Elm.maybe (Maybe.map Generate.Types.value complication.class)
                , category = Elm.maybe (Maybe.map Generate.Types.value complication.category)
                , content =
                    case complication.content of
                        Parsers.Single cost description ->
                            Gen.Data.Complication.make_.single (Elm.int cost) (Elm.string description)

                        Parsers.WithCosts costs description ->
                            Gen.Data.Complication.make_.withGains (Elm.list (List.map Elm.int costs)) (Elm.string description)

                        Parsers.WithChoices () before choices after ->
                            if complication.isTiered then
                                Gen.Data.Complication.make_.withTiers
                                    (Elm.string before)
                                    (choices
                                        |> List.map
                                            (\( choice, cost ) -> Elm.tuple (Elm.string choice) (Elm.int cost))
                                        |> Elm.list
                                    )
                                    (Elm.string after)

                            else
                                Gen.Data.Complication.make_.withChoices
                                    (Elm.string before)
                                    (choices
                                        |> List.map
                                            (\( choice, cost ) -> Elm.tuple (Elm.string choice) (Elm.int cost))
                                        |> Elm.list
                                    )
                                    (Elm.string after)
                , dlc = Elm.maybe (Maybe.map Elm.string dlcName)
                }
                |> Elm.declaration (yassify complication.name)
                |> Elm.expose
        )
        complications
