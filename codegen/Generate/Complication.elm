module Generate.Complication exposing (file)

import Elm
import Elm.Annotation
import Elm.Declare
import Elm.Declare.Extra
import Gen.Data.Complication
import Generate.Enum as Enum exposing (Enum)
import Generate.Types exposing (TypesModule)
import Generate.Utils exposing (yassify)
import Parsers exposing (Content(..))
import String.Extra


type alias ComplicationModule =
    { all : Elm.Expression
    , toString : Elm.Expression -> Elm.Expression
    }


file : TypesModule -> Enum -> List ( Maybe String, Parsers.Complication ) -> Elm.Declare.Module ComplicationModule
file types enum dlcComplications =
    Elm.Declare.module_ [ "Generated", "Complication" ] ComplicationModule
        |> Elm.Declare.with (all dlcComplications)
        |> Elm.Declare.with (Enum.toString enum)
        |> Elm.Declare.Extra.withDeclarations (dlcToComplications types dlcComplications)


all : List ( Maybe String, Parsers.Complication ) -> Elm.Declare.Value
all dlcComplications =
    dlcComplications
        |> List.sortBy (\( dlc, _ ) -> Maybe.withDefault "" dlc)
        |> List.map (\( _, complication ) -> Elm.val (String.Extra.decapitalize (yassify complication.name)))
        |> Elm.list
        |> Elm.withType (Elm.Annotation.list Gen.Data.Complication.annotation_.details)
        |> Elm.Declare.value "all"


dlcToComplications : TypesModule -> List ( Maybe String, Parsers.Complication ) -> List Elm.Declaration
dlcToComplications types complications =
    List.map
        (\( dlcName, complication ) ->
            Gen.Data.Complication.make_.details
                { name = types.complication.value complication.name
                , class = Elm.maybe (Maybe.map types.class.value complication.class)
                , category = Elm.maybe (Maybe.map types.complicationCategory.value complication.category)
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
