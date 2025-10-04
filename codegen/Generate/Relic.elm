module Generate.Relic exposing (RelicsModule, file)

import Elm
import Elm.Annotation
import Elm.Declare
import Elm.Declare.Extra
import Gen.Data.Relic
import Generate.Enum as Enum exposing (Enum)
import Generate.Types exposing (TypesModule)
import Generate.Utils exposing (yassify)
import Parsers exposing (Content(..))
import String.Extra


type alias RelicsModule =
    { all : Elm.Expression
    , toString : Elm.Expression -> Elm.Expression
    , declaration : Elm.Annotation.Annotation
    }


file : TypesModule -> Enum -> List ( Maybe String, Parsers.Relic ) -> Elm.Declare.Module RelicsModule
file types enum dlcRelics =
    Elm.Declare.module_ [ "Generated", "Relic" ] RelicsModule
        |> Elm.Declare.with (all types dlcRelics)
        |> Elm.Declare.with (Enum.toString enum)
        |> Elm.Declare.with (details types)
        |> Elm.Declare.Extra.withDeclarations (dlcToRelics types dlcRelics)


all : TypesModule -> List ( Maybe String, Parsers.Relic ) -> Elm.Declare.Value
all types dlcRelics =
    dlcRelics
        |> List.sortBy (\( dlc, _ ) -> Maybe.withDefault "" dlc)
        |> List.map (\( _, relic ) -> Elm.val (String.Extra.decapitalize (yassify relic.name)))
        |> Elm.list
        |> Elm.withType (Elm.Annotation.list (details types).annotation)
        |> Elm.Declare.value "all"


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


dlcToRelics : TypesModule -> List ( Maybe String, Parsers.Relic ) -> List Elm.Declaration
dlcToRelics types relics =
    List.map
        (\( dlcName, relic ) ->
            (details types).make
                { name = types.relic.value relic.name
                , classes = Elm.list (List.map types.class.value relic.classes)
                , dlc = Elm.maybe (Maybe.map Elm.string dlcName)
                , requires = Elm.maybe (Maybe.map Elm.string relic.requires)
                , content =
                    case relic.content of
                        Parsers.Single cost description ->
                            if relic.name == "Cosmic Pearl" then
                                Gen.Data.Relic.make_.cosmicPearlContent (Elm.int cost) (Elm.string description)

                            else
                                Gen.Data.Relic.make_.single (Elm.int cost) (Elm.string description)

                        Parsers.WithCosts costs description ->
                            Gen.Data.Relic.make_.withChoices (Elm.list (List.map Elm.int costs)) (Elm.string description)

                        Parsers.WithChoices ever _ _ _ ->
                            never ever
                }
                |> Elm.declaration (yassify relic.name)
                |> Elm.expose
        )
        relics
