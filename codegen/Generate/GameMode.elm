module Generate.GameMode exposing (file)

import Elm
import Elm.Annotation
import Elm.Declare
import Elm.Declare.Extra
import Generate.Enum as Enum exposing (Enum)
import Generate.Types exposing (TypesModule)
import Generate.Utils exposing (yassify)
import Parsers
import String.Extra


type alias GameModesModule =
    { all : Elm.Expression
    , gameModeDetails : Elm.Annotation.Annotation
    , toString : Elm.Expression -> Elm.Expression
    }


file : TypesModule -> Enum -> List ( Maybe String, Parsers.GameMode ) -> Elm.Declare.Module GameModesModule
file types enum dlcGameModes =
    Elm.Declare.module_ [ "Generated", "GameMode" ] GameModesModule
        |> Elm.Declare.with (all types dlcGameModes)
        |> Elm.Declare.with (gameModeDetails types)
        |> Elm.Declare.with (Enum.toString enum)
        |> Elm.Declare.Extra.withDeclarations (dlcToGameModes types dlcGameModes)


gameModeDetails :
    TypesModule
    ->
        { annotation : Elm.Annotation.Annotation
        , declaration : Elm.Declaration
        , internal : Elm.Declare.Internal Elm.Annotation.Annotation
        , make :
            { content : Elm.Expression
            , dlc : Elm.Expression
            , name : Elm.Expression
            }
            -> Elm.Expression
        }
gameModeDetails types =
    Elm.Declare.Extra.customRecord "Details"
        |> Elm.Declare.Extra.withField "name" .name types.gameMode.annotation
        |> Elm.Declare.Extra.withField "dlc" .dlc (Elm.Annotation.maybe Elm.Annotation.string)
        |> Elm.Declare.Extra.withField "content" .content Elm.Annotation.string
        |> Elm.Declare.Extra.buildCustomRecord


all : TypesModule -> List ( Maybe String, Parsers.GameMode ) -> Elm.Declare.Value
all types dlcGameModes =
    dlcGameModes
        |> List.map
            (\( _, gameMode ) ->
                yassify gameMode.name
                    |> String.Extra.decapitalize
                    |> Elm.val
            )
        |> Elm.list
        |> Elm.withType (Elm.Annotation.list (gameModeDetails types).annotation)
        |> Elm.Declare.value "all"


dlcToGameModes : TypesModule -> List ( Maybe String, Parsers.GameMode ) -> List Elm.Declaration
dlcToGameModes types gameModes =
    List.map
        (\( dlcName, gameMode ) ->
            (gameModeDetails types).make
                { name = types.gameMode.value gameMode.name
                , dlc = Elm.maybe (Maybe.map Elm.string dlcName)
                , content = Elm.string gameMode.description
                }
                |> Elm.declaration (yassify gameMode.name)
                |> Elm.expose
        )
        gameModes
