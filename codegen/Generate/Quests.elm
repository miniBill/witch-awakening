module Generate.Quests exposing (QuestModule, file)

import Elm
import Elm.Annotation
import Elm.Declare
import Elm.Declare.Extra
import Gen.Data.Quest
import Generate.Types exposing (TypesModule)
import Generate.Utils exposing (yassify)
import Parsers exposing (Score(..))
import String.Extra


type alias QuestModule =
    { all : Elm.Expression
    }


file : TypesModule -> List ( Maybe String, Parsers.Quest ) -> Elm.Declare.Module QuestModule
file types dlcQuests =
    Elm.Declare.module_ [ "Generated", "Quest" ] QuestModule
        |> Elm.Declare.with (all dlcQuests)
        |> Elm.Declare.Extra.withDeclarations (dlcToQuests types dlcQuests)


all : List ( Maybe String, Parsers.Quest ) -> Elm.Declare.Value
all dlcQuests =
    dlcQuests
        |> List.sortBy (\( dlc, _ ) -> Maybe.withDefault "" dlc)
        |> List.map (\( _, quest ) -> Elm.val (String.Extra.decapitalize (yassify quest.name)))
        |> Elm.list
        |> Elm.withType (Elm.Annotation.list Gen.Data.Quest.annotation_.details)
        |> Elm.Declare.value "all"


dlcToQuests : TypesModule -> List ( Maybe String, Parsers.Quest ) -> List Elm.Declaration
dlcToQuests types quests =
    List.map
        (\( dlcName, quest ) ->
            Gen.Data.Quest.make_.details
                { name = types.quest.value quest.name
                , evil =
                    Elm.value
                        { importFrom = [ "Data", "Quest" ]
                        , name = quest.evil
                        , annotation = Nothing
                        }
                , slot = types.slot.value quest.slot
                , threat = Elm.maybe (Maybe.map Elm.int quest.threat)
                , conflict = Elm.maybe (Maybe.map Elm.int quest.conflict)
                , repeatable = Elm.bool quest.repeatable
                , reward = Elm.int quest.reward
                , faction = Elm.maybe (Maybe.map types.faction.value quest.faction)
                , description = Elm.string quest.description
                , sidebars = Elm.list (List.map Elm.string quest.sidebars)
                , notes = Elm.list (List.map Elm.string quest.notes)
                , dlc = Elm.maybe (Maybe.map Elm.string dlcName)
                }
                |> Elm.declaration (yassify quest.name)
                |> Elm.expose
        )
        quests
