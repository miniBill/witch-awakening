module Generate.Quest exposing (QuestModule, file)

import Elm
import Elm.Annotation
import Elm.Declare
import Elm.Declare.Extra
import Generate.Enum as Enum exposing (Enum)
import Generate.Types exposing (TypesModule)
import Generate.Utils exposing (yassify)
import Parsers
import String.Extra


type alias QuestModule =
    { all : Elm.Expression
    , details : Elm.Annotation.Annotation
    , toString : Elm.Expression -> Elm.Expression
    , evil :
        { annotation : Elm.Annotation.Annotation
        , make_ :
            { evilNo : Elm.Expression
            , evilMaybe : Elm.Expression
            , evilYes : Elm.Expression
            }
        , case_ :
            Elm.Expression
            ->
                { evilNo : Elm.Expression
                , evilMaybe : Elm.Expression
                , evilYes : Elm.Expression
                }
            -> Elm.Expression
        }
    }


file : TypesModule -> Enum -> List ( Maybe String, Parsers.Quest ) -> Elm.Declare.Module QuestModule
file types enum dlcQuests =
    Elm.Declare.module_ [ "Generated", "Quest" ] QuestModule
        |> Elm.Declare.with (all types dlcQuests)
        |> Elm.Declare.with (details types)
        |> Elm.Declare.with (Enum.toString enum)
        |> Elm.Declare.with evil
        |> Elm.Declare.Extra.withDeclarations (dlcToQuests types dlcQuests)


details :
    TypesModule
    ->
        { annotation : Elm.Annotation.Annotation
        , declaration : Elm.Declaration
        , internal : Elm.Declare.Internal Elm.Annotation.Annotation
        , make :
            { name : Elm.Expression
            , evil : Elm.Expression
            , repeatable : Elm.Expression
            , slot : Elm.Expression
            , faction : Elm.Expression
            , threat : Elm.Expression
            , conflict : Elm.Expression
            , reward : Elm.Expression
            , requires : Elm.Expression
            , description : Elm.Expression
            , notes : Elm.Expression
            , sidebars : Elm.Expression
            , dlc : Elm.Expression
            }
            -> Elm.Expression
        }
details types =
    Elm.Declare.Extra.customRecord "Details"
        |> Elm.Declare.Extra.withField "name" .name types.quest.annotation
        |> Elm.Declare.Extra.withField "evil" .evil evil.annotation
        |> Elm.Declare.Extra.withField "repeatable" .repeatable Elm.Annotation.bool
        |> Elm.Declare.Extra.withField "slot" .slot types.slot.annotation
        |> Elm.Declare.Extra.withField "faction" .faction (Elm.Annotation.maybe types.faction.annotation)
        |> Elm.Declare.Extra.withField "threat" .threat (Elm.Annotation.maybe Elm.Annotation.int)
        |> Elm.Declare.Extra.withField "conflict" .conflict (Elm.Annotation.maybe Elm.Annotation.int)
        |> Elm.Declare.Extra.withField "reward" .reward Elm.Annotation.int
        |> Elm.Declare.Extra.withField "requires" .requires (Elm.Annotation.maybe Elm.Annotation.string)
        |> Elm.Declare.Extra.withField "description" .description Elm.Annotation.string
        |> Elm.Declare.Extra.withField "notes" .notes (Elm.Annotation.list Elm.Annotation.string)
        |> Elm.Declare.Extra.withField "sidebars" .sidebars (Elm.Annotation.list Elm.Annotation.string)
        |> Elm.Declare.Extra.withField "dlc" .dlc (Elm.Annotation.maybe Elm.Annotation.string)
        |> Elm.Declare.Extra.buildCustomRecord


type alias Evil a =
    { evilYes : a
    , evilMaybe : a
    , evilNo : a
    }


evil :
    Elm.Declare.CustomType
        { evilNo : Elm.Expression
        , evilMaybe : Elm.Expression
        , evilYes : Elm.Expression
        }
evil =
    Elm.Declare.customTypeAdvanced "Evil" { exposeConstructor = True } Evil
        |> Elm.Declare.variant0 "EvilYes" .evilYes
        |> Elm.Declare.variant0 "EvilMaybe" .evilMaybe
        |> Elm.Declare.variant0 "EvilNo" .evilNo
        |> Elm.Declare.finishCustomType


all : TypesModule -> List ( Maybe String, Parsers.Quest ) -> Elm.Declare.Value
all types dlcQuests =
    dlcQuests
        |> List.sortBy (\( dlc, _ ) -> Maybe.withDefault "" dlc)
        |> List.map
            (\( _, quest ) ->
                yassify quest.name
                    |> String.Extra.decapitalize
                    |> Elm.val
            )
        |> Elm.list
        |> Elm.withType (Elm.Annotation.list (details types).annotation)
        |> Elm.Declare.value "all"


dlcToQuests : TypesModule -> List ( Maybe String, Parsers.Quest ) -> List Elm.Declaration
dlcToQuests types quests =
    List.map
        (\( dlcName, quest ) ->
            (details types).make
                { name = types.quest.value quest.name
                , evil =
                    case quest.evil of
                        Parsers.EvilYes ->
                            evil.make_.evilYes

                        Parsers.EvilMaybe ->
                            evil.make_.evilMaybe

                        Parsers.EvilNo ->
                            evil.make_.evilNo
                , slot = types.slot.value quest.slot
                , threat = Elm.maybe (Maybe.map Elm.int quest.threat)
                , conflict = Elm.maybe (Maybe.map Elm.int quest.conflict)
                , repeatable = Elm.bool quest.repeatable
                , reward = Elm.int quest.reward
                , requires = Elm.maybe (Maybe.map Elm.string quest.requires)
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
