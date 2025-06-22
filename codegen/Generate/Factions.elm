module Generate.Factions exposing (..)

import Elm
import Elm.Annotation
import Elm.Arg
import Elm.Declare
import Elm.Declare.Extra
import Gen.Data.Faction
import Gen.List
import Gen.Maybe
import Generate.Types exposing (TypesModule)
import Generate.Utils exposing (yassify)
import Parsers
import String.Extra


type alias FactionsModule =
    { all : Elm.Expression
    }


file : TypesModule -> List ( Maybe String, Parsers.Faction ) -> Elm.Declare.Module FactionsModule
file types dlcFactions =
    Elm.Declare.module_ [ "Generated", "Faction" ] FactionsModule
        |> Elm.Declare.with (all dlcFactions)
        |> Elm.Declare.Extra.withDeclarations (dlcToFactions types dlcFactions)


all : List ( Maybe String, Parsers.Faction ) -> Elm.Declare.Value
all dlcFactions =
    dlcFactions
        |> List.sortBy (\( dlc, _ ) -> Maybe.withDefault "" dlc)
        |> List.map
            (\( _, faction ) ->
                Elm.val (String.Extra.decapitalize (yassify faction.name))
            )
        |> Elm.list
        |> Gen.List.call_.sortBy
            (Elm.fn
                (Elm.Arg.record identity
                    |> Elm.Arg.field "dlc"
                )
                (\dlc -> Gen.Maybe.withDefault (Elm.string "") dlc)
            )
        |> Elm.withType (Elm.Annotation.list Gen.Data.Faction.annotation_.details)
        |> Elm.Declare.value "all"


dlcToFactions : TypesModule -> List ( Maybe String, Parsers.Faction ) -> List Elm.Declaration
dlcToFactions types factions =
    List.map
        (\( dlcName, faction ) ->
            Gen.Data.Faction.make_.details
                { name = types.valueFrom (yassify faction.name)
                , motto = Elm.string faction.motto
                , description = Elm.string faction.description
                , location = Elm.string faction.location
                , relations = Elm.string faction.relations
                , perk = Elm.string faction.perk
                , perkContent = Elm.string faction.perkContent
                , isHuman = Elm.bool faction.isHuman
                , dlc = Elm.maybe (Maybe.map Elm.string dlcName)
                , images =
                    Elm.value
                        { importFrom = [ "Images" ]
                        , name = "faction" ++ yassify faction.name
                        , annotation = Nothing
                        }
                }
                |> Elm.declaration (yassify faction.name)
                |> Elm.expose
        )
        factions
