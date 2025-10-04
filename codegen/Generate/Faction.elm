module Generate.Faction exposing (..)

import Elm
import Elm.Annotation
import Elm.Arg
import Elm.Declare
import Elm.Declare.Extra
import Gen.Data.Faction
import Gen.List
import Gen.Maybe
import Generate.Enum as Enum exposing (Enum)
import Generate.Image exposing (ImageModule)
import Generate.Types exposing (TypesModule)
import Generate.Utils exposing (yassify)
import Parsers
import String.Extra


type alias FactionsModule =
    { all : Elm.Expression
    , toString : Elm.Expression -> Elm.Expression
    }


file : TypesModule -> ImageModule -> Enum -> List ( Maybe String, Parsers.Faction ) -> Elm.Declare.Module FactionsModule
file types image enum dlcFactions =
    Elm.Declare.module_ [ "Generated", "Faction" ] FactionsModule
        |> Elm.Declare.with (all dlcFactions)
        |> Elm.Declare.with (Enum.toString enum)
        |> Elm.Declare.Extra.withDeclarations (dlcToFactions types image dlcFactions)


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


dlcToFactions : TypesModule -> ImageModule -> List ( Maybe String, Parsers.Faction ) -> List Elm.Declaration
dlcToFactions types image factions =
    List.map
        (\( dlcName, faction ) ->
            Gen.Data.Faction.make_.details
                { name = types.faction.value faction.name
                , motto = Elm.string faction.motto
                , description = Elm.string faction.description
                , location = Elm.string faction.location
                , relations = Elm.string faction.relations
                , perk = Elm.string faction.perk
                , perkContent = Elm.string faction.perkContent
                , isHuman = Elm.bool faction.isHuman
                , dlc = Elm.maybe (Maybe.map Elm.string dlcName)
                , images = image.valueFrom ("faction" ++ yassify faction.name)
                }
                |> Elm.declaration (yassify faction.name)
                |> Elm.expose
        )
        factions
