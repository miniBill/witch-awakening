module Generate.Races exposing (RacesModule, file)

import Elm
import Elm.Annotation
import Elm.Arg
import Elm.Declare
import Elm.Declare.Extra
import Elm.Op
import Gen.Data.Race
import Gen.List
import Gen.Maybe
import Generate.Types exposing (TypesModule)
import Generate.Utils exposing (yassify)
import Parsers
import String.Extra


type alias RacesModule =
    { all : Elm.Expression -> Elm.Expression
    }


file : TypesModule -> List ( Maybe String, Parsers.Race ) -> Elm.Declare.Module RacesModule
file types dlcRaces =
    Elm.Declare.module_ [ "Generated", "Race" ] RacesModule
        |> Elm.Declare.with (all types dlcRaces)
        |> Elm.Declare.Extra.withDeclarations (dlcToRaces types dlcRaces)


all : TypesModule -> List ( Maybe String, Parsers.Race ) -> Elm.Declare.Function (Elm.Expression -> Elm.Expression)
all types dlcRaces =
    Elm.Declare.fn "all"
        (Elm.Arg.varWith "races"
            (Elm.Annotation.list types.race.annotation)
        )
    <|
        \races ->
            Elm.Op.append
                (dlcRaces
                    |> List.sortBy (\( dlc, _ ) -> Maybe.withDefault "" dlc)
                    |> List.map (\( _, race ) -> Elm.val (String.Extra.decapitalize race.name))
                    |> Elm.list
                )
                (Gen.Data.Race.call_.all races)
                |> Gen.List.call_.sortBy
                    (Elm.fn
                        (Elm.Arg.record identity
                            |> Elm.Arg.field "dlc"
                        )
                        (\dlc -> Gen.Maybe.withDefault (Elm.string "") dlc)
                    )
                |> Elm.withType (Elm.Annotation.list Gen.Data.Race.annotation_.details)


dlcToRaces : TypesModule -> List ( Maybe String, Parsers.Race ) -> List Elm.Declaration
dlcToRaces types races =
    List.map
        (\( dlcName, race ) ->
            Gen.Data.Race.make_.details
                { name = types.valueFrom race.name
                , content = Elm.string race.description
                , tank = types.valueFrom race.manaCapacity
                , affinities = Elm.list (List.map types.valueFrom race.elements)
                , charge = types.valueFrom race.manaRate
                , dlc = Elm.maybe (Maybe.map Elm.string dlcName)
                }
                |> Elm.declaration (yassify race.name)
                |> Elm.expose
        )
        races
