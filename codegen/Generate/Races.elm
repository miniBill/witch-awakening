module Generate.Races exposing (RacesModule, file)

import Elm
import Elm.Annotation
import Elm.Arg
import Elm.Declare
import Elm.Declare.Extra
import Elm.Op
import Gen.Data.Race
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
        |> Elm.Declare.Extra.withDeclarations (dlcToRaces dlcRaces)


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
                    |> List.map (\( _, race ) -> Elm.val (String.Extra.decapitalize race.name))
                    |> Elm.list
                )
                (Gen.Data.Race.call_.all races)
                |> Elm.withType (Elm.Annotation.list Gen.Data.Race.annotation_.details)


dlcToRaces : List ( Maybe String, Parsers.Race ) -> List Elm.Declaration
dlcToRaces races =
    List.map
        (\( dlcName, race ) ->
            Gen.Data.Race.make_.details
                { name = Generate.Types.valueFrom race.name
                , content = Elm.string race.description
                , tank = Generate.Types.valueFrom race.manaCapacity
                , affinities = Elm.list (List.map Generate.Types.valueFrom race.elements)
                , charge = Generate.Types.valueFrom race.manaRate
                , dlc = Elm.maybe (Maybe.map Elm.string dlcName)
                }
                |> Elm.declaration (yassify race.name)
                |> Elm.expose
        )
        races
