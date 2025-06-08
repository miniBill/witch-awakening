module Generate.Races exposing (file)

import Elm
import Elm.Annotation
import Elm.Arg
import Elm.Declare
import Elm.Declare.Extra
import Elm.Op
import Gen.Data.Race
import Generate.Utils exposing (valueFromTypes, yassify)
import Parsers
import String.Extra


type alias RacesModule =
    { all : Elm.Expression -> Elm.Expression
    }


file : List ( Maybe String, Parsers.Race ) -> Elm.Declare.Module RacesModule
file dlcRaces =
    Elm.Declare.module_ [ "Generated", "Race" ] RacesModule
        |> Elm.Declare.with (all dlcRaces)
        |> Elm.Declare.Extra.withDeclarations (dlcToRaces dlcRaces)


all : List ( Maybe String, Parsers.Race ) -> Elm.Declare.Function (Elm.Expression -> Elm.Expression)
all dlcRaces =
    Elm.Declare.fn "all"
        (Elm.Arg.varWith "races"
            (Elm.Annotation.list (Elm.Annotation.named [ "Generated", "Types" ] "Race"))
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
                { name = valueFromTypes race.name
                , content = Elm.string race.description
                , tank = valueFromTypes race.manaCapacity
                , affinities = Elm.list (List.map valueFromTypes race.elements)
                , charge = valueFromTypes race.manaRate
                , dlc = Elm.maybe (Maybe.map Elm.string dlcName)
                }
                |> Elm.declaration (yassify race.name)
                |> Elm.expose
        )
        races
