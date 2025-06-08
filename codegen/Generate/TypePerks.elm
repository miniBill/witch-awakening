module Generate.TypePerks exposing (file)

import Elm
import Elm.Annotation
import Elm.Declare
import Elm.Declare.Extra
import Elm.Op
import Gen.Data.TypePerk
import Generate.Utils exposing (valueFromTypes, yassify)
import Parsers
import String.Extra


type alias TypePerksModule =
    { all : Elm.Expression
    }


file : List ( Maybe String, Parsers.Race ) -> Elm.Declare.Module TypePerksModule
file dlcRaces =
    Elm.Declare.module_ [ "Generated", "TypePerk" ] TypePerksModule
        |> Elm.Declare.with (all dlcRaces)
        |> Elm.Declare.Extra.withDeclarations (dlcToTypePerks dlcRaces)


all : List ( Maybe String, Parsers.Race ) -> Elm.Declare.Value
all dlcRaces =
    Elm.Op.append
        (dlcRaces
            |> List.filterMap
                (\( _, race ) ->
                    if race.perk == Nothing then
                        Nothing

                    else
                        Just (Elm.val (String.Extra.decapitalize race.name))
                )
            |> Elm.list
        )
        Gen.Data.TypePerk.all
        |> Elm.withType (Elm.Annotation.list Gen.Data.TypePerk.annotation_.details)
        |> Elm.Declare.value "all"


dlcToTypePerks : List ( Maybe String, Parsers.Race ) -> List Elm.Declaration
dlcToTypePerks races =
    List.filterMap
        (\( dlcName, race ) ->
            race.perk
                |> Maybe.map
                    (\perk ->
                        Gen.Data.TypePerk.make_.details
                            { race = valueFromTypes race.name
                            , content = Elm.string perk.description
                            , cost = Elm.int perk.cost
                            , dlc = Elm.maybe (Maybe.map Elm.string dlcName)
                            }
                            |> Elm.declaration (yassify race.name)
                            |> Elm.expose
                    )
        )
        races
