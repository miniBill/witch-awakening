module Generate.TypePerks exposing (TypePerksModule, file)

import Elm
import Elm.Annotation
import Elm.Arg
import Elm.Declare
import Elm.Declare.Extra
import Gen.Data.TypePerk
import Gen.List
import Gen.Maybe
import Generate.Types exposing (TypesModule)
import Generate.Utils exposing (yassify)
import Parsers
import String.Extra


type alias TypePerksModule =
    { all : Elm.Expression
    }


file : TypesModule -> List ( Maybe String, Parsers.Race ) -> Elm.Declare.Module TypePerksModule
file types dlcRaces =
    Elm.Declare.module_ [ "Generated", "TypePerk" ] TypePerksModule
        |> Elm.Declare.with (all dlcRaces)
        |> Elm.Declare.Extra.withDeclarations (dlcToTypePerks types dlcRaces)


all : List ( Maybe String, Parsers.Race ) -> Elm.Declare.Value
all dlcRaces =
    dlcRaces
        |> List.sortBy (\( dlc, _ ) -> Maybe.withDefault "" dlc)
        |> List.filterMap
            (\( _, race ) ->
                if race.perk == Nothing then
                    Nothing

                else
                    Just (Elm.val (String.Extra.decapitalize (yassify race.name)))
            )
        |> Elm.list
        |> Gen.List.call_.sortBy
            (Elm.fn
                (Elm.Arg.record identity
                    |> Elm.Arg.field "dlc"
                )
                (\dlc -> Gen.Maybe.withDefault (Elm.string "") dlc)
            )
        |> Elm.withType (Elm.Annotation.list Gen.Data.TypePerk.annotation_.details)
        |> Elm.Declare.value "all"


dlcToTypePerks : TypesModule -> List ( Maybe String, Parsers.Race ) -> List Elm.Declaration
dlcToTypePerks types races =
    List.filterMap
        (\( dlcName, race ) ->
            race.perk
                |> Maybe.map
                    (\perk ->
                        Gen.Data.TypePerk.make_.details
                            { race =
                                case race.elements of
                                    [] ->
                                        Elm.apply (types.valueFrom (yassify race.name)) [ types.valueFrom "All", types.valueFrom "All" ]

                                    [ _ ] ->
                                        Elm.apply (types.valueFrom (yassify race.name)) [ types.valueFrom "All" ]

                                    _ ->
                                        types.valueFrom (yassify race.name)
                            , content = Elm.string perk.description
                            , cost = Elm.int perk.cost
                            , dlc = Elm.maybe (Maybe.map Elm.string dlcName)
                            }
                            |> Elm.declaration (yassify race.name)
                            |> Elm.expose
                    )
        )
        races
