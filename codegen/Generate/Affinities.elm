module Generate.Affinities exposing (file)

import Elm
import Elm.Annotation
import Elm.Arg
import Elm.Case
import Elm.Declare
import Elm.Declare.Extra
import Gen.Data.Affinity
import Generate.Types exposing (TypesModule)
import Generate.Utils exposing (yassify)
import Parsers
import String.Extra


type alias AffinitiesModule =
    { all : Elm.Expression
    , affinityToColor : Elm.Expression -> Elm.Expression
    }


file : TypesModule -> List ( Maybe String, Parsers.Affinity ) -> Elm.Declare.Module AffinitiesModule
file types dlcAffinities =
    Elm.Declare.module_ [ "Generated", "Affinity" ] AffinitiesModule
        |> Elm.Declare.with (all dlcAffinities)
        |> Elm.Declare.with (affinityToColor types dlcAffinities)
        |> Elm.Declare.Extra.withDeclarations (dlcToAffinities types dlcAffinities)


all : List ( Maybe String, Parsers.Affinity ) -> Elm.Declare.Value
all dlcAffinities =
    dlcAffinities
        |> List.sortBy (\( dlc, _ ) -> Maybe.withDefault "" dlc)
        |> List.map (\( _, affinity ) -> Elm.val (affinityToVarName affinity.name))
        |> Elm.list
        |> Elm.withType (Elm.Annotation.list Gen.Data.Affinity.annotation_.details)
        |> Elm.Declare.value "all"


affinityToColor : TypesModule -> List ( Maybe String, Parsers.Affinity ) -> Elm.Declare.Function (Elm.Expression -> Elm.Expression)
affinityToColor types dlcAffinities =
    Elm.Declare.fn "affinityToColor"
        (Elm.Arg.var "affinity")
        (\affinity ->
            dlcAffinities
                |> List.map
                    (\( _, affinityData ) ->
                        Elm.Case.branch
                            (types.affinity.argWith affinityData.name [])
                            (\_ -> Elm.hex affinityData.color)
                    )
                |> Elm.Case.custom affinity types.affinity.annotation
        )


dlcToAffinities : TypesModule -> List ( Maybe String, Parsers.Affinity ) -> List Elm.Declaration
dlcToAffinities types affinities =
    List.map
        (\( dlcName, affinity ) ->
            Gen.Data.Affinity.make_.details
                { name = types.affinity.value affinity.name
                , dlc = Elm.maybe (Maybe.map Elm.string dlcName)
                }
                |> Elm.declaration (affinityToVarName affinity.name)
                |> Elm.expose
        )
        affinities


affinityToVarName : String -> String
affinityToVarName affinity =
    case affinity of
        "All" ->
            "all_"

        _ ->
            String.Extra.decapitalize (yassify affinity)
