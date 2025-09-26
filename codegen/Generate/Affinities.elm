module Generate.Affinities exposing (file)

import Elm
import Elm.Annotation
import Elm.Arg
import Elm.Case
import Elm.Declare
import Elm.Declare.Extra
import Generate.Types exposing (TypesModule)
import Generate.Utils exposing (yassify)
import Parsers
import String.Extra


type alias AffinitiesModule =
    { all : Elm.Expression
    , affinityToColor : Elm.Expression -> Elm.Expression
    , affinityDetails : Elm.Annotation.Annotation
    }


file : TypesModule -> List ( Maybe String, Parsers.Affinity ) -> Elm.Declare.Module AffinitiesModule
file types dlcAffinities =
    Elm.Declare.module_ [ "Generated", "Affinity" ] AffinitiesModule
        |> Elm.Declare.with (all types dlcAffinities)
        |> Elm.Declare.with (affinityToColor types dlcAffinities)
        |> Elm.Declare.with (affinityDetails types)
        |> Elm.Declare.Extra.withDeclarations (dlcToAffinities types dlcAffinities)


all : TypesModule -> List ( Maybe String, Parsers.Affinity ) -> Elm.Declare.Value
all types dlcAffinities =
    dlcAffinities
        |> List.sortBy (\( dlc, _ ) -> Maybe.withDefault "" dlc)
        |> List.map (\( _, affinity ) -> Elm.val (affinityToVarName affinity.name))
        |> Elm.list
        |> Elm.withType (Elm.Annotation.list (affinityDetails types).annotation)
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


affinityDetails :
    TypesModule
    ->
        { annotation : Elm.Annotation.Annotation
        , declaration : Elm.Declaration
        , internal : Elm.Declare.Internal Elm.Annotation.Annotation
        , make :
            { name : Elm.Expression
            , dlc : Elm.Expression
            }
            -> Elm.Expression
        }
affinityDetails types =
    Elm.Declare.Extra.customRecord "Details"
        |> Elm.Declare.Extra.withField "name" .name types.affinity.annotation
        |> Elm.Declare.Extra.withField "dlc" .dlc (Elm.Annotation.maybe Elm.Annotation.string)
        |> Elm.Declare.Extra.buildCustomRecord


dlcToAffinities : TypesModule -> List ( Maybe String, Parsers.Affinity ) -> List Elm.Declaration
dlcToAffinities types affinities =
    List.map
        (\( dlcName, affinity ) ->
            (affinityDetails types).make
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
