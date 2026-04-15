module Generate.Affinity exposing (file)

import Elm
import Elm.Annotation
import Elm.Arg
import Elm.Case
import Elm.Declare
import Elm.Declare.Extra
import Generate.Enum as Enum exposing (Enum)
import Generate.Types exposing (TypesModule)
import Generate.Utils as Utils exposing (yassify)
import Parsers
import String.Extra


type alias AffinitiesModule =
    { all : Elm.Expression
    , toColor : Elm.Expression -> Elm.Expression
    , toString : Elm.Expression -> Elm.Expression
    , isSelectable : Elm.Expression -> Elm.Expression
    , details : Elm.Annotation.Annotation
    }


file : TypesModule -> Enum -> List ( Maybe String, Parsers.Affinity ) -> Elm.Declare.Module AffinitiesModule
file types enum dlcAffinities =
    Elm.Declare.module_ [ "Generated", "Affinity" ] AffinitiesModule
        |> Elm.Declare.with (all types dlcAffinities)
        |> Elm.Declare.with (toColor types dlcAffinities)
        |> Elm.Declare.with (Enum.toString enum)
        |> Elm.Declare.with (isSelectable types dlcAffinities)
        |> Elm.Declare.with (details types)
        |> Elm.Declare.withDeclarations (dlcToAffinities types dlcAffinities)


all : TypesModule -> List ( Maybe String, Parsers.Affinity ) -> Elm.Declare.Value
all types dlcAffinities =
    dlcAffinities
        |> List.sortBy (\( dlc, _ ) -> Maybe.withDefault "" dlc)
        |> List.map (\( _, affinity ) -> Elm.val (affinityToVarName affinity.name))
        |> Elm.list
        |> Elm.withType (Elm.Annotation.list (details types).annotation)
        |> Elm.Declare.value "all"


isSelectable : TypesModule -> List ( Maybe String, Parsers.Affinity ) -> Elm.Declare.Function (Elm.Expression -> Elm.Expression)
isSelectable types dlcAffinities =
    Elm.Declare.fn "isSelectable"
        (Elm.Arg.var "affinity")
        (\affinity ->
            dlcAffinities
                |> List.map
                    (\( _, affinityData ) ->
                        Elm.Case.branch
                            (types.affinity.argWith affinityData.name [])
                            (\_ -> Elm.bool affinityData.selectable)
                    )
                |> Elm.Case.custom affinity types.affinity.annotation
        )


toColor : TypesModule -> List ( Maybe String, Parsers.Affinity ) -> Elm.Declare.Function (Elm.Expression -> Elm.Expression)
toColor types dlcAffinities =
    Elm.Declare.fn "toColor"
        (Elm.Arg.var "affinity")
        (\affinity ->
            dlcAffinities
                |> List.map
                    (\( _, affinityData ) ->
                        Elm.Case.branch
                            (types.affinity.argWith affinityData.name [])
                            (\_ -> Utils.color affinityData.color)
                    )
                |> Elm.Case.custom affinity types.affinity.annotation
        )


details :
    TypesModule
    ->
        Elm.Declare.Extra.Record
            { name : Elm.Expression
            , dlc : Elm.Expression
            }
details types =
    Elm.Declare.record "Details"
        |> Elm.Declare.withField "name" .name types.affinity.annotation
        |> Elm.Declare.withField "dlc" .dlc (Elm.Annotation.maybe Elm.Annotation.string)
        |> Elm.Declare.buildRecord


dlcToAffinities : TypesModule -> List ( Maybe String, Parsers.Affinity ) -> List Elm.Declaration
dlcToAffinities types affinities =
    List.map
        (\( dlcName, affinity ) ->
            (details types).make
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
