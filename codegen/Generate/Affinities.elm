module Generate.Affinities exposing (file)

import Elm
import Elm.Annotation
import Elm.Arg
import Elm.Case
import Gen.Data.Affinity
import Generate.Types
import Generate.Utils exposing (yassify)
import Parsers
import String.Extra


file : List ( Maybe String, Parsers.Affinity ) -> Elm.File
file dlcAffinities =
    Elm.file [ "Generated", "Affinity" ]
        (all dlcAffinities
            :: Elm.expose (affinityToColor dlcAffinities)
            :: dlcToAffinities dlcAffinities
        )


all : List ( Maybe String, Parsers.Affinity ) -> Elm.Declaration
all dlcAffinities =
    dlcAffinities
        |> List.map (\( _, affinity ) -> Elm.val (affinityToVarName affinity.name))
        |> Elm.list
        |> Elm.withType (Elm.Annotation.list Gen.Data.Affinity.annotation_.details)
        |> Elm.declaration "all"
        |> Elm.expose


affinityToColor : List ( Maybe String, Parsers.Affinity ) -> Elm.Declaration
affinityToColor dlcAffinities =
    Elm.fn (Elm.Arg.var "affinity")
        (\affinity ->
            dlcAffinities
                |> List.map
                    (\( _, affinityData ) ->
                        Elm.Case.branch
                            (Elm.Arg.customType affinityData.name ())
                            (\() -> Elm.hex affinityData.color)
                    )
                |> Elm.Case.custom affinity (Elm.Annotation.named [ "Generated", "Types" ] "Affinity")
        )
        |> Elm.declaration "affinityToColor"


dlcToAffinities : List ( Maybe String, Parsers.Affinity ) -> List Elm.Declaration
dlcToAffinities affinities =
    List.map
        (\( dlcName, affinity ) ->
            Gen.Data.Affinity.make_.details
                { name = Generate.Types.valueFrom affinity.name
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
