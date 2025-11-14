module Generated.CosmicPearlData exposing (toString)

import Generated.Affinity as Affinity
import Generated.Types exposing (CosmicPearlData)


toString : CosmicPearlData -> String
toString data =
    let
        change : String
        change =
            data.change
                |> List.map
                    (\( before, after ) ->
                        Affinity.toString before ++ Affinity.toString after
                    )
                |> String.join ","

        add : String
        add =
            data.add
                |> List.map Affinity.toString
                |> String.join ","
    in
    change ++ "-" ++ add
