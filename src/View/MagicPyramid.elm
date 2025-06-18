module View.MagicPyramid exposing (view)

import Bitwise
import Dict
import Dict.Extra
import Generated.Classes
import Generated.Magic
import Generated.Types as Types
import Hex
import Html.Attributes
import List.Extra
import Svg exposing (Svg)
import Svg.Attributes
import Types exposing (RankedMagic)


view : List RankedMagic -> Svg msg
view magic =
    let
        grouped =
            magic
                |> Dict.Extra.groupBy .rank

        w : Int
        w =
            grouped
                |> Dict.foldl
                    (\_ r acc -> max acc (List.length r))
                    1

        styleNode =
            Svg.node "style"
                []
                [ Svg.text
                    """
                    rect {
                        stroke: black;
                    }

                    text {
                        fill: white;
                        cursor: default;
                        text-anchor: middle;
                        dominant-baseline: middle;
                    }
                    """
                ]
    in
    grouped
        |> Dict.toList
        |> List.concatMap viewMagic
        |> (::) styleNode
        |> Svg.svg
            [ Svg.Attributes.viewBox ("-0.05 -0.05 " ++ String.fromInt (w + 1) ++ ".1 5.1")
            , Html.Attributes.style "width" "100%"
            , Svg.Attributes.fontSize "0.5"
            , Svg.Attributes.strokeWidth "0.1"
            ]


viewMagic : ( Int, List RankedMagic ) -> List (Svg msg)
viewMagic ( rank, magics ) =
    let
        y : Float
        y =
            5 - toFloat rank

        header : Svg msg
        header =
            Svg.text_
                [ Svg.Attributes.x (String.fromFloat 0.5)
                , Svg.Attributes.y (String.fromFloat (y + 0.5))
                ]
                [ Svg.text (String.fromInt rank)
                , Svg.title []
                    [ Svg.text ("Rank " ++ String.fromInt rank)
                    ]
                ]
    in
    magics
        |> List.indexedMap
            (\i magic ->
                let
                    x : Float
                    x =
                        toFloat i + 1
                in
                [ Svg.rect
                    [ Svg.Attributes.x (String.fromFloat x)
                    , Svg.Attributes.y (String.fromFloat y)
                    , Svg.Attributes.width "1"
                    , Svg.Attributes.height "1"
                    , Generated.Magic.all
                        |> List.Extra.find (\magicDetails -> magicDetails.name == magic.name)
                        |> Maybe.andThen (\{ class } -> Maybe.map Generated.Classes.classToColor class)
                        |> Maybe.withDefault 0x00FFFFFF
                        |> (\hex -> "#" ++ Hex.toString (darken hex))
                        |> Svg.Attributes.fill
                    ]
                    [ Svg.title []
                        [ Svg.text (Types.magicToString magic.name)
                        ]
                    ]
                , Svg.text_
                    [ Svg.Attributes.x (String.fromFloat (x + 0.5))
                    , Svg.Attributes.y (String.fromFloat (y + 0.5))
                    ]
                    [ Svg.text (String.left 1 (Types.magicToString magic.name))
                    , Svg.title []
                        [ Svg.text (Types.magicToString magic.name)
                        ]
                    ]
                ]
            )
        |> List.concat
        |> (::) header


darken : Int -> Int
darken q =
    let
        go : Int -> Int
        go i =
            q
                |> Bitwise.shiftRightBy i
                |> Bitwise.and 0xFF
                |> (\v -> v * 2 // 3)
                |> Bitwise.shiftLeftBy i
    in
    go 0 + go 8 + go 16
