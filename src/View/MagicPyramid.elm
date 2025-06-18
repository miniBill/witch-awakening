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

        styleNode : Svg msg
        styleNode =
            Svg.node "style"
                []
                [ Svg.text
                    """
                    text {
                        fill: white;
                        cursor: default;
                        text-anchor: middle;
                        dominant-baseline: central;
                    }
                    """
                ]

        defsNode : Svg msg
        defsNode =
            Svg.defs []
                [ Svg.linearGradient
                    [ Svg.Attributes.id "black-gradient"
                    , Svg.Attributes.gradientTransform "rotate(90)"
                    ]
                    [ Svg.stop [ Svg.Attributes.offset "0%", Svg.Attributes.stopColor "#0004" ] []
                    , Svg.stop [ Svg.Attributes.offset "50%", Svg.Attributes.stopColor "#0004" ] []
                    , Svg.stop [ Svg.Attributes.offset "100%", Svg.Attributes.stopColor "#000f" ] []
                    ]
                ]
    in
    grouped
        |> Dict.toList
        |> List.map viewMagic
        |> (::) styleNode
        |> (::) defsNode
        |> Svg.svg
            [ Svg.Attributes.viewBox ("0 0 " ++ String.fromInt (1 + (w + rowsPerRank - 1) // rowsPerRank) ++ " " ++ String.fromInt (5 * rowsPerRank))
            , Html.Attributes.style "width" "100%"
            , Svg.Attributes.fontSize "0.5"
            ]


rowsPerRank : number
rowsPerRank =
    3


viewMagic : ( Int, List RankedMagic ) -> Svg msg
viewMagic ( rank, magics ) =
    let
        y : Float
        y =
            rowsPerRank * (5 - toFloat rank)

        header : Svg msg
        header =
            [ Svg.path
                [ Svg.Attributes.fill "black"
                , Svg.Attributes.d
                    ("m1 "
                        ++ String.fromFloat (y + rowsPerRank)
                        ++ "-1-"
                        ++ String.fromFloat (rowsPerRank / 2)
                        ++ " 1-"
                        ++ String.fromFloat (rowsPerRank / 2)
                    )
                ]
                []
            , Svg.text_
                [ Svg.Attributes.x (String.fromFloat 0.6)
                , Svg.Attributes.y (String.fromFloat (y + rowsPerRank / 2))
                ]
                [ Svg.text (String.fromInt rank)
                , Svg.title []
                    [ Svg.text ("Rank " ++ String.fromInt rank)
                    ]
                ]
            ]
                |> Svg.g [ Svg.Attributes.id ("Header-" ++ String.fromInt rank) ]
    in
    magics
        |> List.Extra.greedyGroupsOf rowsPerRank
        |> List.indexedMap
            (\i group ->
                let
                    x : Float
                    x =
                        toFloat i + 1
                in
                group
                    |> List.indexedMap
                        (\j magic ->
                            let
                                magicY : Float
                                magicY =
                                    y + toFloat j
                            in
                            [ Svg.image
                                [ Svg.Attributes.x (String.fromFloat x)
                                , Svg.Attributes.y (String.fromFloat magicY)
                                , Svg.Attributes.width "1"
                                , Svg.Attributes.height "1"
                                , Svg.Attributes.xlinkHref (Types.magicToImage magic.name).src
                                , Svg.Attributes.preserveAspectRatio "xMidYMid slice"
                                ]
                                []
                            , Svg.rect
                                [ Svg.Attributes.x (String.fromFloat x)
                                , Svg.Attributes.y (String.fromFloat magicY)
                                , Svg.Attributes.width "1"
                                , Svg.Attributes.height "1"
                                , Svg.Attributes.fill "url(#black-gradient)"
                                ]
                                [ Svg.title []
                                    [ Svg.text (Types.magicToString magic.name)
                                    ]
                                ]
                            , Svg.circle
                                [ Svg.Attributes.cx (String.fromFloat (x + 0.125))
                                , Svg.Attributes.cy (String.fromFloat (magicY + 0.125))
                                , Svg.Attributes.r "0.125"
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
                                , Svg.Attributes.y (String.fromFloat (magicY + 0.4))
                                ]
                                [ Svg.text (String.left 1 (Types.magicToString magic.name))
                                , Svg.title []
                                    [ Svg.text (Types.magicToString magic.name)
                                    ]
                                ]
                            , Svg.text_
                                [ Svg.Attributes.x (String.fromFloat (x + 0.5))
                                , Svg.Attributes.y (String.fromFloat (magicY + 0.8))
                                , Svg.Attributes.fontSize "0.2"
                                , Svg.Attributes.letterSpacing "-0.007"
                                ]
                                [ Svg.text (Types.magicToString magic.name)
                                , Svg.title []
                                    [ Svg.text (Types.magicToString magic.name)
                                    ]
                                ]
                            ]
                                |> Svg.g [ Svg.Attributes.id (Types.magicToString magic.name) ]
                        )
            )
        |> List.concat
        |> (::) header
        |> Svg.g [ Svg.Attributes.id ("Rank " ++ String.fromInt rank) ]


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
