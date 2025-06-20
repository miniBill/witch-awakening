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
import Svg.Events
import Types exposing (Msg(..), RankedMagic)


view : List RankedMagic -> Svg Msg
view magic =
    let
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

        ( lastY, nodes ) =
            magic
                |> Dict.Extra.groupBy .rank
                |> Dict.toList
                |> List.reverse
                |> List.foldl viewMagicRank ( 0, [] )
    in
    nodes
        |> (::) styleNode
        |> (::) defsNode
        |> Svg.svg
            [ Svg.Attributes.viewBox ("0 0 4 " ++ String.fromInt lastY)
            , Html.Attributes.style "width" "100%"
            , Svg.Attributes.fontSize "0.5"
            ]


viewMagicRank : ( Int, List RankedMagic ) -> ( Int, List (Svg Msg) ) -> ( Int, List (Svg Msg) )
viewMagicRank ( rank, magics ) ( y, acc ) =
    let
        header : Svg msg
        header =
            [ Svg.path
                [ Svg.Attributes.fill "black"
                , Svg.Attributes.d
                    ("m1 "
                        ++ String.fromInt (y + height)
                        ++ "-1-"
                        ++ String.fromFloat (toFloat height / 2)
                        ++ " 1-"
                        ++ String.fromFloat (toFloat height / 2)
                    )
                ]
                []
            , Svg.text_
                [ Svg.Attributes.x (String.fromFloat 0.6)
                , Svg.Attributes.y (String.fromFloat (toFloat y + toFloat height / 2))
                ]
                [ Svg.text (String.fromInt rank)
                , Svg.title []
                    [ Svg.text ("Rank " ++ String.fromInt rank)
                    ]
                ]
            ]
                |> Svg.g [ Svg.Attributes.id ("Header-" ++ String.fromInt rank) ]

        height : Int
        height =
            List.length groups

        groups : List (List RankedMagic)
        groups =
            magics
                |> List.Extra.greedyGroupsOf 3

        rankView : Svg Msg
        rankView =
            groups
                |> List.indexedMap
                    (\i group ->
                        let
                            groupY : Float
                            groupY =
                                toFloat (y + i)
                        in
                        group
                            |> List.indexedMap
                                (\j magic ->
                                    let
                                        x : Float
                                        x =
                                            toFloat j + 1
                                    in
                                    viewMagic magic.name x groupY
                                )
                    )
                |> List.concat
                |> (::) header
                |> Svg.g [ Svg.Attributes.id ("Rank " ++ String.fromInt rank) ]
    in
    ( y + height
    , rankView :: acc
    )


viewMagic : Types.Magic -> Float -> Float -> Svg Msg
viewMagic name x y =
    let
        nameString : String
        nameString =
            Types.magicToString name

        title : Svg msg
        title =
            Svg.title [] [ Svg.text nameString ]
    in
    [ Svg.image
        [ Svg.Attributes.x (String.fromFloat x)
        , Svg.Attributes.y (String.fromFloat y)
        , Svg.Attributes.width "1"
        , Svg.Attributes.height "1"
        , Svg.Attributes.xlinkHref (Types.magicToImage name).src
        , Svg.Attributes.preserveAspectRatio "xMidYMid slice"
        ]
        []
    , Svg.rect
        [ Svg.Attributes.x (String.fromFloat x)
        , Svg.Attributes.y (String.fromFloat y)
        , Svg.Attributes.width "1"
        , Svg.Attributes.height "1"
        , Svg.Attributes.fill "url(#black-gradient)"
        , Svg.Attributes.style "cursor: pointer"
        ]
        [ title ]
    , Svg.circle
        [ Svg.Attributes.cx (String.fromFloat (x + 0.125))
        , Svg.Attributes.cy (String.fromFloat (y + 0.125))
        , Svg.Attributes.r "0.125"
        , Generated.Magic.all
            |> List.Extra.find (\magicDetails -> magicDetails.name == name)
            |> Maybe.andThen (\{ class } -> Maybe.map Generated.Classes.classToColor class)
            |> Maybe.withDefault 0x00FFFFFF
            |> (\hex -> "#" ++ Hex.toString (darken hex))
            |> Svg.Attributes.fill
        , Svg.Attributes.style "cursor: pointer"
        ]
        [ title
        ]
    , Svg.text_
        [ Svg.Attributes.x (String.fromFloat (x + 0.5))
        , Svg.Attributes.y (String.fromFloat (y + 0.4))
        , Svg.Attributes.style "cursor: pointer"
        ]
        [ Svg.text (String.left 1 nameString)
        , title
        ]
    , Svg.text_
        [ Svg.Attributes.x (String.fromFloat (x + 0.5))
        , Svg.Attributes.y (String.fromFloat (y + 0.8))
        , Svg.Attributes.fontSize "0.2"
        , Svg.Attributes.letterSpacing "-0.007"
        , Svg.Attributes.style "cursor: pointer"
        ]
        [ Svg.text nameString
        , title
        ]
    ]
        |> Svg.g
            [ Svg.Attributes.id ("pyramid-" ++ nameString)
            , Svg.Events.onClick (ScrollTo nameString)
            ]


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
