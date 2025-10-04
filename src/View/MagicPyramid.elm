module View.MagicPyramid exposing (view)

import Color exposing (Color)
import Dict
import Dict.Extra
import Generated.Magic as Magic
import Generated.Types as Types
import Html.Attributes
import List.Extra
import TypedSvg
import TypedSvg.Attributes
import TypedSvg.Attributes.InPx
import TypedSvg.Core as TypedSvg exposing (Svg)
import TypedSvg.Events
import TypedSvg.Types exposing (Align(..), MeetOrSlice(..), Paint(..), Scale(..))
import Types exposing (IdKind(..), Msg(..), RankedMagic)
import View.Magic


view : List RankedMagic -> Svg Msg
view magic =
    let
        styleNode : Svg msg
        styleNode =
            TypedSvg.style
                []
                [ TypedSvg.text
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
            TypedSvg.defs []
                [ TypedSvg.linearGradient
                    [ TypedSvg.Attributes.id "black-gradient"
                    , TypedSvg.attribute "gradientTransform" "rotate(90)"
                    ]
                    [ TypedSvg.stop [ TypedSvg.Attributes.offset "0%", TypedSvg.Attributes.stopColor "#0004" ] []
                    , TypedSvg.stop [ TypedSvg.Attributes.offset "50%", TypedSvg.Attributes.stopColor "#0004" ] []
                    , TypedSvg.stop [ TypedSvg.Attributes.offset "100%", TypedSvg.Attributes.stopColor "#000f" ] []
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
        |> TypedSvg.svg
            [ TypedSvg.Attributes.viewBox 0 0 4 (toFloat lastY)
            , Html.Attributes.style "width" "100%"
            , TypedSvg.Attributes.InPx.fontSize 0.5
            ]


viewMagicRank : ( Int, List RankedMagic ) -> ( Int, List (Svg Msg) ) -> ( Int, List (Svg Msg) )
viewMagicRank ( rank, magics ) ( y, acc ) =
    let
        header : Svg msg
        header =
            [ TypedSvg.path
                [ TypedSvg.Attributes.fill (Paint Color.black)
                , TypedSvg.Attributes.d
                    ("m1 "
                        ++ String.fromInt (y + height)
                        ++ "-1-"
                        ++ String.fromFloat (toFloat height / 2)
                        ++ " 1-"
                        ++ String.fromFloat (toFloat height / 2)
                    )
                ]
                []
            , TypedSvg.text_
                [ TypedSvg.Attributes.InPx.x 0.6
                , TypedSvg.Attributes.InPx.y (toFloat y + toFloat height / 2)
                ]
                [ TypedSvg.text (String.fromInt rank)
                , TypedSvg.title []
                    [ TypedSvg.text ("Rank " ++ String.fromInt rank)
                    ]
                ]
            ]
                |> TypedSvg.g [ TypedSvg.Attributes.id ("Header-" ++ String.fromInt rank) ]

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
                |> TypedSvg.g [ TypedSvg.Attributes.id ("Rank " ++ String.fromInt rank) ]
    in
    ( y + height
    , rankView :: acc
    )


viewMagic : Types.Magic -> Float -> Float -> Svg Msg
viewMagic name x y =
    let
        nameString : String
        nameString =
            Magic.toString name

        title : Svg msg
        title =
            TypedSvg.title [] [ TypedSvg.text nameString ]
    in
    [ TypedSvg.image
        [ TypedSvg.Attributes.InPx.x x
        , TypedSvg.Attributes.InPx.y y
        , TypedSvg.Attributes.InPx.width 1
        , TypedSvg.Attributes.InPx.height 1
        , TypedSvg.Attributes.xlinkHref (Types.magicToImage name).src
        , TypedSvg.Attributes.preserveAspectRatio (Align ScaleMid ScaleMid) Slice
        ]
        []
    , TypedSvg.rect
        [ TypedSvg.Attributes.InPx.x x
        , TypedSvg.Attributes.InPx.y y
        , TypedSvg.Attributes.InPx.width 1
        , TypedSvg.Attributes.InPx.height 1
        , TypedSvg.Attributes.fill (Reference "black-gradient")
        , TypedSvg.Attributes.style "cursor: pointer"
        ]
        [ title ]
    , TypedSvg.circle
        [ TypedSvg.Attributes.InPx.cx (x + 0.125)
        , TypedSvg.Attributes.InPx.cy (y + 0.125)
        , TypedSvg.Attributes.InPx.r 0.125
        , Magic.all
            |> List.Extra.find (\magicDetails -> magicDetails.name == name)
            |> Maybe.map (\{ class } -> View.Magic.maybeClassToColor class)
            |> Maybe.withDefault Color.white
            |> darken
            |> Paint
            |> TypedSvg.Attributes.fill
        , TypedSvg.Attributes.style "cursor: pointer"
        ]
        [ title
        ]
    , TypedSvg.text_
        [ TypedSvg.Attributes.InPx.x (x + 0.5)
        , TypedSvg.Attributes.InPx.y (y + 0.4)
        , TypedSvg.Attributes.style "cursor: pointer"
        ]
        [ TypedSvg.text (String.left 1 nameString)
        , title
        ]
    , TypedSvg.text_
        [ TypedSvg.Attributes.InPx.x (x + 0.5)
        , TypedSvg.Attributes.InPx.y (y + 0.8)
        , TypedSvg.Attributes.InPx.fontSize 0.2
        , TypedSvg.Attributes.letterSpacing "-0.007"
        , TypedSvg.Attributes.style "cursor: pointer"
        ]
        [ TypedSvg.text nameString
        , title
        ]
    ]
        |> TypedSvg.g
            [ TypedSvg.Attributes.id ("pyramid-" ++ nameString)
            , TypedSvg.Events.onClick (ScrollTo IdKindMagic nameString)
            ]


darken : Color -> Color
darken q =
    let
        { red, green, blue, alpha } =
            Color.toRgba q
    in
    Color.rgba (red * 2 / 3) (green * 2 / 3) (blue * 2 / 3) alpha
