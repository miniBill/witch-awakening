module ColorWheel exposing (Model, Msg, main)

import Browser
import Color exposing (Color)
import Color.Oklch as Oklch exposing (Oklch)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Html.Events.Extra.Pointer as Pointer
import List.Extra
import Round
import TypedSvg exposing (circle, defs, linearGradient, rect, stop, svg, text_)
import TypedSvg.Attributes exposing (dominantBaseline, fill, id, offset, stopColor, stroke, textAnchor, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, fontSize, height, r, strokeWidth, width, x, x1, x2, y, y1, y2)
import TypedSvg.Core exposing (Attribute, Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), DominantBaseline(..), Paint(..))


type alias Model =
    { input : String
    , mouse : Maybe ( Float, Float )
    }


type Msg
    = Input String
    | Pointer (Maybe ( Float, Float ))


main : Program () Model Msg
main =
    Browser.sandbox
        { init =
            { input = ""
            , mouse = Nothing
            }
        , view = view
        , update = update
        }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Input input ->
            { model | input = input }

        Pointer mouse ->
            { model | mouse = mouse }


svgElementWidth : number
svgElementWidth =
    800


view : Model -> Html Msg
view model =
    Html.div
        [ Html.Attributes.style "display" "flex"
        , Html.Attributes.style "gap" "8px"
        ]
        [ Html.textarea
            [ Html.Attributes.value model.input
            , Html.Events.onInput Input
            ]
            []
        , let
            circles : List (Svg msg)
            circles =
                model.input
                    |> String.split "\n"
                    |> List.Extra.removeWhen String.isEmpty
                    |> List.concatMap colorStringToCircle
          in
          (viewPointer model.mouse ++ circles)
            |> svg
                [ viewBox -100 -100 200 200
                , Html.Attributes.style "border" "1px solid black"
                , Html.Attributes.style "width" (String.fromFloat svgElementWidth ++ "px")
                , Pointer.onLeave (\_ -> Pointer Nothing)
                , Pointer.onMove (\ev -> ev |> relativePosition |> Just |> Pointer)
                ]
        ]


relativePosition : Pointer.Event -> ( Float, Float )
relativePosition event =
    let
        ( x, y ) =
            event.pointer.offsetPos
    in
    ( x / svgElementWidth * 2 - 1
    , y / svgElementWidth * 2 - 1
    )


viewPointer : Maybe ( Float, Float ) -> List (Svg Msg)
viewPointer pointer =
    case pointer of
        Nothing ->
            []

        Just ( px, py ) ->
            let
                h : Float
                h =
                    atan2 py px / (pi * 2)

                c : Float
                c =
                    sqrt (px ^ 2 + py ^ 2) / 3

                stopCount : number
                stopCount =
                    100

                labelCount : number
                labelCount =
                    20

                ( stops, labels ) =
                    List.range 0 stopCount
                        |> List.map
                            (\i ->
                                let
                                    l : Float
                                    l =
                                        toFloat i / toFloat stopCount

                                    oklch : Oklch
                                    oklch =
                                        Oklch.oklch l c h

                                    gradientStop : Svg msg
                                    gradientStop =
                                        stop
                                            [ offset (String.fromFloat (l * 100) ++ "%")
                                            , oklch
                                                |> Oklch.toCssString
                                                |> stopColor
                                            ]
                                            []

                                    label : List (Svg msg)
                                    label =
                                        if modBy (stopCount // labelCount) i == 0 then
                                            [ text_
                                                [ x -80
                                                , y (l * 180 - 90)
                                                , fontSize 4
                                                , dominantBaseline DominantBaselineMiddle
                                                , textAnchor AnchorMiddle
                                                ]
                                                [ oklch
                                                    |> Oklch.toColor
                                                    |> colorToHex
                                                    |> Maybe.withDefault "???"
                                                    |> text
                                                ]
                                            ]

                                        else
                                            []
                                in
                                ( gradientStop, label )
                            )
                        |> List.unzip

                gradient : Svg msg
                gradient =
                    defs []
                        [ linearGradient
                            [ id "pointer-gradient"
                            , x1 0
                            , y1 0
                            , x2 0
                            , y2 1
                            ]
                            stops
                        ]
            in
            gradient
                :: rect
                    [ x -100
                    , y -90
                    , width 10
                    , height 180
                    , fill (Reference "pointer-gradient")
                    ]
                    []
                :: List.concat labels
                ++ colorToCircle [ r 8 ] (Oklch.oklch 0.5 c h)


colorToHex : Color -> Maybe String
colorToHex color =
    let
        rgba : { red : Float, green : Float, blue : Float, alpha : Float }
        rgba =
            Color.toRgba color

        f : Float -> Maybe String
        f v =
            (v * 255)
                |> round
                |> clamp 0 255
                |> toHexString
    in
    Maybe.map3 (\r g b -> r ++ g ++ b) (f rgba.red) (f rgba.green) (f rgba.blue)


toHexString : Int -> Maybe String
toHexString i =
    let
        hs : Maybe Char
        hs =
            fromHexDigit (i // 16)

        ls : Maybe Char
        ls =
            fromHexDigit (modBy 16 i)
    in
    Maybe.map2 (\h l -> String.fromList [ h, l ]) hs ls


colorStringToCircle : String -> List (Svg msg)
colorStringToCircle colorString =
    case toColor colorString of
        Nothing ->
            [ text ("Invalid color: " ++ colorString) ]

        Just c ->
            colorToCircle [] (Oklch.fromColor c)


colorToCircle : List (Attribute msg) -> Oklch -> List (Svg msg)
colorToCircle attrs oklch =
    let
        circleX : Float
        circleX =
            oklch.chroma * 300 * cos (turns oklch.hue)

        circleY : Float
        circleY =
            oklch.chroma * 300 * sin (turns oklch.hue)
    in
    [ circle
        ([ r 5
         , cx circleX
         , cy circleY
         , Oklch.toColor oklch
            |> Paint
            |> fill
         , (if oklch.lightness >= 0.5 then
                Color.black

            else
                Color.white
           )
            |> Paint
            |> stroke
         , strokeWidth 0.25
         ]
            ++ attrs
        )
        []
    , text_
        [ fontSize 4
        , x circleX
        , y circleY
        , dominantBaseline DominantBaselineMiddle
        , textAnchor AnchorMiddle
        , (if oklch.lightness > 0.5 then
            Color.black

           else
            Color.white
          )
            |> Paint
            |> fill
        ]
        [ Round.round 2 oklch.lightness
            |> text
        ]
    ]


toColor : String -> Maybe Color
toColor colorString =
    case String.toList colorString of
        [ r, g, b ] ->
            Maybe.map3 Color.rgb255
                (toHex r r)
                (toHex g g)
                (toHex b b)

        [ r1, r2, g1, g2, b1, b2 ] ->
            Maybe.map3 Color.rgb255
                (toHex r1 r2)
                (toHex g1 g2)
                (toHex b1 b2)

        _ ->
            Nothing


toHex : Char -> Char -> Maybe Int
toHex h l =
    Maybe.map2 (\hx lx -> hx * 16 + lx)
        (toHexDigit h)
        (toHexDigit l)


fromHexDigit : Int -> Maybe Char
fromHexDigit c =
    case c of
        0x00 ->
            Just '0'

        0x01 ->
            Just '1'

        0x02 ->
            Just '2'

        0x03 ->
            Just '3'

        0x04 ->
            Just '4'

        0x05 ->
            Just '5'

        0x06 ->
            Just '6'

        0x07 ->
            Just '7'

        0x08 ->
            Just '8'

        0x09 ->
            Just '9'

        0x0A ->
            Just 'a'

        0x0B ->
            Just 'b'

        0x0C ->
            Just 'c'

        0x0D ->
            Just 'd'

        0x0E ->
            Just 'e'

        0x0F ->
            Just 'f'

        _ ->
            Nothing


toHexDigit : Char -> Maybe Int
toHexDigit c =
    case Char.toLower c of
        '0' ->
            Just 0x00

        '1' ->
            Just 0x01

        '2' ->
            Just 0x02

        '3' ->
            Just 0x03

        '4' ->
            Just 0x04

        '5' ->
            Just 0x05

        '6' ->
            Just 0x06

        '7' ->
            Just 0x07

        '8' ->
            Just 0x08

        '9' ->
            Just 0x09

        'a' ->
            Just 0x0A

        'b' ->
            Just 0x0B

        'c' ->
            Just 0x0C

        'd' ->
            Just 0x0D

        'e' ->
            Just 0x0E

        'f' ->
            Just 0x0F

        _ ->
            Nothing
