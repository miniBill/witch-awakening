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
import TypedSvg exposing (circle, defs, linearGradient, stop, svg, text_)
import TypedSvg.Attributes exposing (dominantBaseline, fill, id, offset, stopColor, stroke, textAnchor, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, fontSize, r, strokeWidth, x, y)
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


width : number
width =
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
                , Html.Attributes.style "width" (String.fromFloat width ++ "px")
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
    ( x / width * 2 - 1
    , y / width * 2 - 1
    )


viewPointer : Maybe ( Float, Float ) -> List (Svg Msg)
viewPointer pointer =
    case pointer of
        Nothing ->
            []

        Just ( x, y ) ->
            let
                h : Float
                h =
                    atan2 y x / (pi * 2)

                c : Float
                c =
                    sqrt (x ^ 2 + y ^ 2) / 3

                oklch : Oklch
                oklch =
                    Oklch.oklch 0.5 c h
            in
            defs []
                [ List.range 0 100
                    |> List.map
                        (\i ->
                            let
                                percent : Float
                                percent =
                                    toFloat i
                            in
                            stop
                                [ offset (String.fromFloat percent ++ "%")
                                , Oklch.oklch (percent / 100) c h
                                    |> Oklch.toCssString
                                    |> stopColor
                                ]
                                []
                        )
                    |> linearGradient [ id "gradient" ]
                ]
                :: colorToCircle
                    [ fill (Reference "gradient")
                    , r 8
                    ]
                    oklch


colorStringToCircle : String -> List (Svg msg)
colorStringToCircle colorString =
    case toColor colorString of
        Nothing ->
            [ text ("Invalid color: " ++ colorString) ]

        Just c ->
            colorToCircle [ fill (Paint c) ] (Oklch.fromColor c)


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
