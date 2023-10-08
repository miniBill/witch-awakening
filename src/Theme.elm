module Theme exposing (bebasNeue, blocks, borderColor, captureIt, choice, classToColor, column, gradientText, image, intToColor, morpheus, padding, row, rythm, viewAffinity, wrappedRow)

import Element exposing (Attribute, Element, centerY, el, fill, height, px, rgb, rgb255, text, width)
import Element.Border as Border
import Element.Font as Font
import Gradients
import Hex
import Html exposing (Html)
import Html.Attributes
import Images exposing (Image)
import MarkMini exposing (Block(..), Color(..), Piece(..))
import Parser exposing ((|.))
import Types exposing (Affinity(..), Class(..))


rythm : number
rythm =
    8


padding : Attribute msg
padding =
    Element.padding rythm


spacing : Attribute msg
spacing =
    Element.spacing rythm


image : List (Attribute msg) -> Image -> Element msg
image attrs { src } =
    Element.image attrs
        { src = src
        , description = ""
        }


choice : String -> Element msg
choice value =
    el [ Font.color <| rgb255 0x04 0xD4 0xED ] <| text value


gradientText : Float -> List ( Int, Int, Int ) -> String -> Element msg
gradientText outlineSize gradient value =
    Element.html <|
        gradientTextHtml outlineSize gradient value


gradientTextHtml : Float -> List ( Int, Int, Int ) -> String -> Html msg
gradientTextHtml outlineSize gradient value =
    Html.span
        [ Html.Attributes.class "outlined"
        , Html.Attributes.attribute "data-text" value
        , gradient
            |> List.map rgbToString
            |> String.join ", "
            |> (\joined -> "--text-stroke: " ++ String.fromFloat outlineSize ++ "px #000; --background: linear-gradient(to bottom, " ++ joined ++ ")")
            |> Html.Attributes.attribute "style"
        ]
        [ Html.text value ]


rgbToString : ( Int, Int, Int ) -> String
rgbToString ( r, g, b ) =
    "rgb("
        ++ String.fromInt r
        ++ " "
        ++ String.fromInt g
        ++ " "
        ++ String.fromInt b
        ++ ")"


celticHand : Attribute msg
celticHand =
    Font.family [ Font.typeface "Celtic Hand" ]


bebasNeue : Attribute msg
bebasNeue =
    Font.family [ Font.typeface "Bebas Neue" ]


morpheus : Attribute msg
morpheus =
    Font.family [ Font.typeface "Morpheus" ]


captureIt : Attribute msg
captureIt =
    Font.family [ Font.typeface "Capture It" ]


blocks : List (Attribute msg) -> String -> Element msg
blocks attrs input =
    input
        |> String.split "\n\n"
        |> List.map block
        |> column (spacing :: width fill :: attrs)


block : String -> Element msg
block input =
    case Parser.run (MarkMini.blockParser |. Parser.end) input of
        Err _ ->
            Element.paragraph
                [ Font.color <| rgb 1 0 0 ]
                [ text input ]

        Ok (SectionTitle value) ->
            viewSectionTitle value

        Ok (Paragraph { pieces, center }) ->
            pieces
                |> List.map viewPiece
                |> Html.span []
                |> Element.html
                |> List.singleton
                |> Element.paragraph
                    [ if center then
                        Font.center

                      else
                        Font.alignLeft
                    ]


colors :
    { academic : Int
    , sorceress : Int
    , warlock : Int
    , speech : Int
    , choice : Int
    }
colors =
    { academic = 0x001A77FF
    , sorceress = 0x00FF0000
    , warlock = 0x0019AD00
    , speech = 0x00F88000
    , choice = 0x0004D4ED
    }


toCss : Int -> String
toCss rgb =
    "#" ++ String.padLeft 6 '0' (Hex.toString rgb)


classToColor : Class -> Int
classToColor class =
    case class of
        Academic ->
            colors.academic

        Sorceress ->
            colors.sorceress

        Warlock ->
            colors.warlock


viewPiece : Piece -> Html msg
viewPiece piece =
    case piece of
        Speech children ->
            Html.span
                [ Html.Attributes.style "color" <| toCss colors.speech ]
                (Html.text "“" :: List.map viewPiece children ++ [ Html.text "”" ])

        Colored color children ->
            let
                colorInt : Int
                colorInt =
                    case color of
                        ChoiceColor ->
                            colors.choice

                        ClassColor class ->
                            classToColor class
            in
            Html.span
                [ Html.Attributes.style "color" <| toCss colorInt ]
                (List.map viewPiece children)

        Italic children ->
            Html.i []
                (List.map viewPiece children)

        Underlined children ->
            Html.u []
                (List.map viewPiece children)

        Bold children ->
            Html.b []
                (List.map viewPiece children)

        Text value ->
            Html.text value

        Number value ->
            Html.span
                [ Html.Attributes.style "font-family" "\"Capture It\""
                , Html.Attributes.style "font-size" "20px"
                ]
                [ gradientTextHtml 4 Gradients.yellowGradient (String.fromInt value) ]


column : List (Attribute msg) -> List (Element msg) -> Element msg
column attrs children =
    Element.column (spacing :: attrs) children


row : List (Attribute msg) -> List (Element msg) -> Element msg
row attrs children =
    Element.row (spacing :: attrs) children


wrappedRow : List (Attribute msg) -> List (Element msg) -> Element msg
wrappedRow attrs children =
    Element.wrappedRow (spacing :: attrs) children


viewSectionTitle : String -> Element msg
viewSectionTitle label =
    row
        [ celticHand
        , Font.size 36
        , width fill
        ]
        [ hr, gradientText 4 Gradients.blueGradient label, hr ]


hr : Element msg
hr =
    el
        [ width fill
        , height <| px 1
        , centerY
        , Border.color <| rgb255 0x00 0xE4 0xFF
        , Border.widthEach
            { top = 1
            , bottom = 0
            , left = 0
            , right = 0
            }
        , style "box-shadow" "0px 0px 1px 1px #00E4FF"
        ]
        Element.none


borderColor : Int -> Attribute msg
borderColor color =
    Border.color <| intToColor color


intToColor : Int -> Element.Color
intToColor color =
    rgb255
        (color // 65536)
        (modBy 256 (color // 256))
        (modBy 256 color)


style : String -> String -> Attribute msg
style key value =
    Element.htmlAttribute <| Html.Attributes.style key value


viewAffinity : Affinity -> Element msg
viewAffinity affinity =
    let
        badge : Image
        badge =
            case affinity of
                All ->
                    Images.affinityAll

                Beast ->
                    Images.affinityBeast

                Blood ->
                    Images.affinityBlood

                Body ->
                    Images.affinityBody

                Fire ->
                    Images.affinityFire

                Life ->
                    Images.affinityLife

                Metal ->
                    Images.affinityMetal

                Mind ->
                    Images.affinityMind

                Nature ->
                    Images.affinityNature

                Necro ->
                    Images.affinityNecro

                Soul ->
                    Images.affinitySoul

                Water ->
                    Images.affinityWater

                Wind ->
                    Images.affinityWind
    in
    image [] badge
