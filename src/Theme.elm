module Theme exposing (cyan, gradientText, image, orangeSpeech, quote, speech)

import Element exposing (Attribute, Element, el, rgb255, text)
import Element.Font as Font
import Html
import Html.Attributes


image : List (Attribute msg) -> { a | src : String } -> Element msg
image attrs { src } =
    Element.image attrs
        { src = src
        , description = ""
        }


cyan : String -> Element msg
cyan value =
    el [ Font.color <| rgb255 0x04 0xD4 0xED ] <| text value


gradientText : Float -> List ( Int, Int, Int ) -> String -> Element msg
gradientText outlineSize gradient value =
    Element.html <|
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


speech : String -> Element msg
speech value =
    el [ orangeSpeech ] <|
        (text <| quote value)


orangeSpeech : Attribute msg
orangeSpeech =
    Font.color <| rgb255 0xF8 0x80 0x00


quote : String -> String
quote value =
    "“" ++ value ++ "”"
