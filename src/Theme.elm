module Theme exposing (bebasNeue, celticHand, cyan, gradientText, image, morpheus, orangeSpeech, paragraph, quote, speech)

import Element exposing (Attribute, Element, el, rgb, rgb255, text)
import Element.Font as Font
import Html exposing (Html)
import Html.Attributes
import Parser exposing ((|.), (|=), Parser)


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


celticHand : Attribute msg
celticHand =
    Font.family [ Font.typeface "Celtic Hand" ]


bebasNeue : Attribute msg
bebasNeue =
    Font.family [ Font.typeface "Bebas Neue" ]


morpheus : Attribute msg
morpheus =
    Font.family [ Font.typeface "Morpheus" ]


paragraph : String -> Element msg
paragraph input =
    case
        Parser.run
            (Parser.succeed identity
                |= paragraphParser
                |. Parser.end
            )
            input
    of
        Err _ ->
            Element.paragraph
                [ Font.color <| rgb 1 0 0 ]
                [ text input ]

        Ok pieces ->
            pieces
                |> List.map viewPiece
                |> Html.span []
                |> Element.html
                |> List.singleton
                |> Element.paragraph []


type Piece
    = Speech Piece
    | Cyan Piece
    | Italic Piece
    | Text String


paragraphParser : Parser (List Piece)
paragraphParser =
    Parser.sequence
        { start = ""
        , item =
            Parser.oneOf
                [ Parser.succeed Speech
                    |. Parser.symbol "\""
                    |= insideParser
                    |. Parser.symbol "\""
                , insideParser
                ]
        , end = ""
        , separator = ""
        , trailing = Parser.Optional
        , spaces = Parser.succeed ()
        }


insideParser : Parser Piece
insideParser =
    Parser.succeed
        (\chomped ->
            let
                _ =
                    Debug.log "chomped" chomped
            in
            Text chomped
        )
        |= Parser.getChompedString
            (Parser.chompIf (\c -> c /= '"')
                |. Parser.chompWhile (\c -> c /= '"')
            )


viewPiece : Piece -> Html msg
viewPiece piece =
    case piece of
        Speech children ->
            Html.span
                [ Html.Attributes.style "color" "#F88000" ]
                [ Html.text "“", viewPiece children, Html.text "”" ]

        Cyan children ->
            Html.span
                [ Html.Attributes.style "color" "#04D4ED" ]
                [ viewPiece children ]

        Italic children ->
            Html.i
                []
                [ viewPiece children ]

        Text value ->
            Html.text value
