module Theme exposing (bebasNeue, celticHand, colors, column, cyan, gradientText, image, morpheus, padding, paragraphs, row, wrappedRow)

import Element exposing (Attribute, Element, el, fill, rgb, rgb255, text, width)
import Element.Font as Font
import Hex
import Html exposing (Html)
import Html.Attributes
import Parser exposing ((|.), (|=), Parser)
import Set exposing (Set)


rythm : number
rythm =
    8


padding : Attribute msg
padding =
    Element.padding rythm


spacing : Attribute msg
spacing =
    Element.spacing rythm


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


celticHand : Attribute msg
celticHand =
    Font.family [ Font.typeface "Celtic Hand" ]


bebasNeue : Attribute msg
bebasNeue =
    Font.family [ Font.typeface "Bebas Neue" ]


morpheus : Attribute msg
morpheus =
    Font.family [ Font.typeface "Morpheus" ]


paragraphs : List (Attribute msg) -> String -> Element msg
paragraphs attrs input =
    input
        |> String.split "\n\n"
        |> List.map paragraph
        |> column (spacing :: width fill :: attrs)


paragraph : String -> Element msg
paragraph input =
    case Parser.run paragraphParser input of
        Err _ ->
            Element.paragraph
                [ Font.color <| rgb 1 0 0 ]
                [ text input ]

        Ok ( pieces, { center } ) ->
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


type Piece
    = Speech (List Piece)
    | Cyan (List Piece)
    | Italic (List Piece)
    | Bold (List Piece)
    | Text String


paragraphParser : Parser ( List Piece, { center : Bool } )
paragraphParser =
    Parser.succeed (\center pieces -> ( pieces, { center = center } ))
        |= Parser.oneOf
            [ Parser.succeed True
                |. Parser.symbol "[center]"
            , Parser.succeed False
            ]
        |= mainParser


mainParser : Parser (List Piece)
mainParser =
    Parser.oneOf
        [ Parser.succeed Speech
            |. Parser.symbol "\""
            |= innerParser '"'
            |. Parser.symbol "\""
        , Parser.succeed Italic
            |. Parser.symbol "_"
            |= innerParser '_'
            |. Parser.symbol "_"
        , Parser.succeed Bold
            |. Parser.symbol "*"
            |= innerParser '*'
            |. Parser.symbol "*"
        , Parser.succeed Cyan
            |. Parser.symbol "{cyan "
            |= innerParser '}'
            |. Parser.symbol "}"
        , Parser.succeed Text
            |= Parser.getChompedString
                (Parser.succeed ()
                    |. Parser.chompIf (\c -> not (Set.member c special))
                    |. Parser.chompWhile (\c -> not (Set.member c special))
                )
        ]
        |> many


innerParser : Char -> Parser (List Piece)
innerParser until =
    Parser.chompWhile (\c -> c /= until)
        |> Parser.getChompedString
        |> Parser.andThen
            (\chomped ->
                case Parser.run mainParser chomped of
                    Ok res ->
                        Parser.succeed res

                    Err deadEnds ->
                        Parser.problem <| "Error in inner parser: " ++ Parser.deadEndsToString deadEnds
            )


special : Set Char
special =
    [ '_', '"', '*', '{' ]
        |> Set.fromList


many : Parser a -> Parser (List a)
many parser =
    Parser.loop []
        (\acc ->
            Parser.oneOf
                [ Parser.succeed
                    (\e ->
                        Parser.Loop (e :: acc)
                    )
                    |= parser
                , Parser.succeed
                    (\_ ->
                        Parser.Done <| List.reverse acc
                    )
                    |= Parser.end
                ]
        )


colors :
    { academic : Int
    , sorceress : Int
    , warlock : Int
    , speech : Int
    , cyan : Int
    }
colors =
    { academic = 0xD7FF
    , sorceress = 0x00FF0022
    , warlock = 0x0027B01C
    , speech = 0x00F88000
    , cyan = 0x0004D4ED
    }


toCss : Int -> String
toCss rgb =
    "#" ++ String.padLeft 6 '0' (Hex.toString rgb)


viewPiece : Piece -> Html msg
viewPiece piece =
    case piece of
        Speech children ->
            Html.span
                [ Html.Attributes.style "color" <| toCss colors.speech ]
                (Html.text "“" :: List.map viewPiece children ++ [ Html.text "”" ])

        Cyan children ->
            Html.span
                [ Html.Attributes.style "color" <| toCss colors.cyan ]
                (List.map viewPiece children)

        Italic children ->
            Html.i []
                (List.map viewPiece children)

        Bold children ->
            Html.b []
                (List.map viewPiece children)

        Text value ->
            Html.text value


column : List (Attribute msg) -> List (Element msg) -> Element msg
column attrs children =
    Element.column (spacing :: attrs) children


row : List (Attribute msg) -> List (Element msg) -> Element msg
row attrs children =
    Element.row (spacing :: attrs) children


wrappedRow : List (Attribute msg) -> List (Element msg) -> Element msg
wrappedRow attrs children =
    Element.wrappedRow (spacing :: attrs) children
