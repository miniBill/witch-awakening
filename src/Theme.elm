module Theme exposing (backgroundColor, bebasNeue, blocks, borderColor, captureIt, card, celticHand, choice, classToBadge, classToColor, colors, column, complicationCategoryToColor, complicationCategoryToGradient, gradientText, gradientTextHtml, id, image, intToColor, maybeButton, morpheus, padding, row, rythm, style, topBackground, viewAffinity, wrappedRow)

import Color
import Element exposing (Attribute, Element, centerY, el, fill, height, px, rgb, rgb255, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Generated.Types as Types exposing (Affinity, Class(..), ComplicationCategory(..), Slot(..))
import Gradients
import Hex
import Html exposing (Html)
import Html.Attributes
import Images exposing (Image)
import MarkMini exposing (Block(..), Color(..), Piece(..))
import Parser exposing ((|.))
import String.Extra
import String.Multiline


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
        |> String.Multiline.here
        |> String.split "\n\n"
        |> List.map block
        |> column (spacing :: width fill :: attrs)


block : String -> Element msg
block input =
    case Parser.run (MarkMini.blockParser |. Parser.end) (String.trim input) of
        Err _ ->
            Element.paragraph
                [ Font.color <| rgb 1 0 0 ]
                [ text input ]

        Ok (SectionTitle value) ->
            viewSectionTitle value

        Ok (UnorderedList lines) ->
            lines
                |> List.map
                    (\line ->
                        line
                            |> List.map viewPiece
                            |> Html.li []
                    )
                |> Html.ul []
                |> Element.html
                |> List.singleton
                |> Element.paragraph []

        Ok (Paragraph { pieces, center, mono }) ->
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
                    , if mono then
                        Font.family [ Font.monospace ]

                      else
                        Font.family []
                    ]


colors :
    { academic : Int
    , choice : Int
    , epic : Int
    , folk : Int
    , gameMode : Int
    , heroic : Int
    , noble : Int
    , sorceress : Int
    , speech : Int
    , warlock : Int
    , white : Int
    , worldShift : Int
    }
colors =
    { academic = 0x001A77FF
    , choice = 0x0004D4ED
    , epic = 0x00C32DE6
    , folk = 0x004DE1FF
    , gameMode = 0x00AA08B9
    , heroic = 0x00F2D706
    , noble = 0x0014E602
    , sorceress = 0x00FF0000
    , speech = 0x00F88000
    , warlock = 0x0019AD00
    , white = 0x00FFFFFF
    , worldShift = 0x006ED32A
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

                        SlotColor slot ->
                            slotToColor slot
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

        Link target ->
            let
                cut : String
                cut =
                    if String.startsWith "https://" target then
                        String.dropLeft 8 target

                    else if String.startsWith "http://" target then
                        String.dropLeft 7 target

                    else if String.startsWith "mailto:" target then
                        String.dropLeft 7 target

                    else
                        target
            in
            Html.a [ Html.Attributes.href target ] [ Html.text cut ]

        Slot slot ->
            Html.img [ Html.Attributes.src (Types.slotToImage slot).src ] []

        Affinity affinity ->
            Html.img [ Html.Attributes.src (Types.affinityToImage affinity).src ] []

        Number value ->
            Html.span
                [ Html.Attributes.style "font-family" "\"Capture It\""
                , Html.Attributes.style "font-size" "20px"
                ]
                [ gradientTextHtml 4 Gradients.yellowGradient value ]

        Kisses value ->
            Html.span []
                [ Html.b [] [ Html.i [] [ Html.text "₭\u{200A}" ] ]
                , Html.text value
                ]


slotToColor : Slot -> Int
slotToColor slot =
    case slot of
        Folk ->
            colors.folk

        Noble ->
            colors.noble

        Heroic ->
            colors.heroic

        Epic ->
            colors.epic

        White ->
            colors.white


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
        , id label
        ]
        [ hr, gradientText 4 Gradients.blueGradient label, hr ]


id : String -> Attribute msg
id label =
    Element.htmlAttribute <| Html.Attributes.id (String.Extra.underscored label)


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


backgroundColor : Int -> Attribute msg
backgroundColor color =
    Background.color <| intToColor color


intToColor : Int -> Element.Color
intToColor color =
    rgb255
        (color // 65536)
        (modBy 256 (color // 256))
        (modBy 256 color)


intToBackground : Int -> Element.Color
intToBackground color =
    let
        hsla : { hue : Float, saturation : Float, lightness : Float, alpha : Float }
        hsla =
            Color.rgb255
                (color // 65536)
                (modBy 256 (color // 256))
                (modBy 256 color)
                |> Color.toHsla

        rgba : { red : Float, green : Float, blue : Float, alpha : Float }
        rgba =
            { hsla | lightness = 0.9 }
                |> Color.fromHsla
                |> Color.toRgba
    in
    Element.rgba rgba.red rgba.green rgba.blue rgba.alpha


style : String -> String -> Attribute msg
style key value =
    Element.htmlAttribute <| Html.Attributes.style key value


viewAffinity : Affinity -> Element msg
viewAffinity affinity =
    image [] (Types.affinityToImage affinity)


cardRoundness : Int
cardRoundness =
    72


card :
    List (Attribute msg)
    ->
        { onPress : Maybe msg
        , glow : Maybe Int
        , imageHeight : Int
        , imageAttrs : List (Attribute msg)
        , image : Image
        , inFront : List (Element msg)
        , content : List (Element msg)
        }
    -> Element msg
card attrs config =
    let
        cardAttributes : List (Attribute msg)
        cardAttributes =
            [ height fill
            , width <| Element.minimum 320 <| Element.maximum 400 fill
            , Font.color <| rgb 0 0 0
            , Border.roundEach
                { topLeft = cardRoundness
                , topRight = cardRoundness
                , bottomLeft = 8
                , bottomRight = 8
                }
            , case config.glow of
                Just color ->
                    Background.color <| intToBackground color

                Nothing ->
                    Background.color <| rgb 1 1 1
            , case config.glow of
                Just color ->
                    Border.glow (intToColor color) 8

                Nothing ->
                    Border.width 0
            ]

        content : Element msg
        content =
            Element.column [ height fill, width fill ]
                (el
                    (width fill
                        :: Border.rounded cardRoundness
                        :: height (px config.imageHeight)
                        :: Background.image config.image.src
                        :: List.map Element.inFront config.inFront
                        ++ config.imageAttrs
                    )
                    Element.none
                    :: config.content
                )
    in
    maybeButton (cardAttributes ++ attrs)
        { label = content
        , onPress = config.onPress
        }


maybeButton :
    List (Attribute msg)
    -> { label : Element msg, onPress : Maybe msg }
    -> Element msg
maybeButton attrs config =
    case config.onPress of
        Just _ ->
            Input.button attrs config

        Nothing ->
            el attrs config.label


complicationCategoryToColor : ComplicationCategory -> Int
complicationCategoryToColor category =
    case category of
        WorldShift ->
            colors.worldShift


complicationCategoryToGradient : ComplicationCategory -> List ( Int, Int, Int )
complicationCategoryToGradient category =
    case category of
        WorldShift ->
            Gradients.greenGradient


classToBadge : Class -> Image
classToBadge class =
    case class of
        Academic ->
            Images.badgeAcademic

        Sorceress ->
            Images.badgeSorceress

        Warlock ->
            Images.badgeWarlock


topBackground : Images.Image -> List (Element.Attribute msg)
topBackground { src } =
    [ style "background-image" <| "url(\"" ++ src ++ "\")"
    , style "background-repeat" "no-repeat"
    , style "background-position" "top"
    , style "background-size" "100%"
    ]
