module Theme exposing (affinityToColor, backgroundColor, bebasNeue, blocks, borderColor, borderGlow, captureIt, card, cardRoundness, celticHand, choice, classToBadge, classToColor, collapsibleBlocks, colors, column, complicationCategoryToColor, complicationCategoryToGradient, gradientText, gradientTextHtml, image, intToBackground, intToColor, maybeButton, morpheus, padding, rounded, row, rythm, style, topBackground, viewAffinity, wrappedRow)

import Color
import Element exposing (Attribute, Element, centerY, el, fill, height, px, rgb, rgb255, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Generated.Types as Types exposing (Affinity(..), Class(..), ComplicationCategory(..), Slot(..))
import Gradients
import Hex
import Html exposing (Html)
import Html.Attributes
import Images exposing (Image)
import MarkMini exposing (Block(..), Color(..), Piece(..))
import Parser exposing ((|.))
import String.Extra
import String.Multiline
import Types exposing (Display(..))


rythm : number
rythm =
    8


padding : Attribute msg
padding =
    Element.padding rythm


spacing : Attribute msg
spacing =
    Element.spacing rythm


rounded : Attribute msg
rounded =
    Border.rounded rythm


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
    Element.html <| gradientTextHtml outlineSize gradient value


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
blocks =
    genericBlocks Nothing DisplayFull


collapsibleBlocks : (Display -> msg) -> Display -> List (Attribute msg) -> String -> Element msg
collapsibleBlocks toMsg =
    genericBlocks (Just toMsg)


genericBlocks : Maybe (Display -> msg) -> Display -> List (Attribute msg) -> String -> Element msg
genericBlocks toMsg display attrs input =
    input
        |> String.Multiline.here
        |> String.split "\n\n"
        |> List.map (block toMsg display)
        |> column (spacing :: width fill :: attrs)


block : Maybe (Display -> msg) -> Display -> String -> Element msg
block toMsg display input =
    case Parser.run (MarkMini.blockParser |. Parser.end) (String.trim input) of
        Err _ ->
            Element.paragraph
                [ Font.color <| rgb 1 0 0 ]
                [ text input ]

        Ok (SectionTitle value) ->
            viewSectionTitle toMsg display value

        Ok (UnorderedList lines) ->
            lines
                |> List.map
                    (\line ->
                        line
                            |> List.map viewPiece
                            |> Html.li []
                    )
                |> Html.ul [ Html.Attributes.class "markdown" ]
                |> Element.html
                |> List.singleton
                |> Element.paragraph []

        Ok (Paragraph { pieces, center, mono }) ->
            pieces
                |> List.map viewPiece
                |> Html.span [ Html.Attributes.class "markdown" ]
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
    , companionBlack : ( Int, Int )
    , companionBlue : ( Int, Int )
    , companionGold : ( Int, Int )
    , companionOrange : ( Int, Int )
    , companionRed : ( Int, Int )
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
    , companionBlack = ( 0x004B4A4A, 0x008A8A8A )
    , companionRed = ( 0x00CD4A48, 0x00D98A88 )
    , companionOrange = ( 0x00FF7E4A, 0x00FFA189 )
    , companionBlue = ( 0x0049AEFF, 0x0088C2FC )
    , companionGold = ( 0x00A18729, 0x00F5E96F )
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
                colored : Int -> Html msg
                colored colorInt =
                    Html.span
                        [ Html.Attributes.style "color" <| toCss colorInt ]
                        (List.map viewPiece children)
            in
            case color of
                ChoiceColor ->
                    colored colors.choice

                ClassColor class ->
                    colored <| classToColor class

                SlotColor slot ->
                    colored <| slotToColor slot

        Smol children ->
            Html.span
                [ Html.Attributes.style "font-size" "0.8em" ]
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
            Html.text (String.replace "..." "…" value)

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
            Html.a
                [ Html.Attributes.class "link"
                , Html.Attributes.href target
                , Html.Attributes.target "_blank"
                ]
                [ Html.text cut ]

        Slot slot ->
            Html.img [ Html.Attributes.src (Types.slotToImage slot).src ] []

        Warning ->
            Html.span
                [ Html.Attributes.style "font-size" "1.2em"
                ]
                [ Html.text "⚠️" ]

        Error ->
            Html.span
                [ Html.Attributes.style "font-size" "1.2em"
                ]
                [ Html.text "⛔" ]

        Affinity affinity ->
            Html.img [ Html.Attributes.src (Types.affinityToImage affinity).src ] []

        Power value ->
            Html.span
                [ Html.Attributes.style "font-family" "\"Capture It\""
                , Html.Attributes.style "font-size" "20px"
                ]
                [ gradientTextHtml 4 Gradients.yellowGradient value ]

        RewardPoints value ->
            Html.span
                [ Html.Attributes.style "font-family" "\"Capture It\""
                , Html.Attributes.style "font-size" "20px"
                ]
                [ gradientTextHtml 4 Gradients.blueGradient value ]

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


viewSectionTitle : Maybe (Display -> msg) -> Display -> String -> Element msg
viewSectionTitle toMsg display label =
    let
        gradient : String -> Element msg
        gradient =
            gradientText 4 Gradients.blueGradient
    in
    row
        [ celticHand
        , Font.size 36
        , width fill
        , id label
        ]
    <|
        case toMsg of
            Just tag ->
                let
                    button : Element msg
                    button =
                        Input.button
                            [ Border.rounded 4
                            , Element.padding 4
                            , Border.width 1
                            , borderColor colors.choice
                            ]
                            { onPress = Just <| tag <| Types.nextDisplay display
                            , label =
                                case display of
                                    DisplayFull ->
                                        gradient "▶"

                                    DisplayCompact ->
                                        gradient "▲"

                                    DisplayCollapsed ->
                                        gradient "▼"
                            }
                in
                [ hr
                , gradient label
                , text " "
                , button
                , hr
                ]

            Nothing ->
                [ hr
                , gradient label
                , hr
                ]


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
        ((color // 256)
            |> modBy 256
        )
        (modBy 256 color)


intToBackground : Int -> Element.Color
intToBackground color =
    let
        hsla : { hue : Float, saturation : Float, lightness : Float, alpha : Float }
        hsla =
            Color.rgb255
                (color // 65536)
                ((color // 256)
                    |> modBy 256
                )
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
        { display : Display
        , onPress : Maybe msg
        , glow : Int
        , isSelected : Bool
        , imageHeight : Int
        , imageAttrs : List (Attribute msg)
        , image : Image
        , inFront : List (Element msg)
        , content : List (Element msg)
        }
    -> Element msg
card attrs config =
    if config.display == DisplayCollapsed || config.display == DisplayCompact && not config.isSelected then
        Element.none

    else
        let
            cardAttributes : List (Attribute msg)
            cardAttributes =
                [ height fill
                , Font.color <| rgb 0 0 0
                , if config.display == DisplayCompact then
                    Border.roundEach
                        { topLeft = cardRoundness
                        , topRight = 8
                        , bottomLeft = cardRoundness
                        , bottomRight = 8
                        }

                  else
                    Border.roundEach
                        { topLeft = cardRoundness
                        , topRight = cardRoundness
                        , bottomLeft = 8
                        , bottomRight = 8
                        }
                , if config.isSelected then
                    Background.color <| intToBackground config.glow

                  else
                    Background.color <| rgb 1 1 1
                , if config.isSelected then
                    borderGlow config.glow

                  else
                    Border.width 0
                , if config.display == DisplayFull then
                    width <| Element.minimum 320 <| Element.maximum 400 fill

                  else
                    width fill
                ]

            content : List (Element msg)
            content =
                el
                    (Border.rounded cardRoundness
                        :: (if config.display == DisplayFull then
                                height <| px config.imageHeight

                            else
                                width <| px config.imageHeight
                           )
                        :: (if config.display == DisplayFull then
                                width fill

                            else
                                height <| Element.minimum (config.imageHeight * 2 // 3) fill
                           )
                        :: Background.image config.image.src
                        :: List.map Element.inFront config.inFront
                        ++ config.imageAttrs
                    )
                    Element.none
                    :: config.content
        in
        maybeButton (cardAttributes ++ attrs)
            { label =
                if config.display == DisplayFull then
                    Element.column [ height fill, width fill ] content

                else
                    Element.row [ height fill, width fill ] content
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


borderGlow : Int -> Attribute msg
borderGlow color =
    Border.glow (intToColor color) 8


affinityToColor : Affinity -> Int
affinityToColor affinity =
    case affinity of
        All ->
            0x00FFFFFF

        Beast ->
            0x008C6D33

        Blood ->
            0x00C21D1D

        Body ->
            0x00F5C49E

        Earth ->
            0x00A35A49

        Fire ->
            0x00FF5500

        Life ->
            0x00FF78DD

        Metal ->
            0x00A3A3A3

        Mind ->
            0x00B322B3

        Nature ->
            0x003CA62F

        Necro ->
            0x00C0C0C0

        Soul ->
            0x00FFFFFF

        Water ->
            0x001E93D6

        Wind ->
            0x00CABEDD
