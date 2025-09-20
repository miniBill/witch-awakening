module Theme exposing (backgroundColor, bebasNeue, blocks, borderColor, borderGlow, button, captureIt, card, cardRoundness, celticHand, choice, classToBadge, collapsibleBlocks, colors, column, complicationCategoryToColor, complicationCategoryToGradient, doubleColumn, gradientText, gradientTextHtml, id, image, intToBackground, intToColor, maybeButton, morpheus, padding, rhythm, rounded, row, slider, spacing, style, topBackground, triangleDown, triangleRight, viewAffinity, viewClasses, wrappedRow)

import Color
import Element exposing (Attribute, Element, Length, centerY, el, fill, height, px, rgb, rgb255, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Generated.Affinity
import Generated.Classes
import Generated.Types as Types exposing (Affinity(..), Class(..), ComplicationCategory(..), Slot(..))
import Gradients
import Hex
import Html exposing (Html)
import Html.Attributes
import Images exposing (Image)
import List.Extra
import MarkMini exposing (Block(..), Color(..), Piece(..))
import Parser exposing ((|.))
import String.Extra
import String.Multiline
import Types exposing (Display(..))


rhythm : number
rhythm =
    8


padding : Attribute msg
padding =
    Element.padding rhythm


spacing : Attribute msg
spacing =
    Element.spacing rhythm


rounded : Attribute msg
rounded =
    Border.rounded rhythm


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
    { choice : Int
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
    , speech : Int
    , white : Int
    , worldShift : Int
    }
colors =
    { choice = 0x0004D4ED
    , companionBlack = ( 0x004B4A4A, 0x008A8A8A )
    , companionRed = ( 0x00CD4A48, 0x00D98A88 )
    , companionOrange = ( 0x00FF7E4A, 0x00FFA189 )
    , companionBlue = ( 0x0049AEFF, 0x0088C2FC )
    , companionGold = ( 0x00F2E76D, 0x00F5E96F )
    , epic = 0x00C32DE6
    , folk = 0x004DE1FF
    , gameMode = 0x00AA08B9
    , heroic = 0x00F2D706
    , noble = 0x0014E602
    , speech = 0x00F88000
    , white = 0x00FFFFFF
    , worldShift = 0x006ED32A
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
                    colored <| Generated.Classes.classToColor class

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

        Checkmark ->
            Html.span
                [ Html.Attributes.style "font-size" "1.2em"
                ]
                [ Html.text "✅" ]

        JackOfAll ->
            Html.span
                [ Html.Attributes.style "font-size" "1.2em"
                ]
                [ Html.text "🃏" ]

        Affinity affinity ->
            viewAffinityBadge affinity

        Class class ->
            Html.img [ Html.Attributes.src (classToBadge class).src ] []

        Star ->
            Html.span
                [ Html.Attributes.style "font-family" "\"Capture It\""
                , Html.Attributes.style "font-size" "20px"
                ]
                [ gradientTextHtml 4 Gradients.yellowGradient "★" ]

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

        LineBreak ->
            Html.br [] []


viewAffinityBadge : Affinity -> Html msg
viewAffinityBadge affinity =
    let
        linearGradient : Int -> List ( String, Int ) -> String
        linearGradient angle stops =
            "linear-gradient("
                ++ String.fromInt angle
                ++ "deg, "
                ++ String.join ","
                    (List.map
                        (\( stopColor, stopPercent ) ->
                            stopColor
                                ++ " "
                                ++ String.fromInt stopPercent
                                ++ "%"
                        )
                        stops
                    )
                ++ ")"

        opaque : Int -> Int
        opaque f =
            f * 256 + 0xFF

        colorToCss : Int -> String
        colorToCss color =
            "#" ++ String.padLeft 8 '0' (Hex.toString color)

        common : List (Html.Attribute msg)
        common =
            [ Html.Attributes.style "font-family" "Unreal Tournament"
            , Html.Attributes.style "font-size" "0.9rem"
            , Html.Attributes.style "font-weight" "bold"
            , Html.Attributes.style "margin" "1px"
            , Html.Attributes.style "border-radius" "999px"
            , Html.Attributes.style "box-sizing" "border-box"
            , Html.Attributes.style "display" "inline-block"
            ]

        perColor : List (Html.Attribute msg)
        perColor =
            if affinity == AffinityAll then
                let
                    rainbowGradient : String
                    rainbowGradient =
                        List.range 0 9
                            |> List.map
                                (\i ->
                                    ( let
                                        c : Color.Color
                                        c =
                                            Color.hsl
                                                ((290 - toFloat i * 30) / 360)
                                                1
                                                0.5
                                      in
                                      Color.toCssString c
                                    , i * 10
                                    )
                                )
                            |> linearGradient 90

                    whiteRadial : String
                    whiteRadial =
                        "radial-gradient(ellipse,rgba(255, 255, 255, 0.9) 0%, rgba(255, 255, 255, 0.8) 60%, rgba(255, 255, 255, 0) 100%)"
                in
                [ Html.Attributes.style "background" (String.join ", " [ whiteRadial, rainbowGradient ])
                , Html.Attributes.style "padding" "5px 12px"
                , Html.Attributes.style "border" "none"
                , Html.Attributes.style "color" "black"
                , Html.Attributes.style "text-shadow" "1px 2px 1px gray, 1px -1px 0px gray"
                ]

            else
                let
                    affinityColor : String
                    affinityColor =
                        colorToCss (Generated.Affinity.affinityToColor affinity |> opaque)

                    whiteGradient : String
                    whiteGradient =
                        linearGradient 180 [ ( "#FFFFFF80", 0 ), ( "#FFFFFF00", 30 ) ]
                in
                [ Html.Attributes.style "background" (String.join ", " [ whiteGradient, affinityColor ])
                , Html.Attributes.style "padding" "3px 10px"
                , Html.Attributes.style "border" ("2px solid " ++ affinityColor)
                , Html.Attributes.style "color" "white"
                , Html.Attributes.style "text-shadow" "1px 2px 1px black, 1px -1px 0px black"
                ]
    in
    Html.div
        (common ++ perColor)
        [ Html.text (Types.affinityToString affinity) ]


slotToColor : Slot -> Int
slotToColor slot =
    case slot of
        SlotFolk ->
            colors.folk

        SlotNoble ->
            colors.noble

        SlotHeroic ->
            colors.heroic

        SlotEpic ->
            colors.epic

        SlotWhite ->
            colors.white


column : List (Attribute msg) -> List (Element msg) -> Element msg
column attrs children =
    Element.column (spacing :: attrs) children


doubleColumn : List (Attribute msg) -> ( Length, Length ) -> List (Element msg) -> Element msg
doubleColumn attrs ( leftLength, rightLength ) children =
    Element.table (spacing :: attrs)
        { columns =
            [ { header = Element.none
              , view = \r -> r |> List.head |> Maybe.withDefault Element.none
              , width = leftLength
              }
            , { header = Element.none
              , view = \r -> r |> List.drop 1 |> List.head |> Maybe.withDefault Element.none
              , width = rightLength
              }
            ]
        , data = List.Extra.greedyGroupsOf 2 children
        }


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
                    expandButton : Element msg
                    expandButton =
                        button
                            [ rounded
                            , Element.padding 4
                            , borderColor colors.choice
                            ]
                            { onPress = Just <| tag <| Types.nextDisplay display
                            , label =
                                case display of
                                    DisplayFull ->
                                        gradient triangleDown

                                    DisplayCompact ->
                                        gradient triangleRight

                                    DisplayCollapsed ->
                                        gradient triangleUp
                            }
                in
                [ hr
                , gradient label
                , text " "
                , expandButton
                , hr
                ]

            Nothing ->
                [ hr
                , gradient label
                , hr
                ]


triangleDown : String
triangleDown =
    "▼"


triangleUp : String
triangleUp =
    "▲"


triangleRight : String
triangleRight =
    "▶"


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
    Element.el [] <| Element.html (viewAffinityBadge affinity)


cardRoundness : Int
cardRoundness =
    72


card :
    List (Attribute msg)
    ->
        { display : Display
        , forceShow : Bool
        , onPress : Maybe msg
        , glow : Int
        , isSelected : Bool
        , imageHeight : Int
        , imageAttrs : List (Attribute msg)
        , image : Image
        , inFront : List (Element msg)
        , content : List (Element msg)
        }
    -> Maybe (Element msg)
card attrs config =
    let
        inner : Bool -> Maybe (Element msg)
        inner compact =
            let
                cardAttributes : List (Attribute msg)
                cardAttributes =
                    [ height fill
                    , Font.color <| rgb 0 0 0
                    , if compact then
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
                    , if compact then
                        width <| Element.minimum 500 fill

                      else
                        width <| Element.minimum 320 <| Element.maximum 400 fill
                    ]

                imageSizeAttrs : List (Attribute msg)
                imageSizeAttrs =
                    if compact then
                        [ width <| Element.maximum (config.imageHeight * 3 // 2) fill
                        , height <| Element.minimum config.imageHeight fill
                        ]

                    else
                        [ height <| px config.imageHeight
                        , width fill
                        ]

                content : List (Element msg)
                content =
                    [ el
                        (Border.rounded cardRoundness
                            :: Background.image config.image.src
                            :: imageSizeAttrs
                            ++ List.map Element.inFront config.inFront
                            ++ config.imageAttrs
                        )
                        Element.none
                    , column
                        [ padding
                        , height fill
                        , width fill
                        ]
                        config.content
                    ]
            in
            maybeButton (cardAttributes ++ attrs)
                { label =
                    if compact then
                        Element.row [ height fill, width fill ] content

                    else
                        Element.column [ height fill, width fill ] content
                , onPress = config.onPress
                }
                |> Just
    in
    case config.display of
        DisplayFull ->
            inner False

        DisplayCompact ->
            if config.forceShow || config.isSelected then
                inner True

            else
                Nothing

        DisplayCollapsed ->
            Nothing


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


button :
    List (Attribute msg)
    ->
        { onPress : Maybe msg
        , label : Element msg
        }
    -> Element msg
button attrs =
    Input.button
        (Border.width 1
            :: Element.padding 4
            :: rounded
            :: attrs
        )


complicationCategoryToColor : ComplicationCategory -> Int
complicationCategoryToColor category =
    case category of
        ComplicationCategoryWorldShift ->
            colors.worldShift


complicationCategoryToGradient : ComplicationCategory -> List ( Int, Int, Int )
complicationCategoryToGradient category =
    case category of
        ComplicationCategoryWorldShift ->
            Gradients.greenGradient


classToBadge : Class -> Image
classToBadge class =
    case class of
        ClassAcademic ->
            Images.badgeAcademic

        ClassSorceress ->
            Images.badgeSorceress

        ClassWarlock ->
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


viewClasses : Int -> List Types.Class -> Element msg
viewClasses w classes =
    case classes of
        [] ->
            Element.none

        [ c ] ->
            classToBadge c
                |> image [ width <| px w ]

        _ ->
            let
                sector : Int
                sector =
                    360 // List.length classes

                viewSlice : Int -> Types.Class -> Attribute msg
                viewSlice i class =
                    let
                        from : String
                        from =
                            String.fromInt (sector * i)

                        to : String
                        to =
                            String.fromInt (sector * (i + 1))
                    in
                    Html.img
                        [ Html.Attributes.src (classToBadge class).src
                        , Html.Attributes.style "mask-image"
                            ("conic-gradient("
                                ++ String.join ", "
                                    [ "transparent " ++ from ++ "deg"
                                    , "black " ++ from ++ "deg"
                                    , "black " ++ to ++ "deg"
                                    , "transparent " ++ to ++ "deg"
                                    ]
                                ++ ")"
                            )
                        , Html.Attributes.style "width" (String.fromInt w ++ "px")
                        ]
                        []
                        |> Element.html
                        |> Element.inFront
            in
            Element.el
                (width (px w)
                    :: height (px w)
                    :: List.indexedMap viewSlice classes
                )
                Element.none


slider :
    List (Attribute msg)
    ->
        { onChange : Float -> msg
        , label : Input.Label msg
        , min : Float
        , max : Float
        , value : Float
        , thumb : Maybe Input.Thumb
        , step : Maybe Float
        }
    -> Element msg
slider attrs config =
    let
        backgroundLine : Element msg
        backgroundLine =
            el
                [ width fill
                , height (px 2)
                , centerY
                , Background.color <| rgb 0.7 0.7 0.7
                , Border.rounded 2
                ]
                Element.none
    in
    Input.slider
        (Element.behindContent backgroundLine :: attrs)
        { onChange = config.onChange
        , label = config.label
        , min = min config.min (min config.max config.value)
        , max = max config.max (max config.min config.value)
        , value = config.value
        , thumb = Maybe.withDefault Input.defaultThumb config.thumb
        , step = config.step
        }
