module View.GradientText exposing (Font(..), default, fontToAttributes, html, rgbToString, span, split, text, wrapped)

import Element exposing (Attribute, Element)
import Generated.Fonts as Fonts
import Generated.Gradient as Gradient
import Html exposing (Html)
import Html.Attributes
import List.Extra


type Font
    = CaptureIt
    | CelticHand
    | BebasNeue
    | Morpheus
    | SFTechnodelight
    | StarDust
    | NoFont


default :
    { font : Font
    , outlineSize : Float
    , gradient : List ( Int, Int, Int )
    }
default =
    { font = CaptureIt
    , outlineSize = 4
    , gradient = Gradient.yellowGradient
    }


wrapped :
    List (Attribute msg)
    ->
        { font : Font
        , outlineSize : Float
        , gradient : List ( Int, Int, Int )
        }
    -> String
    -> Element msg
wrapped attrs config str =
    let
        combinedAttrs : List (Attribute msg)
        combinedAttrs =
            Element.spacing 1
                :: Element.htmlAttribute (Html.Attributes.class "centerWrap")
                :: attrs
    in
    split config str
        |> Element.wrappedRow combinedAttrs


split :
    { font : Font
    , outlineSize : Float
    , gradient : List ( Int, Int, Int )
    }
    -> String
    -> List (Element msg)
split config str =
    str
        |> String.replace "-" "-\u{200B}"
        |> String.split " "
        |> List.map
            (\word ->
                word
                    |> String.split "\u{200B}"
                    |> List.map (\p -> text [] config (p ++ "\u{200B}"))
            )
        |> List.Extra.intercalate [ Element.text " " ]


text :
    List (Attribute msg)
    ->
        { font : Font
        , outlineSize : Float
        , gradient : List ( Int, Int, Int )
        }
    -> String
    -> Element msg
text attrs ({ font, outlineSize, gradient } as config) str =
    html [] config str
        |> Element.html
        |> Element.el attrs


span :
    { font : Font
    , gradient : List ( Int, Int, Int )
    , outlineSize : Float
    }
    -> String
    -> Html msg
span config value =
    html [ Html.Attributes.style "font-size" "20px" ] config value


html :
    List (Html.Attribute msg)
    ->
        { font : Font
        , gradient : List ( Int, Int, Int )
        , outlineSize : Float
        }
    -> String
    -> Html msg
html attrs config str =
    Html.span
        ([ Html.Attributes.class "outlined"
         , Html.Attributes.attribute "data-text" str
         , config.gradient
            |> List.map rgbToString
            |> String.join ", "
            |> (\joined -> "--text-stroke: " ++ String.fromFloat config.outlineSize ++ "px #000; --background: linear-gradient(to bottom, " ++ joined ++ ")")
            |> Html.Attributes.attribute "style"
         ]
            ++ fontToAttributes config.font
            ++ attrs
        )
        [ Html.text str ]


rgbToString : ( Int, Int, Int ) -> String
rgbToString ( r, g, b ) =
    "rgb("
        ++ String.fromInt r
        ++ " "
        ++ String.fromInt g
        ++ " "
        ++ String.fromInt b
        ++ ")"


fontToAttributes : Font -> List (Html.Attribute msg)
fontToAttributes font =
    case font of
        CaptureIt ->
            [ Fonts.captureIt
            , Html.Attributes.style "text-transform" "uppercase"
            ]

        CelticHand ->
            [ Fonts.celticHand ]

        BebasNeue ->
            [ Fonts.bebasNeue ]

        Morpheus ->
            [ Fonts.morpheus ]

        SFTechnodelight ->
            [ Fonts.sFTechnodelight ]

        StarDust ->
            [ Fonts.starDust
            , Html.Attributes.style "text-transform" "uppercase"
            ]

        NoFont ->
            []
