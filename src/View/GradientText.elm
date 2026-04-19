module View.GradientText exposing (Config, Font(..), default, dlc, html, span, split, text, wrapped)

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
    | MirageGothic
    | Mortis
    | MagicTheGathering
    | Andalus


type alias Config =
    { font : Maybe Font
    , fontSize : Maybe Int
    , outlineSize : Float
    , gradient : List ( Int, Int, Int )
    }


default : Config
default =
    { font = Just CaptureIt
    , fontSize = Nothing
    , outlineSize = 4
    , gradient = Gradient.yellowGradient
    }


dlc : Config
dlc =
    { font = Just CaptureIt
    , fontSize = Just 24
    , outlineSize = 4
    , gradient = Gradient.purpleGradient
    }


wrapped : List (Attribute msg) -> Config -> String -> Element msg
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


split : Config -> String -> List (Element msg)
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


text : List (Attribute msg) -> Config -> String -> Element msg
text attrs config str =
    html [] config str
        |> Element.html
        |> Element.el attrs


span : Config -> String -> Html msg
span config value =
    html
        []
        { config
            | fontSize =
                case config.fontSize of
                    Just _ ->
                        config.fontSize

                    Nothing ->
                        Just 20
        }
        value


html : List (Html.Attribute msg) -> Config -> String -> Html msg
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
            ++ (case config.fontSize of
                    Nothing ->
                        []

                    Just fontSize ->
                        [ Html.Attributes.style "font-size" (String.fromInt fontSize ++ "px") ]
               )
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


fontToAttributes : Maybe Font -> List (Html.Attribute msg)
fontToAttributes font =
    case font of
        Just CaptureIt ->
            [ Fonts.captureIt
            , Html.Attributes.style "text-transform" "uppercase"
            ]

        Just CelticHand ->
            [ Fonts.celticHand ]

        Just BebasNeue ->
            [ Fonts.bebasNeue ]

        Just Morpheus ->
            [ Fonts.morpheus ]

        Just SFTechnodelight ->
            [ Fonts.sFTechnodelight ]

        Just StarDust ->
            [ Fonts.starDust
            , Html.Attributes.style "text-transform" "uppercase"
            ]

        Just MirageGothic ->
            [ Fonts.mirageGothic ]

        Just Mortis ->
            [ Fonts.mortis ]

        Just MagicTheGathering ->
            [ Fonts.magicTheGathering ]

        Just Andalus ->
            [ Fonts.andalus ]

        Nothing ->
            []
