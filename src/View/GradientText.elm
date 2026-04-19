module View.GradientText exposing (Attribute, Font, andalus, bebasNeue, blueGradient, captureIt, celticHand, dlc, fontSize, gradient, grayGradient, html, magicTheGathering, magicianGradient, mirageGothic, morpheus, mortis, noFont, orangeGradient, outlineSize, purpleGradient, sFTechnodelight, span, split, starDust, text, titleGradient, wrapped)

import Element exposing (Attribute, Element)
import Generated.Fonts as Fonts
import Generated.Gradient as Gradient
import Html exposing (Html)
import Html.Attributes
import List.Extra



-- Types --


type Attribute
    = OutlineSize Float
    | Font (Maybe Font)
    | FontSize Int
    | Gradient (List ( Int, Int, Int ))


fontSize : Int -> Attribute
fontSize =
    FontSize


outlineSize : Float -> Attribute
outlineSize s =
    OutlineSize s


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



-- Fonts --


noFont : Attribute
noFont =
    Font Nothing


captureIt : Attribute
captureIt =
    Font (Just CaptureIt)


celticHand : Attribute
celticHand =
    Font (Just CelticHand)


bebasNeue : Attribute
bebasNeue =
    Font (Just BebasNeue)


morpheus : Attribute
morpheus =
    Font (Just Morpheus)


sFTechnodelight : Attribute
sFTechnodelight =
    Font (Just SFTechnodelight)


starDust : Attribute
starDust =
    Font (Just StarDust)


mirageGothic : Attribute
mirageGothic =
    Font (Just MirageGothic)


mortis : Attribute
mortis =
    Font (Just Mortis)


magicTheGathering : Attribute
magicTheGathering =
    Font (Just MagicTheGathering)


andalus : Attribute
andalus =
    Font (Just Andalus)



-- Gradients --


gradient : List ( Int, Int, Int ) -> Attribute
gradient g =
    Gradient g


blueGradient : Attribute
blueGradient =
    Gradient Gradient.blueGradient


purpleGradient : Attribute
purpleGradient =
    Gradient Gradient.purpleGradient


orangeGradient : Attribute
orangeGradient =
    Gradient Gradient.orangeGradient


grayGradient : Attribute
grayGradient =
    Gradient Gradient.grayGradient


titleGradient : Attribute
titleGradient =
    Gradient Gradient.titleGradient


magicianGradient : Attribute
magicianGradient =
    Gradient Gradient.magicianGradient



-- Presets --


dlc : List Attribute
dlc =
    [ captureIt
    , fontSize 24
    , purpleGradient
    ]



-- Usage --


wrapped : List (Element.Attribute msg) -> List Attribute -> String -> Element msg
wrapped attrs config str =
    let
        combinedAttrs : List (Element.Attribute msg)
        combinedAttrs =
            Element.spacing 1
                :: Element.htmlAttribute (Html.Attributes.class "centerWrap")
                :: attrs
    in
    split config str
        |> Element.wrappedRow combinedAttrs


split : List Attribute -> String -> List (Element msg)
split attrs str =
    str
        |> String.replace "-" "-\u{200B}"
        |> String.split " "
        |> List.map
            (\word ->
                word
                    |> String.split "\u{200B}"
                    |> List.map (\p -> text [] attrs (p ++ "\u{200B}"))
            )
        |> List.Extra.intercalate [ Element.text " " ]


text : List (Element.Attribute msg) -> List Attribute -> String -> Element msg
text attrs config str =
    html config str
        |> Element.html
        |> Element.el attrs


span : List Attribute -> String -> Html msg
span attrs value =
    html (fontSize 20 :: attrs) value


html : List Attribute -> String -> Html msg
html attrs str =
    let
        config :
            { font : Maybe Font
            , outlineSize : Float
            , fontSize : Maybe Int
            , gradient : List ( Int, Int, Int )
            }
        config =
            List.foldl
                (\attr acc ->
                    case attr of
                        Font font ->
                            { acc | font = font }

                        OutlineSize size ->
                            { acc | outlineSize = size }

                        FontSize size ->
                            { acc | fontSize = Just size }

                        Gradient g ->
                            { acc | gradient = g }
                )
                { font = Just CaptureIt
                , gradient = Gradient.yellowGradient
                , outlineSize = 4
                , fontSize = Nothing
                }
                attrs
    in
    Html.span
        (fontToAttributes config.font
            ++ (case config.fontSize of
                    Nothing ->
                        []

                    Just px ->
                        [ Html.Attributes.style "font-size" (String.fromInt px ++ "px") ]
               )
            ++ [ Html.Attributes.class "outlined"
               , Html.Attributes.attribute "data-text" str
               , config.gradient
                    |> List.map rgbToString
                    |> String.join ", "
                    |> (\joined -> "--text-stroke: " ++ String.fromFloat config.outlineSize ++ "px #000; --background: linear-gradient(to bottom, " ++ joined ++ ")")
                    |> Html.Attributes.attribute "style"
               ]
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
