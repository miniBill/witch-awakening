module Main exposing (Flags, Model, Msg, main)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Element exposing (Element, alignRight, centerX, column, el, fill, htmlAttribute, image, inFront, moveDown, newTabLink, paragraph, px, rgb, rgb255, row, scrollbars, text, width)
import Element.Background as Background
import Element.Font as Font
import Gradients
import Html
import Html.Attributes
import Images
import Url


type Msg
    = UrlClicked UrlRequest
    | UrlChanged Url.Url


type alias Model =
    { key : Nav.Key }


type alias Flags =
    {}


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ _ key =
    ( { key = key
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged _ ->
            ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = ""
    , body =
        [ Html.node "style"
            []
            [ Html.text """
            @font-face {
                font-family: "Bebas Neue";
                src: url("public/BebasNeue.otf");
            }

            @font-face {
                font-family: "Morpheus";
                src: url("public/Morpheus.ttf");
            }

            @font-face {
                font-family: "Celtic Hand";
                src: url("public/CelticHand.ttf");
            }

            .outlined {
                position: relative;
                color: transparent;
            }

            .outlined:after {
                background: none;
                content: attr(data-text);
                left: 0;
                position: absolute;
                z-index: 1;
                -webkit-text-stroke: var(--text-stroke);
            }

            .outlined:before {
                -webkit-background-clip: text;
                -webkit-text-fill-color: transparent;
                background-image: var(--background);
                content: attr(data-text);
                left: 0;
                position: absolute;
                z-index: 2;
            }
            """
            ]
        , Element.layout [] <| innerView model
        ]
    }


innerView : Model -> Element Msg
innerView _ =
    column
        [ scrollbars
        , Font.color <| rgb 1 1 1
        , Background.color <| rgb 0 0 0
        ]
    <|
        title
            :: intro
            :: trueForm
            :: List.map viewImage Images.list


title : Element msg
title =
    column
        [ width fill
        ]
        [ paragraph
            [ Font.family [ Font.typeface "Bebas Neue" ]
            , Font.size 180
            , Font.center
            , moveDown 10
            ]
            [ gradientText 8 Gradients.titleGradient "Witch Awakening 3.x"
            ]
        , row
            [ width fill
            , inFront <|
                paragraph
                    [ alignRight
                    , Font.alignLeft
                    , width <| px 400
                    , Font.size 14
                    , Element.paddingEach { left = 0, top = 0, right = 100, bottom = 0 }
                    , moveDown 10
                    ]
                    [ cyan "TL;DR? You should be able to navigate this cyoa reading only blue text if you see a text wall. Not counting option descriptions, of course."
                    ]
            ]
            [ column [ centerX ]
                [ paragraph
                    [ Font.family [ Font.typeface "Morpheus" ]
                    , Font.size 50
                    ]
                    [ gradientText 4 Gradients.grayGradient "Heavy Metal"
                    , text " "
                    , gradientText 4 Gradients.yellowGradient "&"
                    , text " "
                    , gradientText 4 Gradients.orangeGradient "Witch Party"
                    , text " "
                    , gradientText 4 Gradients.yellowGradient "Update"
                    ]
                , paragraph
                    [ Font.alignRight
                    , width fill
                    , Font.size 14
                    , Font.underline
                    ]
                    [ newTabLink []
                        { label = cyan "By OutrageousBears"
                        , url = "https://old.reddit.com/user/OutrageousBears"
                        }
                    ]
                ]
            ]
        ]


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


intro : Element msg
intro =
    text "TODO"


trueForm : Element msg
trueForm =
    text "TODO"


viewImage : { width : Int, height : Int, url : String } -> Element msg
viewImage { url } =
    image [ htmlAttribute <| Html.Attributes.style "max-width" "100%" ]
        { src = url
        , description = ""
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
