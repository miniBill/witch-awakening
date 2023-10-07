module Main exposing (Flags, Model, Msg, main)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Element exposing (Element, centerX, column, el, fill, htmlAttribute, image, paragraph, rgb, rgb255, scrollbars, text, width)
import Element.Background as Background
import Element.Font as Font
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
                src: url("public/BebasNeue-Regular.otf");
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
        [ Font.center
        , centerX
        , Background.color <| rgb 0.2 0 0
        , width fill
        ]
        [ paragraph
            [ Font.family [ Font.typeface "Bebas Neue" ]
            , Font.size 140
            , Font.center
            , style "background" "linear-gradient(to bottom, #443D09 0%, #F4EB6F 25%, #A88E2A 50%, #D9C955 75%, #CCB545 100%)"
            , style "-webkit-background-clip" "text"
            , style "-webkit-text-fill-color" "transparent"
            , style "-webkit-text-stroke" "3px black"
            ]
            [ text "Witch Awakening 3.x" ]
        , text "By [OutrageousBears](https://old.reddit.com/user/OutrageousBears) [gray](Heavy Metal) & [orange](Witch Party) Update"
        , text "[cyan](TL;DR You should be able to navigate this cyoa reading only blue text if you see a text wall. Not counting option descriptions, of course.)"
        ]


hex : Int -> Element.Color
hex value =
    rgb255
        (value // 65536)
        (modBy 256 (value // 256))
        (modBy 256 value)


style : String -> String -> Element.Attribute msg
style key value =
    Element.htmlAttribute <| Html.Attributes.style key value


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
