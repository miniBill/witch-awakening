module Main exposing (Flags, Model, Msg, main)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Element exposing (Element, centerX, column, htmlAttribute, image, paragraph, scrollbars, text)
import Element.Font as Font
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
        [ Element.layout [] <| innerView model
        ]
    }


innerView : Model -> Element Msg
innerView _ =
    column [ scrollbars ] <|
        title
            :: intro
            :: trueForm
            :: List.map viewImage Images.list


title : Element msg
title =
    column [ Font.center, centerX ]
        [ paragraph [ Font.center ] [ text "Witch Awakening 3.x" ]
        , text "By [OutrageousBears](https://old.reddit.com/user/OutrageousBears) [gray](Heavy Metal) & [orange](Witch Party) Update"
        , text "[cyan](TL;DR You should be able to navigate this cyoa reading only blue text if you see a text wall. Not counting option descriptions, of course.)"
        ]


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
