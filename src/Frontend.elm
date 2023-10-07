module Frontend exposing (app)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Element exposing (Element, centerX, column, el, htmlAttribute, image, paragraph, scrollbars, text)
import Element.Font as Font
import Html.Attributes
import Images
import Lamdera
import Types exposing (..)
import Url


app :
    { init : Lamdera.Url -> Nav.Key -> ( FrontendModel, Cmd FrontendMsg )
    , view : FrontendModel -> Browser.Document FrontendMsg
    , update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
    , updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
    , subscriptions : FrontendModel -> Sub FrontendMsg
    , onUrlRequest : UrlRequest -> FrontendMsg
    , onUrlChange : Url.Url -> FrontendMsg
    }
app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , view = view
        , subscriptions = subscriptions
        }


init : Url.Url -> Nav.Key -> ( FrontendModel, Cmd FrontendMsg )
init _ key =
    ( { key = key
      , position = ( 0, 0 )
      }
    , Cmd.none
    )


update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
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

        NoOpFrontendMsg ->
            ( model, Cmd.none )


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )


view : FrontendModel -> Browser.Document FrontendMsg
view model =
    { title = ""
    , body =
        [ Element.layout [] <| innerView model
        ]
    }


innerView : FrontendModel -> Element FrontendMsg
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


subscriptions : FrontendModel -> Sub FrontendMsg
subscriptions _ =
    Sub.none
