module Main exposing (Flags, Model, Msg, main)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Element exposing (Element, alignRight, centerX, column, el, fill, fillPortion, moveDown, newTabLink, paragraph, px, rgb, row, scrollbars, text, textColumn, width)
import Element.Background as Background
import Element.Font as Font
import Gradients
import Html
import Images
import Theme exposing (cyan, gradientText, speech)
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
        [ width fill
        , scrollbars
        , Font.color <| rgb 1 1 1
        , Background.color <| rgb 0 0 0
        ]
        [ title
        , intro
        , trueForm
        ]


title : Element msg
title =
    column
        [ width fill
        ]
        [ paragraph
            [ Font.family [ Font.typeface "Bebas Neue" ]
            , Font.size 180
            , Font.center
            , moveDown 14
            ]
            [ gradientText 8 Gradients.titleGradient "Witch Awakening 3.x"
            ]
        , column
            [ centerX
            , Element.onRight <|
                paragraph
                    [ alignRight
                    , Font.alignLeft
                    , width <| px 320
                    , Font.size 14
                    , Element.paddingEach { left = 20, top = 10, right = 0, bottom = 0 }
                    ]
                    [ cyan "TL;DR? You should be able to navigate this cyoa reading only blue text if you see a text wall. Not counting option descriptions, of course."
                    ]
            , Element.paddingEach { left = 0, top = 0, right = 0, bottom = 10 }
            ]
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


intro : Element msg
intro =
    row []
        [ Theme.image [ width fill ] Images.penelope
        , textColumn
            [ width <| fillPortion 2
            , alignRight
            ]
            [ paragraph [] [ text "It's been one of those days. Whatever that means for you, you just woke up on the wrong side of the bed and it's been downhill since then. First you stubbed a toe before you left your bedroom, then you dropped your lunch somehow. Everything's just been off today, something isn't quite right and you feel like you aren't used to your body for some reason, as though not used to the length of your arms, your height feels abnormal. It's like when you think of a word too hard and now it suddenly seems strange. Weird but tolerable on its own, you were really looking forward to lunch and now it's all over the floor. However you'd normally react to that, you eventually take a deep breath and go for a walk. It just feels appropriate, you want to get out and don't care where you go." ]
            , paragraph [] [ text "30 minutes later you're leaning on a railing overlooking a large public pond breathing in some calming crisp air as a light mist seems to be building up helping to drown the world out just a little, and the off feeling of the day fades into the background. As it does, before your very eyes you see a shimmering distortion over the water that unfurls like a curtain revealing a small building in the middle of the pond, a narrow wood boardwalk leading to its door where an \"Open\" sign hangs on the door." ]
            , paragraph []
                [ text "You're pretty sure that wasn't there ten seconds ago, disregarding that you watched to appear in no uncertain terms. It appears to be a kind of shop with a display window full of antiques, and it says it's open... obviously, "
                , el [ Font.italic ] <| text "you should probably check it out. "
                , cyan "[Y/N] [N... Game Over.] [Y]"
                , text " Stepping onto the wood, it seems real enough. The polished wooden door doesn't creak when you open it but it chimes a little bell while you soak in the assault of information in the room inside. For one, it's much larger than the outside suggested, and it's cram packed full of tables, counters, shelves, filled with antiques illuminated in a reddish glow from the many silk drapes over the windows filtering the light. From behind a counter you see a sleepy looking woman, resting with her cheek in one palm until a "
                , el [ Font.bold, Font.italic ] <| text "\"Mooo!\""
                , text " from an unseen source somewhere shakes her awake and she jolts up "
                , speech "Wha- OH!"
                , text " She perks up 0-10 and locks eyes on you "
                , speech "A new witch at this hour! Goodness! I wasn't expecting anyone until tomorrow."
                , text " Witch? She is dressed as though it were Halloween with her comically large hat and by kringle himself you've never seen so much cleavage in your life. Sensing some incredulousness, she continues without skipping a beat. "
                , speech "Don't worry, I get it, I do this for a living. Walking up new witches is my thing. Free of charge, with just a little bit of guided meditation and you're in for a new life of magic, and seeing what the world is really like..."
                , text " She snaps her fingers and an equally voluptuous maid with a cowbell choker appears pushing a cushioned chair, and the lights dim leaving only purple candle-like flames hovering around the strange woman. "
                , speech "Just relax... take my hand, and look into my eyes. That's right."
                , text " ..."
                ]
            ]
        ]


trueForm : Element msg
trueForm =
    text "TODO"


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
