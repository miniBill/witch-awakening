module Main exposing (Flags, Model, Msg, main)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Element exposing (Element, alignRight, centerX, centerY, column, el, fill, fillPortion, height, moveDown, newTabLink, padding, paragraph, px, rgb, rgb255, row, scrollbars, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Gradients
import Html
import Html.Attributes
import Images
import String.Multiline
import Theme exposing (bebasNeue, celticHand, cyan, gradientText, morpheus)
import Url


type Msg
    = UrlClicked UrlRequest
    | UrlChanged --Url.Url


type alias Model =
    { key : Nav.Key }


type alias Flags =
    {}


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = \_ -> UrlChanged
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

        UrlChanged ->
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
        , Element.layout [ Font.size 16 ]
            (innerView model)
        ]
    }


innerView : Model -> Element Msg
innerView _ =
    column
        [ width fill
        , scrollbars
        , Font.color <| rgb 1 1 1
        , Background.color <| rgb 0 0 0
        , spacing 8
        , padding 8
        ]
        [ title
        , intro
        , trueForm
        ]


title : Element msg
title =
    column [ width fill ]
        [ paragraph
            [ bebasNeue
            , Font.size 180
            , Font.center
            , moveDown 16
            ]
            [ gradientText 8 Gradients.titleGradient "Witch Awakening"
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
                [ morpheus
                , Font.size 52
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
    row [ spacing 10 ]
        [ Theme.image [ width fill ] Images.penelope
        , Theme.paragraphs [ width <| fillPortion 2 ] <| String.Multiline.here """
            It's been one of those days. Whatever that means for you, you just woke up on the wrong side of the bed and it's been downhill since then. First you stubbed a toe before you left your bedroom, then you dropped your lunch somehow. Everything's just been off today, something isn't quite right and you feel like you aren't used to your body for some reason, as though not used to the length of your arms, your height feels abnormal. It's like when you think of a word too hard and now it suddenly seems strange. Weird but tolerable on its own, you were really looking forward to lunch and now it's all over the floor. However you'd normally react to that, you eventually take a deep breath and go for a walk. It just feels appropriate, you want to get out and don't care where you go.

            30 minutes later you're leaning on a railing overlooking a large public pond breathing in some calming crisp air as a light mist seems to be building up helping to drown the world out just a little, and the off feeling of the day fades into the background. As it does, before your very eyes you see a shimmering distortion over the water that unfurls like a curtain revealing a small building in the middle of the pond, a narrow wood boardwalk leading to its door where an “Open” sign hangs on the door.

            You're pretty sure that wasn't there ten seconds ago, disregarding that you watched to appear in no uncertain terms. It appears to be a kind of shop with a display window full of antiques, and it says it's open... obviously, _you should probably check it out._ {cyan [Y/N] [N... Game Over.] [Y]} Stepping onto the wood, it seems real enough. The polished wooden door doesn't creak when you open it but it chimes a little bell while you soak in the assault of information in the room inside. For one, it's much larger than the outside suggested, and it's cram packed full of tables, counters, shelves, filled with antiques illuminated in a reddish glow from the many silk drapes over the windows filtering the light. From behind a counter you see a sleepy looking woman, resting with her cheek in one palm until a _*Mooo!*_ from an unseen source somewhere shakes her awake and she jolts up "Wha- OH!" She perks up 0-10 and locks eyes on you "A new witch at this hour! Goodness! I wasn't expecting anyone until tomorrow." Witch? She is dressed as though it were Halloween with her comically large hat and by kringle himself you've never seen so much cleavage in your life. Sensing some incredulousness, she continues without skipping a beat. "Don't worry, I get it, I do this for a living. Walking up new witches is my thing. Free of charge, with just a little bit of guided meditation and you're in for a new life of magic, and seeing what the world is really like..." She snaps her fingers and an equally voluptuous maid with a cowbell choker appears pushing a cushioned chair, and the lights dim leaving only purple candle-like flames hovering around the strange woman. "Just relax... take my hand, and look into my eyes. That's right." ...
            """
        ]


trueForm : Element msg
trueForm =
    column [ width fill ]
        [ row
            [ celticHand
            , Font.size 36
            , centerX
            , spacing 8
            ]
            [ hr, gradientText 4 Gradients.blueGradient "TRUE FROM - CLASS", hr ]
        , Theme.paragraphs [] <| String.Multiline.here """
            "Ahh, yes... Oh, _wow_! You have an incredible amount of untapped power waiting. First things first: You'll need your true form! We used to simply wait for it to emerge, but these days we can poke and prod the right places to provoke a controlled early awakening. Most witches have multiple potential true forms and one gets locked in when they finally awaken, but with a controlled environment we can force one of the others. Your options don't represent all possible outcomes, but let's see what you have available. First up is what type of witch you are, you can think of it like a “Class” of witch."

            Your witch type determines your method by which you _can naturally progress over time_ towards a power cap. _You_ will have the same power cap and starting power regardless of type "and you're lucky! You've got more than most witches, and it looks like you might be capable of using Rank 5 magic, the _average_ witch only reaches rank 3", You can pre-spend up to your power cap to confirm you have the potential for something to unlock someday, if you wish. It's up to you how well adapted you are to your starting abilities, perhaps you want to study them for some time before you have a full grasp on them?

            [center]{cyan _*Choose one.*_}
            """
        ]


hr : Element msg
hr =
    el
        [ width <| px 200
        , height <| px 1
        , centerY
        , Border.color <| rgb255 0x00 0xE4 0xFF
        , Border.widthEach
            { top = 1
            , bottom = 0
            , left = 0
            , right = 0
            }
        , Element.htmlAttribute <| Html.Attributes.style "box-shadow" "0px 0px 1px 1px #00E4FF"
        ]
        Element.none


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
