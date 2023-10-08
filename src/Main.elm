module Main exposing (Flags, Msg, main)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Element exposing (Element, alignBottom, alignRight, centerX, el, fill, fillPortion, height, inFront, moveDown, moveUp, newTabLink, paragraph, px, rgb, scrollbars, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Lazy
import Gradients
import Html
import Images exposing (Image)
import String.Multiline
import Theme exposing (gradientText)
import Types exposing (Class(..), Model)
import Url


type Msg
    = UrlClicked UrlRequest
    | UrlChanged --Url.Url
    | Class (Maybe Class)


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
      , class = Nothing
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

        Class class ->
            ( { model | class = class }, Cmd.none )


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

            @font-face {
                font-family: "Capture It";
                src: url("public/CaptureIt.ttf");
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
        , Element.layoutWith
            { options =
                [ Element.focusStyle
                    { borderColor = Nothing
                    , backgroundColor = Nothing
                    , shadow = Nothing
                    }
                ]
            }
            [ Font.size 16 ]
            (innerView model)
        ]
    }


innerView : Model -> Element Msg
innerView model =
    Theme.column
        [ width fill
        , scrollbars
        , Font.color <| rgb 1 1 1
        , Background.color <| rgb 0 0 0
        , Theme.padding
        ]
        [ title
        , intro
        , Element.Lazy.lazy trueForm model.class
        ]


title : Element msg
title =
    Element.column [ width fill ]
        [ paragraph
            [ Theme.bebasNeue
            , Font.size 180
            , Font.center
            , moveDown 16
            ]
            [ gradientText 8 Gradients.titleGradient "Witch Awakening"
            ]
        , Element.column
            [ centerX
            , Element.onRight <|
                paragraph
                    [ alignRight
                    , Font.alignLeft
                    , width <| px 320
                    , Font.size 14
                    , Element.paddingEach { left = 20, top = 10, right = 0, bottom = 0 }
                    ]
                    [ Theme.choice "TL;DR? You should be able to navigate this cyoa reading only blue text if you see a text wall. Not counting option descriptions, of course."
                    ]
            , Element.paddingEach { left = 0, top = 0, right = 0, bottom = 10 }
            ]
            [ paragraph
                [ Theme.morpheus
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
                    { label = Theme.choice "By OutrageousBears"
                    , url = "https://old.reddit.com/user/OutrageousBears"
                    }
                ]
            ]
        ]


intro : Element msg
intro =
    Theme.row []
        [ Theme.image [ width fill ] Images.penelope
        , Theme.blocks [ width <| fillPortion 2 ] <| String.Multiline.here """
            It's been one of those days. Whatever that means for you, you just woke up on the wrong side of the bed and it's been downhill since then. First you stubbed a toe before you left your bedroom, then you dropped your lunch somehow. Everything's just been off today, something isn't quite right and you feel like you aren't used to your body for some reason, as though not used to the length of your arms, your height feels abnormal. It's like when you think of a word too hard and now it suddenly seems strange. Weird but tolerable on its own, you were really looking forward to lunch and now it's all over the floor. However you'd normally react to that, you eventually take a deep breath and go for a walk. It just feels appropriate, you want to get out and don't care where you go.

            30 minutes later you're leaning on a railing overlooking a large public pond breathing in some calming crisp air as a light mist seems to be building up helping to drown the world out just a little, and the off feeling of the day fades into the background. As it does, before your very eyes you see a shimmering distortion over the water that unfurls like a curtain revealing a small building in the middle of the pond, a narrow wood boardwalk leading to its door where an “Open” sign hangs on the door.

            You're pretty sure that wasn't there ten seconds ago, disregarding that you watched to appear in no uncertain terms. It appears to be a kind of shop with a display window full of antiques, and it says it's open... obviously, _you should probably check it out._ {choice [Y/N] [N... Game Over.] [Y]} Stepping onto the wood, it seems real enough. The polished wooden door doesn't creak when you open it but it chimes a little bell while you soak in the assault of information in the room inside. For one, it's much larger than the outside suggested, and it's cram packed full of tables, counters, shelves, filled with antiques illuminated in a reddish glow from the many silk drapes over the windows filtering the light. From behind a counter you see a sleepy looking woman, resting with her cheek in one palm until a _*Mooo!*_ from an unseen source somewhere shakes her awake and she jolts up "Wha- OH!" She perks up 0-10 and locks eyes on you "A new witch at this hour! Goodness! I wasn't expecting anyone until tomorrow." Witch? She is dressed as though it were Halloween with her comically large hat and by kringle himself you've never seen so much cleavage in your life. Sensing some incredulousness, she continues without skipping a beat. "Don't worry, I get it, I do this for a living. Walking up new witches is my thing. Free of charge, with just a little bit of guided meditation and you're in for a new life of magic, and seeing what the world is really like..." She snaps her fingers and an equally voluptuous maid with a cowbell choker appears pushing a cushioned chair, and the lights dim leaving only purple candle-like flames hovering around the strange woman. "Just relax... take my hand, and look into my eyes. That's right." ...
            """
        ]


trueForm : Maybe Class -> Element Msg
trueForm class =
    Theme.column [ width fill ]
        [ Theme.blocks [] <| String.Multiline.here """
            # True Form - Class

            "Ahh, yes... Oh, _wow_! You have an incredible amount of untapped power waiting. First things first: You'll need your true form! We used to simply wait for it to emerge, but these days we can poke and prod the right places to provoke a controlled early awakening. Most witches have multiple potential true forms and one gets locked in when they finally awaken, but with a controlled environment we can force one of the others. Your options don't represent all possible outcomes, but let's see what you have available. First up is what type of witch you are, you can think of it like a “Class” of witch."

            Your witch type determines your method by which you _can naturally progress over time_ towards a power cap. _You_ will have the same power cap and starting power regardless of type "and you're lucky! You've got more than most witches, and it looks like you might be capable of using Rank 5 magic, the _average_ witch only reaches rank 3", You can pre-spend up to your power cap to confirm you have the potential for something to unlock someday, if you wish. It's up to you how well adapted you are to your starting abilities, perhaps you want to study them for some time before you have a full grasp on them?

            [center]{choice _*Choose one.*_}
            """
        , Theme.wrappedRow
            [ width fill
            , spacing <| Theme.rythm * 3
            ]
            [ academic class
            , sorceress class
            , warlock class
            ]
        ]


academic : Maybe Class -> Element Msg
academic class =
    classBox class
        { class = Academic
        , image = Images.academic
        , content = """
        Academics are studious and focus easily on tasks, while training and studying to further their magic. Their thorough approach to magic tends to be slower if you want to have a life outside of studies, but the most rewarding as they comprehend in more depth and their growth is in their own hands, advancing as slow or fast as the time and effort they put in. Academics gain 1 {academic _*Focus*_} for every day in which they averaged 4 hours of study, 2 for 8 hours. You can use focus to buy a Power point for 10 Focus. This cost increases by 10 Focus per purchase. (10, 20, 30, etc)

        __Start with [30] power__. Player academics eventually cap out at [100] _power_ before other factors like complications. Any option marked with the {academic *blue*} icon has a flat cost reduction of [-2] Power to its price, which can reduce it to [0], or become a gain of [1] point, or complications give [2] more power. This applies only once per option. Once per Magic specialization, once per perk, etc, and always applies first.

        *Advantage*: General use magic and classics. Academics favor mixed bags like Potions, Runes, and Portals. Any magic marked with blue used by an academic produces twice the yield or is half as time consuming, mana draining, or tiresome to use. Two potions for the price of one, two runes for the price of one, ect. Any duration of a blue marked magic effect applied by you, to you or to another, lasts twice as long For example, double the duration of a temporary potion.

        An Academic can study to master any two schools of magic for free, but takes time to learn equal to if you were saving the power to buy it, but no Power is spent.
        """
        }


sorceress : Maybe Class -> Element Msg
sorceress class =
    classBox class
        { class = Sorceress
        , image = Images.sorceress
        , content = """
    Sorceresses are inherently imbued with magic as natural extensions of their will so they tend to be more in tune with their bodies and grow through tactile training. They're naturals but tend to have less of a tangible understanding of how and why magic works or interesting implications of magical theory. Fireballs go boom, ain't gotta explain sheit. Sorceresses gain 1 {sorceress _*Might*_} for every day in which they averaged 1 hour of straining practice, 2 for 4 hours. You can use Might to buy a Power point for 10 Might. This cost increases by 10 Might per purchase. (10, 20, 30, etc)

    __Start with [30] power__. Player sorceresses eventually cap out at [100] _power_ before other factors like complications. Any option marked with the {sorceress *red*} icon has a flat cost reduction of [-2] Power to its price, which can reduce it to [0], or become a gain of [1] point, or complications give [2] more power. This applies only once per option. Once per Magic specialization, once per perk, etc, and always applies first.

    *Advantage*: Inner power and direct combat usage. Sorceresses favor direct magic like Elementalist magic. They are not limited by affinity when buying elementalist magics of affinities they do not have, and one that matches their affinities can be taken for free.

    Sorceresses have stronger and more unique auras that are like beacons to anyone who can detect them. This aura can color any elemental magic the sorceress uses, such as white flames, gold stone, black water, or prismatic wind. This includes Naturalism. If they choose to use colored elementalism, then that magic is 50% more damaging, with 50% larger areas of effect and range.
    """
        }


warlock : Maybe Class -> Element Msg
warlock class =
    classBox class
        { class = Warlock
        , image = Images.warlock
        , content = """
    Warlocks are endowed with power from some third party. Their power can't be taken back afterward anymore than such an entity might be capable of stealing power from any other witch. Instead of studying, or training, they spend time in service, partnership, employ, or worship to a patron. They grow by gaining with their patron(s), by doing quests, the Warlock gains {warlock _*Favor*_} equal to the Reward value of the quest, Warlocks can trade Favor 1-1 directly for Power due to the scarcity, being dependent on Quests. A Warlock can continue to do quests without a quest slot, but doing so offers no rewards except Favor.

    __Start with [30] power__. Player warlocks eventually cap out at [100] _power_ before other factors like complications. Any option marked with the icon has a flat cost reduction of [-2] Power to its price, which can reduce it to [0], or become a gain of [1] point, or complications give [2] more power. This applies only once per option. Once per Magic specialization, once per perk, etc, and always applies first.

    *Advantage*: Darker and external magics, Relic usage. Warlocks favor indirect power like Hexes and Curses. They have a personalized brand they can mark on any relic they own or willing creature. They always know the location of one of their marks and when anyone else touches it, and a stronger sensation like an alarm if any harm comes to it. Branded creatures can be affected by the warlock's magic at any distance and the mark can be the target of things such as scrying even if the warlock doesn't know where it is.

    Warlocks can immediately start with 20 Reward Points to purchase relics that are infused in their own soul, summoned the same way as Mothergifts. (See Witchery)
    """
        }


classBox :
    Maybe Class
    ->
        { class : Class
        , image : Image
        , content : String
        }
    -> Element Msg
classBox selected { class, image, content } =
    let
        isSelected : Bool
        isSelected =
            case selected of
                Nothing ->
                    False

                Just selectedClass ->
                    selectedClass == class

        roundness : Int
        roundness =
            72
    in
    Input.button
        [ height fill
        , width fill
        , Font.color <| rgb 0 0 0
        , Background.color <| rgb 1 1 1
        , Border.roundEach
            { topLeft = roundness
            , topRight = roundness
            , bottomLeft = 8
            , bottomRight = 8
            }
        , if isSelected then
            Border.glow (Theme.intToColor <| Theme.classToColor class) 8

          else
            Border.width 0
        ]
        { label =
            Element.column [ height fill ]
                [ el
                    [ Border.width 8
                    , Theme.borderColor <| Theme.classToColor class
                    , width fill
                    , height <| px 400
                    , Border.rounded roundness
                    , inFront <|
                        el
                            [ alignBottom
                            , Theme.morpheus
                            , Font.size 56
                            , centerX
                            , moveUp 8
                            ]
                            (gradientText 4 Gradients.yellowGradient <|
                                Types.classToString class
                            )
                    , Background.image image.src
                    ]
                    Element.none
                , Theme.blocks
                    [ height fill
                    , Theme.padding
                    ]
                    content
                ]
        , onPress =
            Just <|
                Class <|
                    if isSelected then
                        Nothing

                    else
                        Just class
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
