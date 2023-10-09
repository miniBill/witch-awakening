module Main exposing (Flags, Msg, main)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Element exposing (Element, alignRight, centerX, fill, fillPortion, moveDown, newTabLink, paragraph, px, rgb, scrollbars, text, width)
import Element.Background as Background
import Element.Font as Font
import Element.Lazy
import Generated.Types as Types
import Gradients
import Images
import List.Extra
import String.Multiline
import Theme exposing (gradientText)
import Types exposing (Choice(..), Model)
import Url
import Url.Builder exposing (QueryParameter)
import View.Class as Class
import View.Complications as Complications
import View.Race as Race


type Msg
    = UrlClicked UrlRequest
    | UrlChanged --Url.Url
    | Choice Choice


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
      , race = Nothing
      , complications = []
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

        Choice choice ->
            let
                newModel : Model
                newModel =
                    case choice of
                        ChoiceClass class ->
                            { model | class = class }

                        ChoiceRace race ->
                            { model | race = race }

                        ChoiceComplication complication selected ->
                            if selected then
                                { model | complications = complication :: model.complications }

                            else
                                { model | complications = List.Extra.remove complication model.complications }
            in
            ( newModel
            , Nav.replaceUrl model.key (toUrl newModel)
            )


toUrl : Model -> String
toUrl model =
    let
        pair : String -> (a -> String) -> Maybe a -> Maybe QueryParameter
        pair key f value =
            Maybe.map
                (\v ->
                    Url.Builder.string key (f v)
                )
                value
    in
    [ [ pair "class" Types.classToString model.class
      , pair "race" Types.raceToString model.race
      ]
    , List.map
        (pair "complication"
            (\{ name, kind } ->
                Types.complicationNameToString name ++ Types.complicationKindToString kind
            )
            << Just
        )
        model.complications
    ]
        |> List.concat
        |> List.filterMap identity
        |> Url.Builder.toQuery
        |> (\query -> "#" ++ String.dropLeft 1 query)


view : Model -> Browser.Document Msg
view model =
    { title = ""
    , body =
        [ Element.layoutWith
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
        [ viewTitle
        , viewIntro
        , Element.Lazy.lazy Class.viewClass model.class
        , Element.Lazy.lazy Race.viewRace model.race
        , Element.Lazy.lazy Complications.viewComplications model.complications
        ]
        |> Element.map Choice


viewTitle : Element msg
viewTitle =
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


viewIntro : Element msg
viewIntro =
    Theme.row []
        [ Theme.image [ width fill ] Images.penelope
        , Theme.blocks [ width <| fillPortion 2 ] <| String.Multiline.here """
            It's been one of those days. Whatever that means for you, you just woke up on the wrong side of the bed and it's been downhill since then. First you stubbed a toe before you left your bedroom, then you dropped your lunch somehow. Everything's just been off today, something isn't quite right and you feel like you aren't used to your body for some reason, as though not used to the length of your arms, your height feels abnormal. It's like when you think of a word too hard and now it suddenly seems strange. Weird but tolerable on its own, you were really looking forward to lunch and now it's all over the floor. However you'd normally react to that, you eventually take a deep breath and go for a walk. It just feels appropriate, you want to get out and don't care where you go.

            30 minutes later you're leaning on a railing overlooking a large public pond breathing in some calming crisp air as a light mist seems to be building up helping to drown the world out just a little, and the off feeling of the day fades into the background. As it does, before your very eyes you see a shimmering distortion over the water that unfurls like a curtain revealing a small building in the middle of the pond, a narrow wood boardwalk leading to its door where an “Open” sign hangs on the door.

            You're pretty sure that wasn't there ten seconds ago, disregarding that you watched to appear in no uncertain terms. It appears to be a kind of shop with a display window full of antiques, and it says it's open... obviously, _you should probably check it out._ {choice [Y/N] [N... Game Over.] [Y]} Stepping onto the wood, it seems real enough. The polished wooden door doesn't creak when you open it but it chimes a little bell while you soak in the assault of information in the room inside. For one, it's much larger than the outside suggested, and it's cram packed full of tables, counters, shelves, filled with antiques illuminated in a reddish glow from the many silk drapes over the windows filtering the light. From behind a counter you see a sleepy looking woman, resting with her cheek in one palm until a _*Mooo!*_ from an unseen source somewhere shakes her awake and she jolts up "Wha- OH!" She perks up 0-10 and locks eyes on you "A new witch at this hour! Goodness! I wasn't expecting anyone until tomorrow." Witch? She is dressed as though it were Halloween with her comically large hat and by kringle himself you've never seen so much cleavage in your life. Sensing some incredulousness, she continues without skipping a beat. "Don't worry, I get it, I do this for a living. Walking up new witches is my thing. Free of charge, with just a little bit of guided meditation and you're in for a new life of magic, and seeing what the world is really like..." She snaps her fingers and an equally voluptuous maid with a cowbell choker appears pushing a cushioned chair, and the lights dim leaving only purple candle-like flames hovering around the strange woman. "Just relax... take my hand, and look into my eyes. That's right." ...
            """
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
