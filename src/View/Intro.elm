module View.Intro exposing (viewIntro, viewTitle)

import Element exposing (Element, alignRight, centerX, el, fill, fillPortion, newTabLink, paragraph, text, width)
import Element.Font as Font
import Generated.Attribution
import Generated.Gradient as Gradient
import Generated.Image as Image
import Html.Attributes
import List.Extra
import Theme exposing (Font(..))
import Types exposing (IdKind(..))


viewTitle : Bool -> Element msg
viewTitle allCompact =
    let
        mainLogo : Element msg
        mainLogo =
            Theme.gradientTextWrapped BebasNeue
                [ Theme.centerWrap
                , centerX
                , Font.size 160
                , Element.paddingEach
                    { top = Theme.rhythm * 2
                    , bottom = 0
                    , left = Theme.rhythm * 2
                    , right = Theme.rhythm * 2
                    }
                , Element.htmlAttribute (Html.Attributes.style "overflow" "clip")
                ]
                8
                Gradient.titleGradient
                "Wit\u{200B}ch Awa\u{200B}ken\u{200B}ing"
    in
    if allCompact then
        mainLogo

    else
        Theme.column
            [ width fill ]
            [ mainLogo
            , Theme.column
                [ centerX
                , Element.paddingEach
                    { left = Theme.rhythm
                    , top = 0
                    , right = Theme.rhythm
                    , bottom = Theme.rhythm * 2
                    }
                ]
                [ paragraph
                    [ Font.center
                    , Font.size 52
                    ]
                    [ Theme.gradientText Morpheus [] 4 Gradient.grayGradient "Heavy Metal"
                    , text " "
                    , Theme.gradientText Morpheus [] 4 Gradient.yellowGradient "&"
                    , text " "
                    , Theme.gradientText Morpheus [] 4 Gradient.orangeGradient "Witch Party"
                    , text " "
                    , Theme.gradientText Morpheus [] 4 Gradient.yellowGradient "Update"
                    ]
                , paragraph
                    [ alignRight
                    , Font.alignLeft
                    , Font.size 14
                    , Element.paddingEach { left = 12, top = 0, right = 0, bottom = 0 }
                    ]
                    [ Theme.choice "TL;DR? You should be able to navigate this cyoa reading only blue text if you see a text wall. Not counting option descriptions, of course."
                    ]
                , paragraph
                    [ Font.alignRight
                    , width fill
                    , Font.size 14
                    , Font.underline
                    , Element.paddingEach { left = 0, top = 0, right = 0, bottom = Theme.rhythm }
                    ]
                    [ newTabLink []
                        { label = Theme.choice "By OutrageousBears"
                        , url = "https://old.reddit.com/user/OutrageousBears"
                        }
                    ]
                , paragraph
                    [ Font.alignRight
                    , width fill
                    , Font.size 14
                    , Element.spacing Theme.rhythm
                    ]
                    (text "With DLCs: "
                        :: (Generated.Attribution.all
                                |> List.sortBy .name
                                |> List.map viewDLCAttribution
                                |> List.Extra.intercalate [ text ", " ]
                           )
                    )
                ]
            ]


viewDLCAttribution :
    { name : String
    , author : String
    , link : Maybe String
    }
    -> List (Element msg)
viewDLCAttribution dlcAttribution =
    let
        by : Element msg
        by =
            Theme.choice (" By " ++ dlcAttribution.author)
    in
    (dlcAttribution.name
        |> String.replace "The " "The\u{00A0}"
        |> String.replace "the " "the\u{00A0}"
        |> String.replace "of " "of\u{00A0}"
        |> String.replace "Of " "Of\u{00A0}"
        |> Theme.gradientTextSplit CaptureIt 4 Gradient.purpleGradient
        |> List.map (el [ Font.size 20 ])
    )
        ++ [ case dlcAttribution.link of
                Just url ->
                    newTabLink [ Font.underline ]
                        { label = by
                        , url = url
                        }

                Nothing ->
                    by
           ]


viewIntro : Element msg
viewIntro =
    Theme.wrappedRow [ Element.paddingXY 16 0 ]
        [ Theme.image [ width <| Element.minimum 200 fill ] Image.penelope
        , Theme.blocks [ width <| Element.minimum 200 <| fillPortion 2 ] IdKindGameMode mainIntro
        ]


mainIntro : String
mainIntro =
    """
    It’s been one of those days. Whatever that means for you, you just woke up on the wrong side of the bed and it’s been downhill since then. First you stubbed a toe before you left your bedroom, then you dropped your lunch somehow. Everything’s just been off today, something isn’t quite right and you feel like you aren’t used to your body for some reason, as though not used to the length of your arms, your height feels abnormal. It’s like when you think of a word too hard and now it suddenly seems strange. Weird but tolerable on its own, you were really looking forward to lunch and now it’s all over the floor. However you’d normally react to that, you eventually take a deep breath and go for a walk. It just feels appropriate, you want to get out and don’t care where you go.

    30 minutes later you’re leaning on a railing overlooking a large public pond breathing in some calming crisp air as a light mist seems to be building up helping to drown the world out just a little, and the off feeling of the day fades into the background. As it does, before your very eyes you see a shimmering distortion over the water that unfurls like a curtain revealing a small building in the middle of the pond, a narrow wood boardwalk leading to its door where an “Open” sign hangs on the door.

    You’re pretty sure that wasn’t there ten seconds ago, disregarding that you watched to appear in no uncertain terms. It appears to be a kind of shop with a display window full of antiques, and it says it’s open... obviously, _you should probably check it out._ {choice [Y/N] [N... Game Over.] [Y]} Stepping onto the wood, it seems real enough. The polished wooden door doesn’t creak when you open it but it chimes a little bell while you soak in the assault of information in the room inside. For one, it’s much larger than the outside suggested, and it’s cram packed full of tables, counters, shelves, filled with antiques illuminated in a reddish glow from the many silk drapes over the windows filtering the light. From behind a counter you see a sleepy looking woman, resting with her cheek in one palm until a _*Mooo!*_ from an unseen source somewhere shakes her awake and she jolts up "Wha- OH!" She perks up 0-10 and locks eyes on you "A new witch at this hour! Goodness! I wasn’t expecting anyone until tomorrow." Witch? She is dressed as though it were Halloween with her comically large hat and by kringle himself you’ve never seen so much cleavage in your life. Sensing some incredulousness, she continues without skipping a beat. "Don’t worry, I get it, I do this for a living. Walking up new witches is my thing. Free of charge, with just a little bit of guided meditation and you’re in for a new life of magic, and seeing what the world is really like..." She snaps her fingers and an equally voluptuous maid with a cowbell choker appears pushing a cushioned chair, and the lights dim leaving only purple candle-like flames hovering around the strange woman. "Just relax... take my hand, and look into my eyes. That’s right." ...
    """
