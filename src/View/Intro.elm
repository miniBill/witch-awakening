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
                , Font.size 140
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
                "Wit\u{200B}ch Awa\u{200B}ken\u{200B}ing 4"
    in
    if allCompact then
        mainLogo

    else
        Theme.column
            [ width fill ]
            [ paragraph [ Theme.padding, Font.center ]
                [ text "This interactive is in the process of being updated to "
                , Element.newTabLink [ Font.underline ]
                    { url = "https://old.reddit.com/r/makeyourchoice/comments/1sjh0cb/witch_awakening_update_4_blade_grace/"
                    , label = text "version 4"
                    }
                , text ". Version 3.5 will be permanently available "
                , Element.newTabLink [ Font.underline ]
                    { url = "https://witch-awakening-v3.5.taglialegne.it"
                    , label = text "here"
                    }
                , text "."
                ]
            , paragraph [ Theme.padding, Font.center ]
                [ text "Saves from v3.5 should be compatible with v4, if not feel free to message me "
                , Element.newTabLink [ Font.underline ]
                    { url = "https://old.reddit.com/user/cmt_miniBill/"
                    , label = text " on reddit."
                    }
                ]
            , mainLogo
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
                    [ Theme.gradientText Morpheus [] 4 Gradient.orangeGradient "Blade"
                    , text " "
                    , Theme.gradientText Morpheus [] 4 Gradient.grayGradient "&"
                    , text " "
                    , Theme.gradientText Morpheus [] 4 Gradient.blueGradient "Grace"
                    , text " "
                    , Theme.gradientText Morpheus [] 4 Gradient.grayGradient "Update"
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
    You really woke up on the wrong side of the bed today, nothing felt right since before you even opened your eyes. A disconnecting sense of wrongness like you were living a lie, that life was a dream. Your food tasted wrong, colors seemed different but you couldn’t articulate why. The only thing that felt right was the calling. That deep nagging feeling like you should do something kept pulling at your attention all morning, all but demanding you go for a walk.

    Eventually you gave in, following that pull on your mind. At first the way was familiar, until there was a path you’d never noticed before that led you to a small lake you feel like you should have known about. Brushing foliage away from your face that was masking a direct view of the lake, you step across the boundary into the clearing through a sense of resistance as though walking through a curtain of air. With it came a sense of relief and comfort, taking away your unease that clung to you all day, and as it faded away, the lake and your surroundings smoothly bloomed with new vibrance of color and detail. There on the waters you could see more clearly through every blink, a quaint shop sitting atop a tangle of vines reminiscent of spiders legs, and a woody gnarled but elegant bridge arching from its doorstep to the shore.

    It appears to be a kind of antiques store, with a window full of antiques, and it says it’s open... obviously, you should probably check it out. {choice [Y/N?] [N... Game Over.] [Y-]}

    Stepping onto the wood, it seems real enough. The polished wooden door doesn’t creak when you open it but it chimes a little bell while you soak in the assault of information in the room inside. For one, it’s much larger than the outside suggested, and it’s cram packed full of tables, counters, shelves, filled with antiques illuminated in a reddish glow from the many red silk and velvet draperies over the windows.

    From behind a counter you see a sleepy looking woman, resting with her cheek in one palm until a _*“Mooo!”*_ from an unseen cow somewhere shakes her awake and she jolts up "Wha- OH!" She perks up 0-10 and locks eyes on you. "{choice A new witch at this hour!} Goodness! I wasn’t expecting anyone until tomorrow." Witch? She is dressed as though it were Halloween with her comically large hat. Sensing some incredulousness, she continues without skipping a beat. "Don’t worry, I get it, I do this for a living. Waking up new witches is my thing. Free of charge, with just a little bit of guided meditation and you’re in for a new life of magic, and seeing what the world is really like..." She snaps her fingers and an equally voluptuous maid with a cowbell choker appears pushing a cushioned chair, and the lights dim leaving only purple candle-like flames hovering around the strange woman.

    "Just relax... take my hand, and look into my eyes. That’s right." ... Perhaps uncharacteristic to how you may have reacted, you don’t provide resistance.
    """
