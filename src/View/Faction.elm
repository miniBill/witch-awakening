module View.Faction exposing (viewFaction)

import Data.Faction as Faction
import Element exposing (Element, alignBottom, alignTop, centerX, column, el, fill, fillPortion, height, rgb, rgba, shrink, spacing, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Generated.Types as Types exposing (Faction)
import Gradients
import Images exposing (Image)
import Theme exposing (gradientText)
import Types exposing (Choice(..))


viewFaction : Maybe Faction -> Element Choice
viewFaction faction =
    Theme.column
        [ width fill
        , spacing <| Theme.rythm * 2
        ]
        [ Theme.column
            [ Theme.padding
            , spacing 32
            , Background.image Images.factionIntro.src
            , width fill
            ]
            [ el
                [ Theme.morpheus
                , Font.size 52
                , centerX
                ]
                (Theme.gradientText 4 Gradients.yellowGradient "FACTIONS")
            , Theme.blocks
                [ Background.color <| rgba 0 0 0 0.75
                , Border.rounded Theme.rythm
                , Theme.padding
                , width <| Element.maximum 800 fill
                , centerX
                ]
                (Faction.intro ++ String.repeat 4 "\n" ++ Faction.summaries)
            ]
        , Faction.all
            |> List.map (factionBox faction)
            |> Theme.column
                [ width fill
                , spacing <| Theme.rythm * 3
                ]
            |> Element.map ChoiceFaction
        ]


factionBox :
    Maybe Faction
    -> Faction.Details
    -> Element (Maybe Faction)
factionBox selected { name, motto, description, location, relations, perk, perkContent, images } =
    let
        isSelected : Bool
        isSelected =
            case selected of
                Nothing ->
                    False

                Just selectedFaction ->
                    selectedFaction == name

        glow : Maybe Int
        glow =
            if isSelected then
                Just 0x00F3EA6F

            else
                Nothing

        msg : Maybe Faction
        msg =
            if isSelected then
                Nothing

            else
                Just name

        img : Image -> Element msg
        img { src } =
            el
                [ width fill
                , height <| Element.minimum 300 fill
                , Background.image src
                ]
                Element.none
    in
    Theme.column [ width fill ]
        [ Theme.row [ width fill ]
            [ img images.image1
            , Theme.column [ width <| fillPortion 4 ]
                [ img images.image2
                , img images.image3
                , column [ centerX ]
                    [ el [ centerX, Font.size 40, Theme.celticHand ] <|
                        Theme.gradientText 2 Gradients.blueGradient (Types.factionToString name)
                    , el [ centerX, Font.size 24, Theme.morpheus ] <|
                        Theme.gradientText 2 Gradients.yellowGradient motto
                    ]
                ]
            , img images.image4
            ]
        , Theme.row [ width fill ]
            [ Theme.column
                [ width <| fillPortion 5
                , height fill
                , Theme.backgroundColor 0x00C1C1C1
                , Font.color <| rgb 0 0 0
                ]
                [ Theme.blocks
                    [ height fill
                    , Theme.padding
                    ]
                    ("[DESCRIPTION:] " ++ description)
                , Theme.blocks
                    [ height fill
                    , Theme.padding
                    ]
                    ("[LOCATION:] " ++ location)
                , Theme.blocks
                    [ height fill
                    , Theme.padding
                    ]
                    ("[RELATIONS:] " ++ relations)
                ]
            , Theme.card
                [ Theme.backgroundColor 0x00C1C1C1
                , width fill
                , height shrink
                , alignTop
                ]
                { glow = glow
                , imageAttrs = []
                , imageHeight = 240
                , image = images.image5
                , inFront =
                    [ el
                        [ alignBottom
                        , Theme.celticHand
                        , Font.size 24
                        , centerX
                        ]
                        (gradientText 3 Gradients.blueGradient perk)
                    ]
                , content =
                    [ Theme.blocks
                        [ Theme.padding ]
                        perkContent
                    ]
                , onPress = Just msg
                }
            ]
        ]
