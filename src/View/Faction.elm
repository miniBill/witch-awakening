module View.Faction exposing (viewFaction)

import Data.Faction as Faction
import Element exposing (Element, alignBottom, alignTop, centerX, column, el, fill, fillPortion, height, moveDown, px, rgb, rgba, shrink, spacing, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Generated.Types as Types exposing (Faction)
import Gradients
import Images exposing (Image)
import Theme exposing (gradientText)
import Types exposing (Choice(..), Display(..))
import View


viewFaction : Display -> Maybe ( Faction, Bool ) -> Element Choice
viewFaction display faction =
    View.collapsible (Theme.topBackground Images.factionIntro)
        display
        DisplayFaction
        ChoiceFaction
        "# Factions"
        [ Theme.column
            [ Theme.padding
            , spacing 32
            , width fill
            ]
            [ Theme.blocks
                [ Background.color <| rgba 0 0 0 0.75
                , Theme.rounded
                , Theme.padding
                , width <| Element.maximum 800 fill
                , centerX
                ]
                (Faction.intro ++ String.repeat 4 "\n" ++ Faction.summaries)
            ]
        , Faction.witchFactions
            |> List.map (factionBox display faction)
            |> Theme.column
                [ width fill
                , spacing <| Theme.rythm * 3
                ]
        , Theme.column
            [ Background.image Images.factionHumansIntro1.src
            , width fill
            , Theme.padding
            ]
            [ el [ height <| px 200 ] Element.none
            , Theme.row [ width fill ]
                [ Theme.blocks
                    [ width fill
                    , Background.color <| rgba 1 1 1 0.75
                    , Font.color <| rgb 0 0 0
                    , Font.center
                    , Theme.padding
                    , Theme.rounded
                    ]
                    Faction.humansIntro
                , Theme.image
                    [ width fill
                    , moveDown <| 40 + Theme.rythm * 3.5
                    ]
                    Images.factionHumansIntro2
                ]
            , el [ height <| px 40 ] Element.none
            ]
        , Faction.humanFactions
            |> List.map (factionBox display faction)
            |> Theme.column
                [ width fill
                , spacing <| Theme.rythm * 3
                ]
        ]
        [ (Faction.witchFactions ++ Faction.humanFactions)
            |> List.map (factionBox display faction)
            |> Theme.column
                [ width fill
                , spacing <| Theme.rythm * 3
                ]
        ]


factionBox :
    Display
    -> Maybe ( Faction, Bool )
    -> Faction.Details
    -> Element (Maybe ( Faction, Bool ))
factionBox display selected { name, motto, description, location, relations, perk, perkContent, images } =
    if display /= DisplayFull && Maybe.map Tuple.first selected /= Just name then
        Element.none

    else
        let
            ( isFactionSelected, isPerkSelected ) =
                case selected of
                    Nothing ->
                        ( False, False )

                    Just ( selectedFaction, perkSelected ) ->
                        if selectedFaction == name then
                            ( True, perkSelected )

                        else
                            ( False, False )

            factionGlow : Maybe Int
            factionGlow =
                if isFactionSelected then
                    Just 0x00F3EA6F

                else
                    Nothing

            factionMsg : Maybe ( Faction, Bool )
            factionMsg =
                if isFactionSelected then
                    Nothing

                else
                    Just ( name, False )

            img : Image -> Element msg
            img { src } =
                el
                    [ width fill
                    , height <| Element.minimum 300 fill
                    , Background.image src
                    ]
                    Element.none

            introRow : Element msg
            introRow =
                if display == DisplayFull then
                    Theme.row [ width fill ]
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

                else
                    el [ centerX, Font.size 40, Theme.celticHand ] <|
                        Theme.gradientText 2 Gradients.blueGradient (Types.factionToString name)
        in
        Theme.column [ width fill ]
            [ introRow
            , Theme.row [ width fill ]
                [ Theme.maybeButton
                    [ width <| fillPortion 5
                    , height fill
                    , Font.color <| rgb 0 0 0
                    , case factionGlow of
                        Just color ->
                            Background.color <| Theme.intToBackground color

                        Nothing ->
                            Theme.backgroundColor 0x00C1C1C1
                    , case factionGlow of
                        Just color ->
                            Theme.borderGlow color

                        Nothing ->
                            Border.width 0
                    ]
                    { label =
                        Theme.column []
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
                    , onPress = Just factionMsg
                    }
                , if display /= DisplayFull && not isPerkSelected then
                    Element.none

                  else
                    let
                        perkMsg : Maybe ( Faction, Bool )
                        perkMsg =
                            if isPerkSelected then
                                Just ( name, False )

                            else
                                Just ( name, True )

                        glowColor : Int
                        glowColor =
                            0x00F3EA6F
                    in
                    Theme.card
                        [ if isPerkSelected then
                            Background.color <| Theme.intToBackground glowColor

                          else
                            Theme.backgroundColor 0x00C1C1C1
                        , width fill
                        , height shrink
                        , alignTop
                        ]
                        { display = DisplayFull
                        , forceShow = False
                        , glow = glowColor
                        , isSelected = isPerkSelected
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
                                (perkContent ++ "\n\n_*" ++ Types.factionToMagic name ++ "*_ is half price for you, stacks with affinity.")
                            ]
                        , onPress = Just perkMsg
                        }
                ]
            ]
