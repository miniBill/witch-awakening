module View.Faction exposing (viewFaction)

import Data.Faction as Faction
import Element exposing (Element, alignBottom, alignTop, centerX, column, el, fill, fillPortion, height, rgb, rgba, shrink, spacing, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Generated.Faction
import Generated.Magic
import Generated.Types as Types exposing (Faction)
import Gradients
import Images exposing (Image)
import List.Extra
import Set exposing (Set)
import Theme exposing (gradientText)
import Types exposing (Choice(..), Display(..))
import View


viewFaction : Set String -> Display -> Maybe ( Faction, Bool ) -> Element Choice
viewFaction hideDLC display faction =
    let
        filtered : List Faction.Details
        filtered =
            Generated.Faction.all
                |> View.filterDLC hideDLC
    in
    if List.isEmpty filtered then
        Element.none

    else
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
            , filtered
                |> List.Extra.removeWhen .isHuman
                |> List.filterMap (factionBox display faction)
                |> Theme.column
                    [ width fill
                    , spacing <| Theme.rhythm * 3
                    ]
            , Element.row
                [ width fill
                , Background.image Images.factionHumansIntro1.src
                , width fill
                ]
                [ el [ width fill, Element.paddingEach { left = 32, bottom = 64, top = 200, right = 32 } ] <|
                    Theme.blocks
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
                    , alignBottom

                    -- , moveDown <| 40 + Theme.rhythm * 3.5
                    ]
                    Images.factionHumansIntro2
                ]
            , filtered
                |> List.filter .isHuman
                |> List.filterMap (factionBox display faction)
                |> Theme.column
                    [ width fill
                    , spacing <| Theme.rhythm * 3
                    ]
            ]
            [ filtered
                |> List.filterMap (factionBox display faction)
                |> Theme.column
                    [ width fill
                    , spacing <| Theme.rhythm * 3
                    ]
            ]


factionBox :
    Display
    -> Maybe ( Faction, Bool )
    -> Faction.Details
    -> Maybe (Element (Maybe ( Faction, Bool )))
factionBox display selected { name, motto, description, location, relations, perk, perkContent, images } =
    if display /= DisplayFull && Maybe.map Tuple.first selected /= Just name then
        Nothing

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
        Theme.column [ width fill, Theme.id (Types.factionToString name) ]
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
                            case List.Extra.find (\magic -> magic.faction == Just name) Generated.Magic.all of
                                Nothing ->
                                    []

                                Just magic ->
                                    [ Theme.blocks []
                                        (perkContent ++ "\n\n_*" ++ Types.magicToString magic.name ++ "*_ is half price for you, stacks with affinity.")
                                    ]
                        , onPress = Just perkMsg
                        }
                        |> Maybe.withDefault Element.none
                ]
            ]
            |> Just
