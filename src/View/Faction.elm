module View.Faction exposing (viewFaction)

import Color exposing (Color)
import Data.Faction as Faction
import Element exposing (Element, alignBottom, alignTop, centerX, column, el, fill, fillPortion, height, inFront, moveDown, paddingXY, rgb, rgba, shrink, spacing, width)
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
import Theme
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
factionBox display selected details =
    if display /= DisplayFull && Maybe.map Tuple.first selected /= Just details.name then
        Nothing

    else
        Theme.column [ width fill, Theme.id (Types.factionToString details.name) ]
            [ introRow display details
            , Theme.wrappedRow [ width fill ]
                [ content selected details
                , viewPerk display selected details
                ]
            ]
            |> Just


content :
    Maybe ( Faction, Bool )
    -> Faction.Details
    -> Element (Maybe ( Faction, Bool ))
content selected { name, description, location, relations } =
    let
        isFactionSelected : Bool
        isFactionSelected =
            case selected of
                Nothing ->
                    False

                Just ( selectedFaction, _ ) ->
                    selectedFaction == name

        factionGlow : Maybe Color
        factionGlow =
            if isFactionSelected then
                Just (Color.rgb255 0xF3 0xEA 0x6F)

            else
                Nothing

        factionMsg : Maybe ( Faction, Bool )
        factionMsg =
            if isFactionSelected then
                Nothing

            else
                Just ( name, False )
    in
    Theme.maybeButton
        [ width <| fillPortion 5
        , height fill
        , Font.color <| rgb 0 0 0
        , case factionGlow of
            Just color ->
                Theme.backgroundColor <| Theme.colorToBackground color

            Nothing ->
                Theme.backgroundColor (Color.rgb255 0xC1 0xC1 0xC1)
        , case factionGlow of
            Just color ->
                Theme.borderGlow color

            Nothing ->
                Border.width 0
        ]
        { label =
            Theme.column [ alignTop ]
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


viewPerk :
    Display
    -> Maybe ( Faction, Bool )
    -> Faction.Details
    -> Element (Maybe ( Faction, Bool ))
viewPerk display selected { name, perk, perkContent, images } =
    let
        isPerkSelected : Bool
        isPerkSelected =
            selected == Just ( name, True )
    in
    if display /= DisplayFull && not isPerkSelected then
        Element.none

    else
        let
            glowColor : Color
            glowColor =
                Color.rgb255 0xF3 0xEA 0x6F

            perkMsg : Maybe ( Faction, Bool )
            perkMsg =
                Just ( name, not isPerkSelected )
        in
        Theme.card
            [ if isPerkSelected then
                Theme.backgroundColor <| Theme.colorToBackground glowColor

              else
                Theme.backgroundColor (Color.rgb255 0xC1 0xC1 0xC1)
            , width <| Element.minimum 150 fill
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
                [ Theme.gradientTextWrapped
                    [ alignBottom
                    , Theme.celticHand
                    , Font.size 24
                    , centerX
                    , paddingXY 8 0
                    ]
                    3
                    Gradients.blueGradient
                    perk
                ]
            , content =
                case List.Extra.find (\magic -> magic.faction == Just name) Generated.Magic.all of
                    Nothing ->
                        [ Theme.blocks [] perkContent ]

                    Just magic ->
                        [ Theme.blocks []
                            (perkContent ++ "\n\n_*" ++ Types.magicToString magic.name ++ "*_ is half price for you, stacks with affinity.")
                        ]
            , onPress = Just perkMsg
            }
            |> Maybe.withDefault Element.none


introRow :
    Display
    ->
        { a
            | name : Faction
            , dlc : Maybe String
            , motto : String
            , images : { image1 : Image, image2 : Image, image3 : Image, image4 : Image, image5 : Image }
        }
    -> Element msg
introRow display { name, dlc, motto, images } =
    let
        img : Image -> Element msg
        img { src } =
            el
                [ width <| Element.minimum 100 fill
                , height <| Element.minimum 300 fill
                , Background.image src
                ]
                Element.none
    in
    if display == DisplayFull then
        Theme.wrappedRow
            [ width fill
            , inFront
                (case dlc of
                    Nothing ->
                        Element.none

                    Just dlcName ->
                        el
                            [ centerX
                            , Theme.captureIt
                            , Font.size 24
                            , moveDown 4
                            ]
                            (Theme.gradientText 4 Gradients.purpleGradient dlcName)
                )
            ]
            [ img images.image1
            , Theme.column [ width <| fillPortion 4 ]
                [ img images.image2
                , img images.image3
                , column [ width fill ]
                    [ Theme.gradientTextWrapped
                        [ width fill
                        , Font.size 40
                        , Theme.celticHand
                        ]
                        2
                        Gradients.blueGradient
                        (Types.factionToString name)
                    , Theme.gradientTextWrapped
                        [ width fill
                        , Font.size 24
                        , Theme.morpheus
                        ]
                        2
                        Gradients.yellowGradient
                        motto
                    ]
                ]
            , img images.image4
            ]

    else
        Theme.gradientTextWrapped
            [ width fill
            , Font.size 40
            , Theme.celticHand
            ]
            2
            Gradients.blueGradient
            (Types.factionToString name)
