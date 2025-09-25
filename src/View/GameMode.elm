module View.GameMode exposing (viewGameMode)

import Color exposing (Color)
import Data.GameMode as GameMode
import Element exposing (Element, alignBottom, alignTop, centerX, el, fill, moveDown, moveUp, spacing, width)
import Element.Border as Border
import Element.Font as Font
import Generated.GameMode as GameMode
import Generated.Types as Types exposing (GameMode)
import Gradients
import Set exposing (Set)
import Theme exposing (gradientText)
import Types exposing (Choice(..), Display)
import View


viewGameMode : Set String -> Display -> Maybe GameMode -> Element Choice
viewGameMode hideDLC display gameMode =
    let
        filtered : List GameMode.Details
        filtered =
            GameMode.all
                |> View.filterDLC hideDLC
    in
    if List.isEmpty filtered then
        Element.none

    else
        let
            slotsBox : Element msg
            slotsBox =
                Theme.blocks
                    [ width <| Element.maximum 400 fill
                    , alignTop
                    , Border.width 1
                    , Theme.padding
                    , Theme.borderColor Theme.colors.gameMode
                    ]
                    GameMode.slotDescription

            boxes : List (Element (Maybe GameMode))
            boxes =
                filtered
                    |> List.filterMap (gameModeBox display gameMode)
        in
        View.collapsible []
            display
            DisplayGameMode
            ChoiceGameMode
            "# Game Mode"
            [ Theme.blocks [] GameMode.intro
            , (boxes ++ [ slotsBox ])
                |> Theme.wrappedRow
                    [ centerX
                    , spacing <| Theme.rhythm * 3
                    ]
            ]
            [ boxes
                |> Theme.wrappedRow
                    [ width fill
                    , spacing <| Theme.rhythm * 3
                    ]
            ]


gameModeBox :
    Display
    -> Maybe GameMode
    -> GameMode.Details
    -> Maybe (Element (Maybe GameMode))
gameModeBox display selected { name, content } =
    let
        isSelected : Bool
        isSelected =
            selected == Just name

        msg : Maybe GameMode
        msg =
            if isSelected then
                Nothing

            else
                Just name

        gradient : List ( Int, Int, Int )
        gradient =
            Gradients.purpleGradient

        color : Color
        color =
            Theme.colors.gameMode

        gainGradient : Element msg
        gainGradient =
            ""
                |> gradientText 4 Gradients.yellowGradient
    in
    Theme.card [ Theme.id (Types.gameModeToString name) ]
        { display = display
        , forceShow = False
        , glow = color
        , isSelected = isSelected
        , imageAttrs =
            [ Border.width 4
            , Theme.borderColor color
            ]
        , imageHeight = 400
        , image = Types.gameModeToImage name
        , inFront =
            [ Element.column
                [ alignTop
                , Font.size 28
                , centerX
                , moveDown 8
                ]
                [ el [ centerX, Theme.captureIt ] <|
                    gradientText 4 gradient "Game Mode"
                , el [ centerX, Theme.captureIt ] gainGradient
                ]
            , el
                [ alignBottom
                , Theme.celticHand
                , Font.size 32
                , centerX
                , moveUp 4
                ]
                (gradientText 4 gradient <|
                    Types.gameModeToString name
                )
            ]
        , content = [ Theme.blocks [] content ]
        , onPress = Just msg
        }
