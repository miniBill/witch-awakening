module View.GameMode exposing (viewGameMode)

import Color exposing (Color)
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
                    slotDescription

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
            [ Theme.blocks [] intro
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


intro : String
intro =
    """
    {choice You can only choose one game mode, or none to play the default way. Each supports a different type of player, and what you might want out of it.}
    """


slotDescription : String
slotDescription =
    """
    Slot modes ignore the requirement of 3 ➡️ 4 ➡️ 5 for acquiring magics. A slot is a slot and stands on its own, and is meant to be simplified

    Slots mode is quite simplified.

    - (folk) {folk *FOLK*} slots can be used to obtain anything in the cyoa marked with the blue slot token.
    - (noble) {noble *NOBLE*} slots are a green token.
    - (heroic) {heroic *HEROIC*} slots use a gold token.
    - (epic) {epic *EPIC*} slots use a purple token.
    - (white) {white White} tokens are for variable options granting or requiring slots of a custom value based on the Power granted or required of the option in question, can use multiple slots per individual segments of a variable option, like each 4p class of Summer School.

    *Folk* = 1-4p. *Noble* = 5-8p. *Heroic* = 9-12p. *Epic* = 13+.

    You can also use this to break down any slot gains from sources that may have given you power later on.

    _*Complications*_ are marked with a dot of the type that complication grants.

    In _Constellation_ mode, complication dots can be stacked or broken down like Skill Tree is capable of doing.

    In _Skill Tree mode_, you can reserve the slots from complications for later use.

    *Options with a cost of 0* or less that would normally be free as a result of your class, can be treated as a white token “Free” slot.
    """
