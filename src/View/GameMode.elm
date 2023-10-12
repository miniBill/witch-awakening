module View.GameMode exposing (viewGameMode)

import Data.GameMode as GameMode
import Element exposing (Element, alignBottom, alignTop, centerX, el, fill, height, moveDown, moveUp, spacing, width)
import Element.Border as Border
import Element.Font as Font
import Generated.Types as Types exposing (GameMode)
import Gradients
import Theme exposing (gradientText)
import Types exposing (Choice(..))


viewGameMode : Maybe GameMode -> Element Choice
viewGameMode gameMode =
    Theme.column
        [ width fill
        , spacing <| Theme.rythm * 2
        ]
        [ Theme.blocks [] GameMode.intro
        , (List.map
            (gameModeBox gameMode)
            GameMode.all
            ++ [ Theme.blocks
                    [ width <| Element.maximum 400 fill
                    , alignTop
                    , Border.width 1
                    , Theme.padding
                    , Theme.borderColor Theme.colors.gameMode
                    ]
                    GameMode.slotDescription
               ]
          )
            |> Theme.wrappedRow
                [ centerX
                , spacing <| Theme.rythm * 3
                ]
            |> Element.map ChoiceGameMode
        ]


gameModeBox :
    Maybe GameMode
    -> GameMode.Details
    -> Element (Maybe GameMode)
gameModeBox selected { name, content } =
    let
        isSelected : Bool
        isSelected =
            selected == Just name

        glow : Maybe Int
        glow =
            if isSelected then
                Just color

            else
                Nothing

        msg : Maybe GameMode
        msg =
            if isSelected then
                Nothing

            else
                Just name

        gradient : List ( Int, Int, Int )
        gradient =
            Gradients.purpleGradient

        color : Int
        color =
            Theme.colors.gameMode

        gainGradient : Element msg
        gainGradient =
            ""
                |> gradientText 4 Gradients.yellowGradient
    in
    Theme.card
        { glow = glow
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
        , content =
            [ Theme.blocks
                [ height fill
                , Theme.padding
                ]
                content
            ]
        , onPress = Just msg
        }
