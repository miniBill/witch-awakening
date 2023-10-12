module View.Class exposing (viewClass)

import Data.Class as Class
import Element exposing (Element, alignBottom, centerX, el, fill, height, moveUp, spacing, width)
import Element.Border as Border
import Element.Font as Font
import Generated.Types as Types exposing (Class)
import Gradients
import Theme exposing (gradientText)
import Types exposing (Choice(..))


viewClass : Maybe Class -> Element Choice
viewClass class =
    Theme.column
        [ width fill
        , spacing <| Theme.rythm * 2
        ]
        [ Theme.blocks [] Class.intro
        , Class.all
            |> List.map (classBox class)
            |> Theme.wrappedRow
                [ centerX
                , spacing <| Theme.rythm * 3
                ]
            |> Element.map ChoiceClass
        ]


classBox :
    Maybe Class
    -> Class.Details
    -> Element (Maybe Class)
classBox selected { class, content } =
    let
        isSelected : Bool
        isSelected =
            case selected of
                Nothing ->
                    False

                Just selectedClass ->
                    selectedClass == class

        glow : Maybe Int
        glow =
            if isSelected then
                Just <| Theme.classToColor class

            else
                Nothing

        msg : Maybe Class
        msg =
            if isSelected then
                Nothing

            else
                Just class
    in
    Theme.card
        { glow = glow
        , imageAttrs =
            [ Border.width 8
            , Theme.borderColor <| Theme.classToColor class
            ]
        , imageHeight = 400
        , image = Types.classToImage class
        , inFront =
            [ el
                [ alignBottom
                , Theme.morpheus
                , Font.size 56
                , centerX
                , moveUp 8
                ]
                (gradientText 4 Gradients.yellowGradient <|
                    Types.classToString class
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
