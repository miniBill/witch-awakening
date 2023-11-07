module View.Class exposing (viewClass)

import Data.Class as Class
import Element exposing (Element, alignBottom, centerX, el, fill, height, moveUp, spacing, width)
import Element.Border as Border
import Element.Font as Font
import Generated.Types as Types exposing (Class)
import Gradients
import Theme exposing (gradientText)
import Types exposing (Choice(..), Display)
import View


viewClass : Display -> Maybe Class -> Element Choice
viewClass display class =
    let
        classBoxes : List (Element (Maybe Class))
        classBoxes =
            Class.all
                |> List.map (classBox display class)
    in
    View.collapsible []
        display
        DisplayClass
        ChoiceClass
        Class.title
        [ Theme.blocks [] Class.intro
        , classBoxes
            |> Theme.wrappedRow
                [ width fill
                , spacing <| Theme.rythm * 3
                ]
        ]
        [ classBoxes
            |> Theme.column
                [ width fill
                , spacing <| Theme.rythm * 3
                ]
        ]


classBox :
    Display
    -> Maybe Class
    -> Class.Details
    -> Element (Maybe Class)
classBox display selected { name, content } =
    let
        isSelected : Bool
        isSelected =
            case selected of
                Nothing ->
                    False

                Just selectedClass ->
                    selectedClass == name

        msg : Maybe Class
        msg =
            if isSelected then
                Nothing

            else
                Just name
    in
    Theme.card []
        { display = display
        , forceShow = selected == Nothing
        , glow = Theme.classToColor name
        , isSelected = isSelected
        , imageAttrs =
            [ Border.width 8
            , Theme.borderColor <| Theme.classToColor name
            ]
        , imageHeight = 400
        , image = Types.classToImage name
        , inFront =
            [ el
                [ alignBottom
                , Theme.morpheus
                , Font.size 56
                , centerX
                , moveUp 8
                ]
                (gradientText 4 Gradients.yellowGradient <|
                    Types.classToString name
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
