module View.Class exposing (viewClass)

import Data.Class as Class
import Element exposing (Element, alignBottom, centerX, el, fill, height, moveUp, spacing, width)
import Element.Border as Border
import Element.Font as Font
import Generated.Types as Types exposing (Class)
import Gradients
import Theme exposing (gradientText)
import Types exposing (Choice(..), Display(..))


viewClass : Display -> Maybe Class -> Element Choice
viewClass display class =
    Theme.column
        [ width fill
        , spacing <| Theme.rythm * 2
        ]
    <|
        case display of
            DisplayFull ->
                [ Element.map DisplayClass <| Theme.collapsibleBlocks display [] Class.intro
                , Class.all
                    |> List.map (classBox class)
                    |> Theme.wrappedRow
                        [ centerX
                        , spacing <| Theme.rythm * 3
                        ]
                    |> Element.map ChoiceClass
                ]

            DisplayCompact ->
                [ Element.map DisplayClass <| Theme.collapsibleBlocks display [] Class.title
                , Class.all
                    |> List.filter (\{ name } -> Just name == class)
                    |> List.map (classBox class)
                    |> Theme.wrappedRow
                        [ centerX
                        , spacing <| Theme.rythm * 3
                        ]
                    |> Element.map ChoiceClass
                ]

            DisplayCollapsed ->
                [ Element.map DisplayClass <| Theme.collapsibleBlocks display [] Class.title
                ]


classBox :
    Maybe Class
    -> Class.Details
    -> Element (Maybe Class)
classBox selected { name, content } =
    let
        isSelected : Bool
        isSelected =
            case selected of
                Nothing ->
                    False

                Just selectedClass ->
                    selectedClass == name

        glow : Maybe Int
        glow =
            if isSelected then
                Just <| Theme.classToColor name

            else
                Nothing

        msg : Maybe Class
        msg =
            if isSelected then
                Nothing

            else
                Just name
    in
    Theme.card []
        { glow = glow
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
