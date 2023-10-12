module View.Class exposing (viewClass)

import Data.Class as Class
import Element exposing (Element, alignBottom, centerX, el, fill, height, moveUp, spacing, width)
import Element.Border as Border
import Element.Font as Font
import Generated.Types as Types exposing (Class)
import Gradients
import String.Multiline
import Theme exposing (gradientText)
import Types exposing (Choice(..))


viewClass : Maybe Class -> Element Choice
viewClass class =
    Theme.column
        [ width fill
        , spacing <| Theme.rythm * 2
        ]
        [ Theme.blocks [] <| String.Multiline.here """
            # True Form - Class

            "Ahh, yes... Oh, _wow_! You have an incredible amount of untapped power waiting. First things first: You'll need your true form! We used to simply wait for it to emerge, but these days we can poke and prod the right places to provoke a controlled early awakening. Most witches have multiple potential true forms and one gets locked in when they finally awaken, but with a controlled environment we can force one of the others. Your options don't represent all possible outcomes, but let's see what you have available. First up is what type of witch you are, you can think of it like a “Class” of witch."

            Your witch type determines your method by which you _can naturally progress over time_ towards a power cap. _You_ will have the same power cap and starting power regardless of type "and you're lucky! You've got more than most witches, and it looks like you might be capable of using Rank 5 magic, the _average_ witch only reaches rank 3", You can pre-spend up to your power cap to confirm you have the potential for something to unlock someday, if you wish. It's up to you how well adapted you are to your starting abilities, perhaps you want to study them for some time before you have a full grasp on them?

            {center}{choice _*Choose one.*_}
            """
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
