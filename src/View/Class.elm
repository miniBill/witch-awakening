module View.Class exposing (viewClass)

import Element exposing (Element, alignBottom, centerX, el, fill, moveDown, moveUp, spacing, width)
import Element.Border as Border
import Element.Font as Font
import Generated.Classes
import Generated.Types as Types exposing (Class)
import Gradients
import Set exposing (Set)
import Theme exposing (gradientText)
import Types exposing (Choice(..), Display)
import View


viewClass : Set String -> Display -> Maybe Class -> Element Choice
viewClass hideDLC display class =
    let
        classBoxes : List (Element (Maybe Class))
        classBoxes =
            Generated.Classes.all
                |> View.filterDLC hideDLC
                |> List.filterMap (classBox display class)
    in
    if List.isEmpty classBoxes then
        Element.none

    else
        View.collapsible []
            display
            DisplayClass
            ChoiceClass
            "# True Form - Class"
            [ Theme.blocks [] intro
            , classBoxes
                |> Theme.wrappedRow
                    [ width fill
                    , spacing <| Theme.rhythm * 3
                    ]
            ]
            [ classBoxes
                |> Theme.column
                    [ width fill
                    , spacing <| Theme.rhythm * 3
                    ]
            ]


intro : String
intro =
    """
    "Ahh, yes... Oh, _wow_! You have an incredible amount of untapped power waiting. First things first: You’ll need your true form! We used to simply wait for it to emerge, but these days we can poke and prod the right places to provoke a controlled early awakening. Most witches have multiple potential true forms and one gets locked in when they finally awaken, but with a controlled environment we can force one of the others. Your options don’t represent all possible outcomes, but let’s see what you have available. First up is what type of witch you are, you can think of it like a “Class” of witch."

    Your witch type determines your method by which you _can naturally progress over time_ towards a power cap. _You_ will have the same power cap and starting power regardless of type "and you’re lucky! You’ve got more than most witches, and it looks like you might be capable of using Rank 5 magic, the _average_ witch only reaches rank 3", You can pre-spend up to your power cap to confirm you have the potential for something to unlock someday, if you wish. It’s up to you how well adapted you are to your starting abilities, perhaps you want to study them for some time before you have a full grasp on them?

    {center}{choice _*Choose one.*_}
    """


classBox :
    Display
    -> Maybe Class
    -> Generated.Classes.Details
    -> Maybe (Element (Maybe Class))
classBox display selected { name, dlc, color, content } =
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
    Theme.card [ Theme.id (Types.classToString name) ]
        { display = display
        , forceShow = selected == Nothing
        , glow = color
        , isSelected = isSelected
        , imageAttrs =
            [ Border.width 8
            , Theme.borderColor color
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
            , case dlc of
                Nothing ->
                    Element.none

                Just dlcName ->
                    el
                        [ centerX
                        , Theme.captureIt
                        , Font.size 24
                        , moveDown 8
                        ]
                        (Theme.gradientText 4 Gradients.purpleGradient dlcName)
            ]
        , content = [ Theme.blocks [] content ]
        , onPress = Just msg
        }
