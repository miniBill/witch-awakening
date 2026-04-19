module View.Class exposing (viewClass)

import Color
import Element exposing (Attribute, Element, alignBottom, centerX, fill, moveDown, spacing, width)
import Element.Border as Border
import Generated.Class as Class
import Generated.Image as Image
import Generated.Types as Types exposing (Class)
import Set exposing (Set)
import Theme
import Types exposing (Choice(..), Display, IdKind(..))
import View
import View.GradientText as GradientText


viewClass : Set String -> Display -> Maybe Class -> Element Choice
viewClass hideDLC display class =
    let
        filtered : List Class.Details
        filtered =
            Class.all
                |> View.filterDLC hideDLC
    in
    if List.isEmpty filtered then
        Element.none

    else
        let
            classBoxes : List (Element (Maybe Class))
            classBoxes =
                filtered
                    |> List.filterMap (classBox display class)
        in
        View.collapsible []
            display
            DisplayClass
            ChoiceClass
            IdKindClass
            "# True Form - Class"
            [ Theme.blocks [] IdKindClass intro
            , classBoxes
                |> Theme.wrappedRow
                    [ width fill
                    , spacing <| Theme.rhythm * 3
                    , Theme.centerWrap
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
    "Ahh, yes... Oh, _wow_! You have an incredible amount of untapped power waiting. First things first: You’ll need your true form! We used to simply wait for it to emerge, but these days we can poke and prod the right places to provoke a controlled early awakening. Most witches have multiple potential true forms and one gets locked in when they finally awaken, but with a controlled environment we can force one over the others. Your options don’t represent all possible outcomes, but let’s see what you have available. First up is what type of witch you are, you can think of it like a “Class” of witch."

    Your witch type determines the method by which you _can naturally progress over time_ towards a power cap. _You_ will have the same power cap and starting power regardless of type "and you’re lucky! You’ve got more than most witches, and it looks like you might be capable of using Rank 5 magic, normally a strong witch only reaches rank 3", You can pre-spend up to your power cap to confirm you have the potential for something to unlock someday, if you wish. It’s up to you how well adapted you are to your starting abilities, perhaps you want to study them for a while before you have a full grasp on them?

    {center}{choice _*Choose one.*_}
    """


classBox :
    Display
    -> Maybe Class
    -> Class.Details
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
    Theme.card [ Theme.id IdKindClass (Class.toString name) ]
        { display = display
        , forceShow = selected == Nothing
        , glow = color |> Maybe.withDefault Color.white
        , isSelected = isSelected
        , imageAttrs =
            case color of
                Just c ->
                    [ Border.width 8
                    , Theme.borderColor c
                    ]

                Nothing ->
                    wizardBorder
        , imageHeight = 400
        , image = Types.classToImage name
        , inFront =
            [ className name
            , case dlc of
                Nothing ->
                    Element.none

                Just dlcName ->
                    GradientText.wrapped
                        [ centerX
                        , moveDown 8
                        ]
                        GradientText.dlc
                        dlcName
            ]
        , content = [ Theme.blocks [] IdKindClass content ]
        , onPress = Just msg
        }


wizardBorder : List (Attribute msg)
wizardBorder =
    [ Element.padding 0
    , Border.width 8
    , Theme.borderColor (Color.rgba 0 0 0 0)
    , [ Theme.toUrlFunction Image.classWizard
      , "conic-gradient( #ffc282, #fc8161, #fff, #9992ff, #98e586, #9b91fd, #fff, #fd8161, #ffc684, #97e184, #ffc282)"
      ]
        |> String.join ", "
        |> Theme.style "background-image"
    , [ "cover"
      , "auto"
      ]
        |> String.join ", "
        |> Theme.style "background-size"
    , [ "padding-box"
      , "border-box"
      ]
        |> String.join ", "
        |> Theme.style "background-origin"
    ]


className : Class -> Element (Maybe Class)
className name =
    let
        go :
            List (Element.Attribute msg)
            -> List GradientText.Attribute
            -> Element msg
        go attrs config =
            GradientText.wrapped
                (alignBottom
                    :: centerX
                    :: attrs
                )
                (GradientText.fontSize 56 :: config)
                (Class.toString name)
    in
    case name of
        Types.ClassMagician ->
            go
                [ moveDown 8 ]
                [ GradientText.sFTechnodelight
                , GradientText.magicianGradient
                ]

        Types.ClassSorceress ->
            go
                [ moveDown 4, Theme.style "text-shadow" "rgba(255, 17, 0, 0.9) 0px 0px 39px" ]
                [ GradientText.starDust
                , GradientText.gradient
                    [ ( 251, 247, 222 )
                    , ( 233, 213, 139 )
                    ]
                ]

        Types.ClassWarlock ->
            go
                [ moveDown 20 ]
                [ GradientText.mirageGothic
                , GradientText.fontSize 96
                , GradientText.outlineSize 6
                , GradientText.gradient
                    [ ( 207, 207, 207 )
                    , ( 120, 120, 120 )
                    ]
                ]

        Types.ClassWizard ->
            go
                [ moveDown 30 ]
                [ GradientText.magicTheGathering
                , GradientText.fontSize 96
                , GradientText.outlineSize 6
                , GradientText.gradient
                    [ ( 100, 200, 100 )
                    , ( 100, 100, 255 )
                    , ( 255, 255, 255 )
                    , ( 200, 0, 0 )
                    , ( 200, 100, 0 )
                    , ( 100, 100, 200 )
                    ]
                ]

        Types.ClassSlayer ->
            go
                [ moveDown 20 ]
                [ GradientText.andalus
                , GradientText.fontSize 96
                , GradientText.outlineSize 6
                , GradientText.gradient
                    [ ( 255, 206, 40 )
                    , ( 176, 108, 27 )
                    ]
                ]
