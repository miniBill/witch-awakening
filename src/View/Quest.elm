module View.Quest exposing (viewQuests)

import Data.Faction as Faction
import Data.Quest as Quest
import Element exposing (Attribute, Element, alignBottom, alignRight, alignTop, centerX, el, fill, height, inFront, moveDown, moveLeft, moveRight, moveUp, padding, px, rgb, rgba, shrink, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Generated.Quest
import Generated.Types as Types exposing (Quest, Slot(..))
import Gradients
import Html.Attributes
import Images
import Theme
import Types exposing (Choice(..), Display(..))
import View


viewQuests : Display -> List Quest -> Element Choice
viewQuests display quests =
    let
        blocks : List (Element ( Quest, Bool ))
        blocks =
            Generated.Quest.all
                |> List.indexedMap (questBox display quests)
                |> List.filterMap identity
    in
    View.collapsible (Theme.topBackground Images.questIntro)
        display
        DisplayQuests
        ChoiceQuest
        "# Quests"
        [ introBlock
        , blocks
            |> Theme.wrappedRow
                [ width fill
                , spacing <| Theme.rhythm * 3
                ]
        ]
        [ blocks
            |> Theme.wrappedRow
                [ width fill
                , spacing <| Theme.rhythm * 3
                ]
        ]


questBox :
    Display
    -> List Quest
    -> Int
    -> Quest.Details
    -> Maybe (Element ( Quest, Bool ))
questBox display selected number quest =
    let
        isSelected : Bool
        isSelected =
            List.member quest.name selected

        nameString : String
        nameString =
            Types.questToString quest.name

        card : Maybe (Element ( Quest, Bool ))
        card =
            Theme.card
                [ Theme.id nameString
                , Border.width 3
                , Theme.borderColor (slotToColor quest.slot)
                , Border.rounded Theme.cardRoundness
                , width <| Element.minimum 460 fill
                , Background.color (rgb 0 0 0)
                , Font.color (rgb 1 1 1)
                ]
                { display =
                    case display of
                        DisplayFull ->
                            DisplayFull

                        DisplayCompact ->
                            -- This looks awful in "compact" mode
                            DisplayFull

                        DisplayCollapsed ->
                            DisplayCollapsed
                , forceShow = False
                , glow = slotToColor quest.slot
                , isSelected = isSelected
                , imageAttrs = []
                , imageHeight = 400
                , image = Types.questToImage quest.name
                , inFront =
                    [ Theme.gradientText 4 Gradients.yellowGradient nameString
                        |> el
                            [ alignBottom
                            , Font.size 32
                            , Theme.captureIt
                            , moveRight 16
                            , moveUp 60
                            ]
                    , case quest.faction of
                        Nothing ->
                            Element.none

                        Just faction ->
                            faction
                                |> Faction.toShortString
                                |> Theme.gradientText 4 Gradients.blueGradient
                                |> el
                                    [ alignBottom
                                    , Font.size 28
                                    , Theme.celticHand
                                    , moveRight 16
                                    , moveUp 92
                                    ]
                    , quest.slot
                        |> Types.slotToString
                        |> Theme.gradientText 4
                            (case quest.slot of
                                Epic ->
                                    Gradients.epicGradient

                                Heroic ->
                                    Gradients.heroicGradient

                                Noble ->
                                    Gradients.nobleGradient

                                _ ->
                                    Gradients.blueGradient
                            )
                        |> el
                            [ alignBottom
                            , alignRight
                            , Font.size 28
                            , Theme.celticHand
                            , moveLeft 16
                            , moveUp 64
                            ]
                    , (case quest.evil of
                        Quest.EvilNo ->
                            Element.none

                        Quest.EvilMaybe ->
                            Theme.image [] Images.questEvilPath

                        Quest.EvilYes ->
                            Theme.image [] Images.questEvil
                      )
                        |> el
                            [ alignRight
                            , moveRight 4
                            , moveUp 4
                            ]
                    , case quest.dlc of
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
                    , statsTable quest
                        |> el
                            [ alignBottom
                            , moveDown 40
                            , width fill
                            , Element.paddingXY Theme.rhythm 0
                            ]
                    , (number + 1)
                        |> String.fromInt
                        |> Theme.gradientText 4 Gradients.yellowGradient
                        |> el
                            [ Font.size 28
                            , Theme.captureIt
                            ]
                    ]
                , content =
                    [ el [ height (px 40) ] Element.none
                    , Theme.blocks
                        [ Element.htmlAttribute (Html.Attributes.class "compact")
                        , spacing 4
                        ]
                        (if List.isEmpty quest.notes then
                            String.trim quest.description

                         else
                            quest.notes
                                |> List.map (\line -> "{center} + " ++ line)
                                |> (::) "{center} *Notes*:"
                                |> (::) (String.trim quest.description)
                                |> String.join "\n\n"
                        )
                    ]
                , onPress = Just ( quest.name, not isSelected )
                }
    in
    Maybe.map
        (\c ->
            Theme.column [ alignTop ]
                (c
                    :: List.map (viewSidebar quest.slot) quest.sidebars
                    ++ (if number == 0 then
                            [ evilSidebar ]

                        else
                            []
                       )
                )
        )
        card


evilSidebar : Element ( Quest, Bool )
evilSidebar =
    Theme.row
        [ alignBottom
        , moveRight 16
        , moveDown 16
        , width (Element.maximum 100 fill)
        , Images.questEvilPath
            |> Theme.image
                [ moveUp 16
                , moveLeft 16
                ]
            |> inFront
        , width (Element.maximum 400 fill)
        , Border.width 1
        , Font.size 14
        , padding (Theme.rhythm // 2)
        ]
        [ el [ width <| px 60 ] Element.none
        , Theme.blocks
            [ Element.htmlAttribute (Html.Attributes.class "compact")
            ]
            sidebar
        ]


statsTable : Quest.Details -> Element msg
statsTable details =
    Element.table
        [ width fill
        , Font.color (rgb 0 0 0)
        ]
        { columns =
            { header = Element.none
            , width = shrink
            , view =
                \v ->
                    case v of
                        Just ( label, _, _ ) ->
                            cellWithLeftBorder
                                [ Background.color (rgb 1 1 1)
                                , Font.bold
                                ]
                                label
                                1
                                (text label)

                        Nothing ->
                            Element.none
            }
                :: List.map
                    statColumn
                    (List.range 1 10)
        , data =
            [ Just ( "THREAT", details.threat, dangerToColor details.threat )
            , Just ( "CONFLICT", details.conflict, dangerToColor details.conflict )
            , Just ( "REWARD", details.reward, scoreToColor details.reward )
            , Nothing
            ]
        }


dangerToColor : Int -> ( Int, Int )
dangerToColor p =
    if p == 10 then
        Theme.colors.companionGold

    else if p >= 7 then
        Theme.colors.companionRed

    else if p >= 4 then
        Theme.colors.companionOrange

    else
        Theme.colors.companionBlue


scoreToColor : Int -> ( Int, Int )
scoreToColor p =
    if p == 10 then
        Theme.colors.companionGold

    else if p >= 7 then
        Theme.colors.companionBlue

    else if p >= 4 then
        Theme.colors.companionOrange

    else
        Theme.colors.companionRed


cellWithLeftBorder : List (Attribute msg) -> String -> Int -> Element msg -> Element msg
cellWithLeftBorder attrs label leftBorder content =
    el
        ([ padding <| Theme.rhythm // 2
         , width fill
         , height fill
         , Border.widthEach
            { top = 1
            , left = leftBorder
            , bottom =
                if label == "" then
                    1

                else
                    0
            , right = 1
            }
         ]
            ++ attrs
        )
        content


statColumn : Int -> Element.Column (Maybe ( String, Int, ( Int, Int ) )) msg
statColumn ranking =
    let
        view : Maybe ( String, Int, ( Int, Int ) ) -> Element msg
        view v =
            case v of
                Just ( label, score, ( mainColor, _ ) ) ->
                    let
                        backgroundColor : Int
                        backgroundColor =
                            if score >= ranking then
                                mainColor

                            else
                                0x00FFFFFF
                    in
                    cellWithLeftBorder
                        [ Theme.backgroundColor backgroundColor
                        ]
                        label
                        0
                        Element.none

                Nothing ->
                    cellWithLeftBorder
                        [ Font.size 10
                        , Font.center
                        , Theme.backgroundColor 0x00FFFFFF
                        ]
                        ""
                        0
                        (text <| String.fromInt ranking)
    in
    { header = Element.none
    , width = fill
    , view = view
    }


slotToColor : Slot -> Int
slotToColor slot =
    case slot of
        White ->
            0x00FFFFFF

        Folk ->
            0xE5E5

        Noble ->
            0x007CC534

        Heroic ->
            0x00C5AA3E

        Epic ->
            0x00FF60A3


viewSidebar : Slot -> String -> Element ( Quest, Bool )
viewSidebar slot content =
    Theme.blocks
        [ width <| Element.maximum 360 fill
        , Theme.padding
        , centerX
        , Border.width 1
        , Theme.borderColor (slotToColor slot)
        , Font.size 14
        , Element.htmlAttribute (Html.Attributes.class "compact")
        ]
        content


introBlock : Element msg
introBlock =
    Theme.column
        [ width fill
        , spacing <| Theme.rhythm * 2
        ]
        [ Theme.gradientText 4 Gradients.yellowGradient "Or \"Plot Hooks\""
            |> el
                [ Font.size 38
                , Theme.morpheus
                , centerX
                , moveUp 8
                ]
        , let
            color : Element.Color
            color =
                rgba 0 0 0 0.75
          in
          Theme.wrappedRow
            [ width <| Element.maximum 800 fill
            , centerX
            , spacing <| 2 * Theme.rhythm
            ]
            [ Theme.blocks
                [ width fill
                , Background.color color
                , Border.shadow
                    { offset = ( 0, 0 )
                    , size = 5
                    , blur = 5
                    , color = color
                    }
                ]
                intro
            ]
        , el [ height <| px 40 ] Element.none
        ]


intro : String
intro =
    """
    Quests are specific events written into your fate and strongly predestined to occur but by no means are the only things you may end up doing. People used to seek out seers, sages, and prophets in order to get information on their quests, but these days it's pretty easy to use this same guided awakening ritual to get a better look at them, though not as detailed as some seers can get. Find an oracle if you want more information, else you'll just figure it out as you go along like most people.

    {choice Choose some quests! You can take a max of __5 folk__ quests, __4 noble__, __2 heroic__, and __1 epic__. Faction quests for your own faction don't count for the limit, but you can also take a faction quest of factions you're on good terms with using your slots.}

    Each quest shows 3 stats. Threat is roughly how dangerous the quest is, the presence of hazards or enemies. Conflict is the severity of which it causes friction with other factions named in the description. Reward is how many reward points are given by the end, which can be used to buy relics you obtain as part of the quest, or can be spent {1} to [1] to acquire power from your power cap early, bypassing time and method of advancement, just not the cap. You can save points for larger relics.

    {choice Quests do not give specific \\"End States\\", being more of a hook or writing prompt, it's up to you to interpret and satisfy the quest with the means you have available. Writing at least a 100 word storybeat in how you approach it can add an additional reward point to the questo per 100 up to double. You can combine quests into \\"arcs\\" with combined RP and word count. You can spend RP mid \\"Arc\\", such as finding the shiny hat before facing the wolves or taking the bad guy's fancy stick.}
    """


sidebar : String
sidebar =
    """
    Indicates a quest with a built in evil / villain route. If the ? is missing, the quest is by nature villainous. However, you can use any plot hook to write / imagine an evil angle regardless, so long as it still merits the Reward tier
    """
