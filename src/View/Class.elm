module View.Class exposing (viewClass)

import Element exposing (Element, alignBottom, centerX, el, fill, height, inFront, moveUp, px, rgb, spacing, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Generated.Types as Types exposing (Class(..))
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

            [center]{choice _*Choose one.*_}
            """
        , Theme.wrappedRow
            [ width fill
            , spacing <| Theme.rythm * 3
            ]
            (List.map (classBox class)
                [ academic, sorceress, warlock ]
            )
        ]


type alias ClassDetails =
    { class : Class
    , content : String
    }


academic : ClassDetails
academic =
    { class = Academic
    , content = """
        Academics are studious and focus easily on tasks, while training and studying to further their magic. Their thorough approach to magic tends to be slower if you want to have a life outside of studies, but the most rewarding as they comprehend in more depth and their growth is in their own hands, advancing as slow or fast as the time and effort they put in. Academics gain 1 {academic _*Focus*_} for every day in which they averaged 4 hours of study, 2 for 8 hours. You can use focus to buy a Power point for 10 Focus. This cost increases by 10 Focus per purchase. (10, 20, 30, etc)

        __Start with [30] power__. Player academics eventually cap out at [100] _power_ before other factors like complications. Any option marked with the {academic *blue*} icon has a flat cost reduction of -2 Power to its price, which can reduce it to 0, or become a gain of 1 point, or complications give 2 more power. This applies only once per option. Once per Magic specialization, once per perk, etc, and always applies first.

        *Advantage*: General use magic and classics. Academics favor mixed bags like Potions, Runes, and Portals. Any magic marked with blue used by an academic produces twice the yield or is half as time consuming, mana draining, or tiresome to use. Two potions for the price of one, two runes for the price of one, ect. Any duration of a blue marked magic effect applied by you, to you or to another, lasts twice as long For example, double the duration of a temporary potion.

        An Academic can study to master any two schools of magic for free, but takes time to learn equal to if you were saving the power to buy it, but no Power is spent.
        """
    }


sorceress : ClassDetails
sorceress =
    { class = Sorceress
    , content = """
        Sorceresses are inherently imbued with magic as natural extensions of their will so they tend to be more in tune with their bodies and grow through tactile training. They're naturals but tend to have less of a tangible understanding of how and why magic works or interesting implications of magical theory. Fireballs go boom, ain't gotta explain sheit. Sorceresses gain 1 {sorceress _*Might*_} for every day in which they averaged 1 hour of straining practice, 2 for 4 hours. You can use Might to buy a Power point for 10 Might. This cost increases by 10 Might per purchase. (10, 20, 30, etc)

        __Start with [30] power__. Player sorceresses eventually cap out at [100] _power_ before other factors like complications. Any option marked with the {sorceress *red*} icon has a flat cost reduction of -2 Power to its price, which can reduce it to 0, or become a gain of 1 point, or complications give 2 more power. This applies only once per option. Once per Magic specialization, once per perk, etc, and always applies first.

        *Advantage*: Inner power and direct combat usage. Sorceresses favor direct magic like Elementalist magic. They are not limited by affinity when buying elementalist magics of affinities they do not have, and one that matches their affinities can be taken for free.

        Sorceresses have stronger and more unique auras that are like beacons to anyone who can detect them. This aura can color any elemental magic the sorceress uses, such as white flames, gold stone, black water, or prismatic wind. This includes Naturalism. If they choose to use colored elementalism, then that magic is 50% more damaging, with 50% larger areas of effect and range.
        """
    }


warlock : ClassDetails
warlock =
    { class = Warlock
    , content = """
        Warlocks are endowed with power from some third party. Their power can't be taken back afterward anymore than such an entity might be capable of stealing power from any other witch. Instead of studying, or training, they spend time in service, partnership, employ, or worship to a patron. They grow by gaining with their patron(s), by doing quests, the Warlock gains {warlock _*Favor*_} equal to the Reward value of the quest, Warlocks can trade Favor 1-1 directly for Power due to the scarcity, being dependent on Quests. A Warlock can continue to do quests without a quest slot, but doing so offers no rewards except Favor.

        __Start with [30] power__. Player warlocks eventually cap out at [100] _power_ before other factors like complications. Any option marked with the icon has a flat cost reduction of -2 Power to its price, which can reduce it to 0, or become a gain of 1 point, or complications give 2 more power. This applies only once per option. Once per Magic specialization, once per perk, etc, and always applies first.

        *Advantage*: Darker and external magics, Relic usage. Warlocks favor indirect power like Hexes and Curses. They have a personalized brand they can mark on any relic they own or willing creature. They always know the location of one of their marks and when anyone else touches it, and a stronger sensation like an alarm if any harm comes to it. Branded creatures can be affected by the warlock's magic at any distance and the mark can be the target of things such as scrying even if the warlock doesn't know where it is.

        Warlocks can immediately start with 20 Reward Points to purchase relics that are infused in their own soul, summoned the same way as Mothergifts. (See Witchery)
        """
    }


classBox :
    Maybe Class
    -> ClassDetails
    -> Element Choice
classBox selected { class, content } =
    let
        isSelected : Bool
        isSelected =
            case selected of
                Nothing ->
                    False

                Just selectedClass ->
                    selectedClass == class

        roundness : Int
        roundness =
            72
    in
    Input.button
        [ height fill
        , width fill
        , Font.color <| rgb 0 0 0
        , Background.color <| rgb 1 1 1
        , Border.roundEach
            { topLeft = roundness
            , topRight = roundness
            , bottomLeft = 8
            , bottomRight = 8
            }
        , if isSelected then
            Border.glow (Theme.intToColor <| Theme.classToColor class) 8

          else
            Border.width 0
        ]
        { label =
            Element.column [ height fill ]
                [ el
                    [ Border.width 8
                    , Theme.borderColor <| Theme.classToColor class
                    , width fill
                    , height <| px 400
                    , Border.rounded roundness
                    , inFront <|
                        el
                            [ alignBottom
                            , Theme.morpheus
                            , Font.size 56
                            , centerX
                            , moveUp 8
                            ]
                            (gradientText 4 Gradients.yellowGradient <|
                                Types.classToString class
                            )
                    , Background.image (Types.classToImage class).src
                    ]
                    Element.none
                , Theme.blocks
                    [ height fill
                    , Theme.padding
                    ]
                    content
                ]
        , onPress =
            Just <|
                Class <|
                    if isSelected then
                        Nothing

                    else
                        Just class
        }
