module View.Race exposing (viewRace)

import Element exposing (Element, alignTop, centerX, el, fill, height, inFront, moveUp, px, rgb, spacing, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Gradients
import Images exposing (Image)
import String.Multiline
import Theme exposing (gradientText)
import Types exposing (Affinity(..), Choice(..), Race(..), Size(..))


viewRace : Maybe Race -> Element Choice
viewRace race =
    Theme.column [ width fill ]
        [ Theme.blocks [] <| String.Multiline.here """
            # True Form - Race

            "This is my favorite part, so don't zone out on me: Even if most people prefer to be a neutral it's fun to see what other option or two a person might have. Let's see... oh, oh. ooh" The witch shudders with quite the enthusiasm, and continues "_This is ten times_ the most options I've seen in another witch. _*You have so many possibilities*_! Let's explore them... I haven't even seen some of these before, in person _or_ in an awakening ritual. I'm a fan of #7 in particular, they're so cute." This is the physical expression of your True Form, what you would revert to if any effect were to reveal the true nature of a shapechanger for example. Your old body was just a temporary vessel for this true form. In your change, {choice you can totally remake your body within human limits}. However, witches are female by default, but you can take the perk _Elephant Trunk_ to be male if you wish, or the _Transformation Sequence_ perk to transform into your past human vessel and back. Note with the options presented you can become another non-human species as a Witch of that species, ie; A witch can be a naiad but not all naids are witches. Every different race of witch has a different source of Mana as well as some unique benefits and base abilities. All witches except Neutral and partially Daeva share some degree of _inherent_ life extension, though there are many other methods any witch can learn or obtain later on regardless of their race. {choice *The blue potion icon represents Mana, the yellow bolt icon represents Charge rate. _This uses a Relative system and not absolute value, don't overthink it_*}, it's a representation of what you can expect in comparing your witch race to an average of other witches _of your same general power level_. High mana represents high storage, & charge represents the time it takes your method to fill your mana stores though it can often scale with overexposure, and stack.

            This is very simply just to give you a suggestion for perspective on how much magic you could expect to use. High Mana means a bigger tank, but charge is how fast it is filled. So high tank might be good for going all in while a low tank but high charge might have the endurance to go for longer. This is roughly balanced by how easy the charge source is to come by and how well it can stack up on itself.
            
            """
        , Theme.wrappedRow
            [ width fill
            , spacing <| Theme.rythm * 3
            ]
            [ neutral race ]
        ]


neutral : Maybe Race -> Element Choice
neutral race =
    raceBox race
        { race = Neutral
        , image = Images.neutral
        , tank = Medium
        , affinities = [ Soul, Body ]
        , charge = Medium
        , content = """
            The overwhelming majority of witches are Neutrals. There is nothing visually abnormal about them (by default). Even some witches who have the option to awaken as another race may want to choose to be Neutral for its baseline casual existence, no new dependencies or anything abnormal to consider. They're effectively the "Human" witch, but witches refer to them as neutral rather than human to distinguish them from the mundane. They age slower than humans, at half the rate, and do so very gracefully, and they tend to be more attractive than the average human, and are harder to kill, with more efficient biology to reduce inconveniences with less biological requirements than normal by 25%, and prevent dying from a papercut, or from a tiny air bubble in the wrong place.

            Neutrals draw Mana from Sleep, recovering around 1% of their Mana per hour of sleep, doubling per hour. 2 hours is more impactful than two 1 hour naps for example, regaining 16% for their 5th hour for a total of 31%, then 63% total for 6hrs.
    """
        }


raceBox :
    Maybe Race
    ->
        { race : Race
        , image : Image
        , tank : Size
        , affinities : List Affinity
        , charge : Size
        , content : String
        }
    -> Element Choice
raceBox selected { race, image, content } =
    let
        isSelected : Bool
        isSelected =
            case selected of
                Nothing ->
                    False

                Just selectedRace ->
                    selectedRace == race

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
            Border.glow (rgb 1 1 1) 8

          else
            Border.width 0
        ]
        { label =
            Element.column [ height fill ]
                [ el
                    [ width fill
                    , height <| px 400
                    , Border.rounded roundness
                    , inFront <|
                        el
                            [ alignTop
                            , Theme.morpheus
                            , Font.size 56
                            , centerX
                            , moveUp 8
                            ]
                            (gradientText 4 Gradients.yellowGradient <|
                                Types.raceToString race
                            )
                    , Background.image image.src
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
                Race <|
                    if isSelected then
                        Nothing

                    else
                        Just race
        }
