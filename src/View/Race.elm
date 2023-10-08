module View.Race exposing (viewRace)

import Element exposing (Element, alignTop, centerX, el, fill, height, inFront, moveDown, moveRight, moveUp, px, rgb, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Gradients
import Images exposing (Image)
import String.Multiline
import Theme exposing (gradientText, viewAffinity)
import Types exposing (Affinity(..), Choice(..), Race(..), Size(..))


viewRace : Maybe Race -> Element Choice
viewRace race =
    Theme.column
        [ width fill
        , spacing <| Theme.rythm * 2
        ]
        [ Theme.blocks [] <| String.Multiline.here """
            # True Form - Race

            "This is my favorite part, so don't zone out on me: Even if most people prefer to be a neutral it's fun to see what other option or two a person might have. Let's see... oh, oh. ooh" The witch shudders with quite the enthusiasm, and continues "_This is ten times_ the most options I've seen in another witch. _*You have so many possibilities*_! Let's explore them... I haven't even seen some of these before, in person _or_ in an awakening ritual. I'm a fan of #7 in particular, they're so cute." This is the physical expression of your True Form, what you would revert to if any effect were to reveal the true nature of a shapechanger for example. Your old body was just a temporary vessel for this true form. In your change, {choice you can totally remake your body within human limits}. However, witches are female by default, but you can take the perk _Elephant Trunk_ to be male if you wish, or the _Transformation Sequence_ perk to transform into your past human vessel and back. Note with the options presented you can become another non-human species as a Witch of that species, ie; A witch can be a naiad but not all naids are witches. Every different race of witch has a different source of Mana as well as some unique benefits and base abilities. All witches except Neutral and partially Daeva share some degree of _inherent_ life extension, though there are many other methods any witch can learn or obtain later on regardless of their race. {choice *The blue potion icon represents Mana, the yellow bolt icon represents Charge rate. _This uses a Relative system and not absolute value, don't overthink it_*}, it's a representation of what you can expect in comparing your witch race to an average of other witches _of your same general power level_. High mana represents high storage, & charge represents the time it takes your method to fill your mana stores though it can often scale with overexposure, and stack.

            This is very simply just to give you a suggestion for perspective on how much magic you could expect to use. High Mana means a bigger tank, but charge is how fast it is filled. So high tank might be good for going all in while a low tank but high charge might have the endurance to go for longer. This is roughly balanced by how easy the charge source is to come by and how well it can stack up on itself.
            
            """
        , Theme.wrappedRow
            [ width fill
            , spacing <| Theme.rythm * 3
            ]
            (List.map (raceBox race)
                [ neutral
                , daeva
                , ifrit
                , siren
                ]
            )
        ]


neutral : RaceDetails
neutral =
    { race = Neutral
    , image = Images.neutral
    , tank = Medium
    , affinities = [ Soul, Body ]
    , charge = Medium
    , content = """
        The overwhelming majority of witches are Neutrals. There is nothing visually abnormal about them (by default). Even some witches who have the option to awaken as another race may want to choose to be Neutral for its baseline casual existence, no new dependencies or anything abnormal to consider. They're effectively the “Human” witch, but witches refer to them as neutral rather than human to distinguish them from the mundane. They age slower than humans, at half the rate, and do so very gracefully, and they tend to be more attractive than the average human, and are harder to kill, with more efficient biology to reduce inconveniences with less biological requirements than normal by 25%, and prevent dying from a papercut, or from a tiny air bubble in the wrong place.

        __Neutrals draw Mana__ from Sleep, recovering around 1% of their Mana per hour of sleep, doubling per hour. 2 hours is more impactful than two 1 hour naps for example, regaining 16% for their 5th hour for a total of 31%, then 63% total for 6hrs.
        """
    }


daeva : RaceDetails
daeva =
    { race = Daeva
    , image = Images.daeva
    , tank = Medium
    , affinities = [ Body, Life ]
    , charge = High
    , content = """
        Daeva are to humans, what huntans are to monkeys. They're peak evolution and represent the perfection of body and overflowing spark of life. Unlike the other witch types, they're unnatural for the excess humanity, not the addition of anything new. Where the others may have scales or feathers, you take normal human traits and take them further, with a body like a comic book heroine, a goddess in the flesh, free of any imperfections and basic inconveniences of mortality. They flatly don't age, forever maintaining a mature but youthful physique. The average daeva height sits around 6ft 6inches up to 8, but short daeva can happen. They're physically around twice as strong as a human male bodybuilder, without an obvious change in their body tone, and seem to have endless stamina. They feel emotions more strongly, while being in better control of them.

        __Daeva draw Mana__ from Emotions, either the presence of high emotions in others, or singular high emotions directed at them. ie; An excited crowd vs a lover are both high.
        """
    }


ifrit : RaceDetails
ifrit =
    { race = Ifrit
    , image = Images.ifrit
    , tank = High
    , affinities = [ Fire, Necro ]
    , charge = Low
    , content = """
        Ifriti are beings native to the elemental plane of fire. Their connection manifests in the most obvious manner of the hitches, as their bodies are burned away in their awakening, their body becoming a living conflagration of flame made flesh anchored and governed by a core skull. They bleed plasma and smoke while limbs severed from their core flicker away like dying flames. The fire of their bodies is slightly above room temp and does not burn, and is tangible enough to touch, feeling like normal flesh, though their hair is more gaseous and warmer. Their body is only a little transparent, enough to see the skull but not all the way through them. They inherently do not age, but can “eat” flammable fuel sources along with traditionally edible materials. Damage to the body is superficial and mended by absorbing flames, though damage to the skull must be healed by traditional means.

        __Ifriti draw Mana__ from Burning, the release of energy released by matter through buming caused by the witch.
        """
    }


siren : RaceDetails
siren =
    { race = Siren
    , image = Images.siren
    , tank = High
    , affinities = [ Wind, Beast ]
    , charge = High
    , content = """
        Sirens are beings of wind and music, melodies carried by the wind, and have a connection to birds. Their bodies grow leathers in a similar pattern as Nymphs grow scales, though some go further with leather plumage sufficient to be relatively modest without clothing, and are able to transfigure themselves into the form of a specific bird they're associated with and hybrid states between the two. Whatever the bird type, Sirens are all well known for melodic voices that are deeply pleasant to outright hypnotically attractive. Sirens age at half human rates, and reverse their age through nesting in cocoons of feathers that reverses a year per day. A dead siren who is still intact enough can be nursed back to life by breathing into them a few times a day for 1-3 days depending on severity.

        __Sirens draw Mana__ from Breath, whenever they take someone's breath away, when someone forgets to take a breath, when otherwise unable to breathe, or when the Siren overtly draws the breath from their lungs, within a few inches of their lips.
        """
    }


type alias RaceDetails =
    { race : Race
    , image : Image
    , tank : Size
    , affinities : List Affinity
    , charge : Size
    , content : String
    }


raceBox :
    Maybe Race
    -> RaceDetails
    -> Element Choice
raceBox selected { race, image, tank, affinities, charge, content } =
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
            Border.glow (rgb255 243 234 111) 8

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
                            , Theme.captureIt
                            , Font.size 56
                            , centerX
                            ]
                            (gradientText 6 Gradients.yellowGradient <|
                                Types.raceToString race
                            )
                    , Background.image image.src
                    ]
                    Element.none
                , Theme.row [ centerX ]
                    [ viewTank tank
                    , row [ moveDown 4 ]
                        (List.intersperse
                            (text " - ")
                            (List.map viewAffinity affinities)
                        )
                    , viewCharge charge
                    ]
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


viewTank : Size -> Element Choice
viewTank size =
    el
        [ Theme.morpheus
        , Font.size 30
        , Element.onLeft <| Theme.image [ moveUp 10 ] Images.tank
        ]
    <|
        Theme.gradientText 4 Gradients.blueGradient <|
            Types.sizeToString size


viewCharge : Size -> Element Choice
viewCharge size =
    el
        [ Theme.morpheus
        , Font.size 30
        , Element.onLeft <| Theme.image [ moveUp 10 ] Images.charge
        , moveRight 30
        ]
    <|
        Theme.gradientText 4 Gradients.yellowGradient <|
            Types.sizeToString size
