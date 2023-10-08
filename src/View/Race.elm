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
import Types exposing (Choice(..), Race(..))


viewRace : Maybe Race -> Element Choice
viewRace race =
    Theme.column [ width fill ]
        [ Theme.blocks [] <| String.Multiline.here """
            # True Form - Race

            
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
        , content = """
    
    """
        }


raceBox :
    Maybe Race
    ->
        { race : Race
        , image : Image
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
