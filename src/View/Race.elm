module View.Race exposing (viewRace)

import Data.Race as Race
import Element exposing (Attribute, Element, alignTop, centerX, el, fill, height, moveDown, moveRight, moveUp, rgb, spacing, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Generated.Types as Types exposing (Affinity, Race, Size)
import Gradients
import Images exposing (Image)
import Theme exposing (gradientText, viewAffinity)
import Types exposing (Choice(..), Display)
import View


viewRace : Display -> List Race -> Element Choice
viewRace display races =
    let
        raceBoxes : Element ( Race, Bool )
        raceBoxes =
            Race.all
                |> List.map (raceBox display races)
                |> Theme.wrappedRow
                    [ width fill
                    , spacing <| Theme.rythm * 3
                    ]
    in
    View.collapsible []
        display
        DisplayRace
        ChoiceRace
        Race.title
        [ Theme.blocks [] Race.intro
        , raceBoxes
        ]
        [ raceBoxes
        ]


raceBox :
    Display
    -> List Race
    -> Race.Details
    -> Element ( Race, Bool )
raceBox display selected { name, tank, affinities, charge, content } =
    let
        isSelected : Bool
        isSelected =
            List.member name selected
    in
    Theme.card []
        { display = display
        , glow = 0x00F3EA6F
        , isSelected = isSelected
        , imageAttrs = []
        , imageHeight = 600
        , image = Types.raceToImage name
        , inFront =
            [ el
                [ alignTop
                , Theme.captureIt
                , Font.size 56
                , centerX
                ]
                (gradientText 6 Gradients.yellowGradient <| Types.raceToString name)
            ]
        , content =
            [ Theme.column [ width fill, height fill ]
                [ Theme.row [ centerX ]
                    [ viewTank tank
                    , viewAffinities affinities
                    , viewCharge charge
                    ]
                , Theme.blocks
                    [ height fill
                    , Theme.padding
                    ]
                    content
                ]
            ]
        , onPress = Just ( name, not isSelected )
        }


viewAffinities : List Affinity -> Element ( Race, Bool )
viewAffinities affinities =
    Theme.row
        [ moveDown 2
        , Border.width 4
        , Border.color <| rgb 0 0 0
        , Border.rounded 999
        , Background.color <| rgb 0 0 0
        ]
        (List.map viewAffinity affinities)


viewTank : Size -> Element msg
viewTank size =
    viewSize []
        Images.tank
        (List.map
            (\( r, g, b ) -> ( r // 2, g * 3 // 5, b ))
            Gradients.blueGradient
        )
        size


viewCharge : Size -> Element msg
viewCharge size =
    viewSize [ moveRight 30 ]
        Images.charge
        Gradients.yellowGradient
        size


viewSize :
    List (Attribute msg)
    -> Image
    -> List ( Int, Int, Int )
    -> Size
    -> Element msg
viewSize attrs image gradient size =
    Types.sizeToString size
        |> Theme.gradientText 4 gradient
        |> el
            ([ Theme.morpheus
             , Font.size 20
             , Element.onLeft <| Theme.image [ moveUp 10 ] image
             ]
                ++ attrs
            )
