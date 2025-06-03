module View.Race exposing (viewRace)

import Data.Affinity as Affinity
import Data.Race as Race
import Element exposing (Attribute, Element, alignTop, centerX, el, fill, moveDown, moveRight, moveUp, rgb, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Generated.Races
import Generated.Types as Types exposing (Affinity(..), Race(..), Size)
import Gradients
import Images exposing (Image)
import List.Extra
import Theme exposing (gradientText, viewAffinity)
import Types exposing (Choice(..), Display)
import View
import View.Affinity as Affinity


viewRace : Display -> List Race -> Element Choice
viewRace display races =
    let
        raceBoxes : Element ( Race, Bool )
        raceBoxes =
            Generated.Races.all races
                |> List.sortBy (\{ dlc } -> Maybe.withDefault "" dlc)
                |> List.filterMap (raceBox display races)
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
    -> Maybe (Element ( Race, Bool ))
raceBox display selected { name, tank, affinities, charge, content, dlc } =
    let
        isSelected : Bool
        isSelected =
            List.member name selected

        shortName : String
        shortName =
            Types.raceToString name
                |> String.split "-"
                |> List.take 1
                |> String.concat
    in
    Theme.card []
        { display = display
        , forceShow = List.isEmpty selected
        , glow = 0x00F3EA6F
        , isSelected = isSelected
        , imageAttrs = []
        , imageHeight = ( 600, 400 )
        , image = Types.raceToImage name
        , inFront =
            [ el
                [ alignTop
                , Theme.captureIt
                , Font.size 56
                , centerX
                ]
                (gradientText 6 Gradients.yellowGradient shortName)
            , case dlc of
                Nothing ->
                    Element.none

                Just dlcName ->
                    el
                        [ centerX
                        , Theme.captureIt
                        , Font.size 24
                        , moveDown 60
                        ]
                        (Theme.gradientText 4 Gradients.purpleGradient dlcName)
            ]
        , content =
            Theme.row [ centerX ]
                [ viewTank tank
                , viewAffinities affinities
                , viewCharge charge
                ]
                :: Theme.blocks [] content
                :: affinityPicker name
        , onPress =
            case ( name, isSelected ) of
                ( Dravir _, False ) ->
                    Nothing

                ( Genie _, False ) ->
                    Nothing

                ( Gemini _, False ) ->
                    Nothing

                _ ->
                    Just ( name, not isSelected )
        }


affinityPicker : Race -> List (Element ( Race, Bool ))
affinityPicker race =
    let
        picker : (Affinity -> Race) -> Affinity -> List Affinity -> List (Element ( Race, Bool ))
        picker ctor currentAffinity list =
            [ el [ Font.bold ] <| text "Pick an affinity:"
            , list
                |> List.map
                    (\affinity ->
                        let
                            isSelected : Bool
                            isSelected =
                                affinity == currentAffinity

                            msg : ( Race, Bool )
                            msg =
                                ( ctor affinity, not isSelected )
                        in
                        Affinity.button isSelected msg affinity
                    )
                |> Theme.wrappedRow []
            ]
    in
    case race of
        Dravir currentAffinity ->
            picker Dravir currentAffinity [ Fire, Wind, Water, Earth, Metal, Nature ]

        Genie currentAffinity ->
            picker Genie currentAffinity Affinity.all

        Gemini currentAffinity ->
            picker Gemini currentAffinity (List.Extra.remove Earth Affinity.all)

        _ ->
            []


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
