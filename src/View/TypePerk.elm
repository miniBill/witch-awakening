module View.TypePerk exposing (viewTypePerks)

import Data.TypePerk as TypePerk
import Element exposing (Element, alignBottom, alignRight, centerX, el, fill, moveDown, moveLeft, px, rgb, spacing, width)
import Element.Font as Font
import Generated.TypePerk
import Generated.Types as Types exposing (Race(..), Slot)
import Gradients
import Images
import Theme exposing (gradientText)
import Types exposing (Choice(..), Display)
import View


viewTypePerks : List Race -> Display -> List Race -> Element Choice
viewTypePerks witchRaces display typePerks =
    let
        boxes : List (Element ( Race, Bool ))
        boxes =
            Generated.TypePerk.all
                |> List.filterMap (typePerkBox witchRaces display typePerks)
    in
    View.collapsible
        [ Theme.style "background-image" <| "url(\"" ++ Images.typePerksBackground.src ++ "\"), url(\"" ++ Images.typePerkBottomBackground.src ++ "\")"
        , Theme.style "background-repeat" "no-repeat, no-repeat"
        , Theme.style "background-position" "top, bottom"
        , Theme.style "background-size" "100%, 100%"
        ]
        display
        DisplayTypePerks
        ChoiceTypePerk
        TypePerk.title
        [ Theme.blocks
            [ Font.color <| rgb 0 0 0
            , Theme.backgroundColor Theme.colors.white
            , Theme.padding
            , centerX
            , width <| Element.maximum 600 fill
            , Theme.rounded
            ]
            "These are particular perks that can be optionally taken by a witch of a given racial type. If hybridized (via later perk), you can purchase type perks of both types."
        , boxes
            |> Theme.wrappedRow
                [ width fill
                , spacing <| Theme.rythm * 3
                ]
        ]
        [ boxes
            |> Theme.wrappedRow
                [ width fill
                , spacing <| Theme.rythm * 3
                ]
        ]


typePerkBox :
    List Race
    -> Display
    -> List Race
    -> TypePerk.Details
    -> Maybe (Element ( Race, Bool ))
typePerkBox witchRaces display selected { race, cost, content, dlc } =
    let
        isSelected : Bool
        isSelected =
            List.member race selected

        slot : Slot
        slot =
            Types.gainToSlot cost
    in
    Theme.card [ Theme.id ("perk-" ++ Types.raceToString race) ]
        { display = display
        , forceShow = List.member race witchRaces
        , glow = 0x00F3EA6F
        , isSelected = isSelected
        , imageAttrs = [ Theme.style "background-position" "top" ]
        , imageHeight = 360
        , image =
            case race of
                Jotun ->
                    Images.typePerkJotun

                Xeno ->
                    Images.typePerkXeno

                Spider ->
                    Images.typePerkSpider

                Pixie ->
                    Images.typePerkPixie

                Fairy ->
                    Images.typePerkFairy

                Genie _ ->
                    Images.typePerkGenie

                Gemini _ ->
                    Images.typePerkGemini

                _ ->
                    Types.raceToImage race
        , inFront =
            [ Types.raceToString race
                |> String.split "-"
                |> List.take 1
                |> String.concat
                |> gradientText 6 Gradients.yellowGradient
                |> el
                    [ Theme.captureIt
                    , Font.size 56
                    , centerX
                    ]
            , String.fromInt -cost
                |> gradientText 6 Gradients.yellowGradient
                |> el
                    [ alignRight
                    , Theme.captureIt
                    , moveLeft 28
                    , moveDown 16
                    , Font.size 30
                    ]
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
            , Types.slotToImage slot
                |> Theme.image [ width <| px 40 ]
                |> el [ alignBottom ]
            ]
        , content = [ Theme.blocks [] content ]
        , onPress = Just ( race, not isSelected )
        }
