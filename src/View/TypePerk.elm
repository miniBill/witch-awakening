module View.TypePerk exposing (viewTypePerks)

import Data.TypePerk as TypePerk
import Element exposing (Element, alignBottom, alignRight, centerX, el, fill, height, moveDown, moveLeft, px, rgb, spacing, width)
import Element.Border as Border
import Element.Font as Font
import Generated.Types as Types exposing (Race(..), Slot)
import Gradients
import Images
import Theme exposing (gradientText)
import Types exposing (Choice(..))


viewTypePerks : List Race -> Element Choice
viewTypePerks typePerks =
    Theme.column
        [ width fill
        , spacing <| Theme.rythm * 2
        , Element.paddingEach { top = Theme.rythm, left = 0, right = 0, bottom = 0 }
        , Theme.style "background-image" <| "url(\"" ++ Images.typePerksBackground.src ++ "\"), url(\"" ++ Images.typePerkBottomBackground.src ++ "\")"
        , Theme.style "background-repeat" "no-repeat, no-repeat"
        , Theme.style "background-position" "top, bottom"
        , Theme.style "background-size" "100%, 100%"
        ]
        [ Theme.blocks [] "# Type Perks"
        , Theme.blocks
            [ Font.color <| rgb 0 0 0
            , Theme.backgroundColor Theme.colors.white
            , Theme.padding
            , centerX
            , width <| Element.maximum 600 fill
            , Border.rounded Theme.rythm
            ]
            "These are particular perks that can be optionally taken by a witch of a given racial type. If hybridized (via later perk), you can purchase type perks of both types."
        , TypePerk.all
            |> List.map (typePerkBox typePerks)
            |> Theme.wrappedRow
                [ width fill
                , spacing <| Theme.rythm * 3
                ]
            |> Element.map (\( race, selected ) -> ChoiceTypePerk race selected)
        ]


typePerkBox :
    List Race
    -> TypePerk.Details
    -> Element ( Race, Bool )
typePerkBox selected { race, cost, content } =
    let
        isSelected : Bool
        isSelected =
            List.member race selected

        glow : Maybe Int
        glow =
            if isSelected then
                Just 0x00F3EA6F

            else
                Nothing

        slot : Slot
        slot =
            Types.gainToSlot cost
    in
    Theme.card_ []
        { glow = glow
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

                Genie ->
                    Images.typePerkGenie

                Gemini ->
                    Images.typePerkGemini

                _ ->
                    Types.raceToImage race
        , inFront =
            [ Types.raceToString race
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
            , Types.slotToImage slot
                |> Theme.image [ width <| px 40 ]
                |> el [ alignBottom ]
            ]
        , content =
            [ Theme.blocks
                [ height fill
                , Theme.padding
                ]
                content
            ]
        , onPress = Just ( race, not isSelected )
        }
