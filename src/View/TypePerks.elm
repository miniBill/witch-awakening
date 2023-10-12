module View.TypePerks exposing (viewTypePerks)

import Data.TypePerks as TypePerks
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
        , TypePerks.all
            |> List.map (typePerkBox typePerks)
            |> Theme.wrappedRow
                [ width fill
                , spacing <| Theme.rythm * 3
                ]
            |> Element.map (\( race, selected ) -> ChoiceTypePerk race selected)
        ]


typePerkBox :
    List Race
    -> TypePerks.Details
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
    Theme.card
        { glow = glow
        , imageAttrs = [ Theme.style "background-position" "top" ]
        , imageHeight = 360
        , image =
            if race == Jotun then
                Images.typePerkJotun

            else
                Types.raceToImage race
        , inFront =
            [ el
                [ Theme.captureIt
                , Font.size 56
                , centerX
                ]
                (gradientText 6 Gradients.yellowGradient <|
                    Types.raceToString race
                )
            , el
                [ alignRight
                , Theme.captureIt
                , moveLeft 28
                , moveDown 16
                , Font.size 30
                ]
                (gradientText 6 Gradients.yellowGradient <|
                    String.fromInt -cost
                )
            , el [ alignBottom ] <|
                Theme.image [ width <| px 40 ] <|
                    Types.slotToImage slot
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
