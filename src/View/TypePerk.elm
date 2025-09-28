module View.TypePerk exposing (viewTypePerks)

import Color exposing (Color)
import Data.TypePerk as TypePerk
import Element exposing (Element, alignBottom, alignRight, centerX, centerY, el, fill, moveDown, moveLeft, moveUp, paddingXY, px, rgb, spacing, text, width)
import Element.Font as Font
import Generated.TypePerk
import Generated.Types as Types exposing (Class, Race(..), Slot)
import Gradients
import Images
import Set exposing (Set)
import Theme
import Types exposing (Choice(..), Display)
import View
import View.Race


viewTypePerks : Set String -> Display -> List Race -> List Class -> Maybe Class -> List Race -> Element Choice
viewTypePerks hideDLC display witchRaces classes mainClass typePerks =
    let
        filtered : List TypePerk.Details
        filtered =
            Generated.TypePerk.all
                |> View.filterDLC hideDLC
    in
    if List.isEmpty filtered then
        Element.none

    else
        let
            boxes : List (Element Choice)
            boxes =
                filtered
                    |> List.filterMap (typePerkBox classes mainClass witchRaces display typePerks)
        in
        View.collapsible
            [ Theme.style "background-image" <| "url(\"" ++ Images.typePerkBackground.src ++ "\"), url(\"" ++ Images.typePerkBottomBackground.src ++ "\")"
            , Theme.style "background-repeat" "no-repeat, no-repeat"
            , Theme.style "background-position" "top, bottom"
            , Theme.style "background-size" "100%, 100%"
            ]
            display
            DisplayTypePerks
            identity
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
                    , spacing <| Theme.rhythm * 3
                    , Theme.centerWrap
                    ]
            ]
            [ boxes
                |> Theme.wrappedRow
                    [ width fill
                    , spacing <| Theme.rhythm * 3
                    , Theme.centerWrap
                    ]
            ]


type Length
    = Short
    | Average
    | Long


typePerkBox :
    List Class
    -> Maybe Class
    -> List Race
    -> Display
    -> List Race
    -> TypePerk.Details
    -> Maybe (Element Choice)
typePerkBox classes mainClass witchRaces display selected ({ cost, content, dlc } as details) =
    let
        isSelected : Bool
        isSelected =
            List.member details.race selected

        slot : Slot
        slot =
            Types.gainToSlot cost

        raceString : String
        raceString =
            View.Race.raceToShortString details.race

        nameLength : Length
        nameLength =
            if stringWidth raceString < 9 then
                Short

            else if stringWidth raceString < 12 then
                Average

            else
                Long

        color : Color.Color
        color =
            Color.rgb255 0xF3 0xEA 0x6F
    in
    Theme.card [ Theme.id ("perk-" ++ raceString) ]
        { display = display
        , forceShow = List.member details.race witchRaces
        , glow = color
        , isSelected = isSelected
        , imageAttrs = [ Theme.style "background-position" "top" ]
        , imageHeight = 360
        , image = raceToTypePerkImage details.race
        , inFront =
            [ Theme.gradientTextWrapped
                [ Theme.captureIt
                , case nameLength of
                    Short ->
                        Font.size 56

                    Average ->
                        Font.size 46

                    Long ->
                        Font.size 40
                , case nameLength of
                    Short ->
                        moveLeft 0

                    Average ->
                        moveLeft 18

                    Long ->
                        moveLeft 30
                , case nameLength of
                    Short ->
                        moveDown 0

                    Average ->
                        moveDown 8

                    Long ->
                        moveDown 11
                , centerX
                , paddingXY 30 0
                ]
                6
                Gradients.yellowGradient
                raceString
            , Theme.gradientTextWrapped
                [ alignRight
                , Theme.captureIt
                , moveLeft 28
                , moveDown 16
                , Font.size 30
                ]
                6
                Gradients.yellowGradient
                (String.fromInt -cost)
            , case dlc of
                Nothing ->
                    Element.none

                Just dlcName ->
                    Theme.gradientTextWrapped
                        [ centerX
                        , Theme.captureIt
                        , Font.size 24
                        , moveDown 60
                        ]
                        4
                        Gradients.purpleGradient
                        dlcName
            , case details.name of
                Nothing ->
                    Element.none

                Just n ->
                    let
                        len : Float
                        len =
                            stringWidth n
                    in
                    Theme.column
                        [ centerX
                        , Theme.captureIt
                        , if len > 20 then
                            Font.size 20

                          else
                            Font.size 24
                        , if len > 20 then
                            moveUp 2

                          else
                            moveUp 8
                        , alignBottom
                        ]
                        (if len > 20 then
                            let
                                words : List String
                                words =
                                    String.words n

                                wordCount : Int
                                wordCount =
                                    List.length words

                                halfCount : Int
                                halfCount =
                                    (wordCount + 1) // 2
                            in
                            [ List.take halfCount words
                            , List.drop halfCount words
                            ]
                                |> List.map
                                    (\line ->
                                        line
                                            |> String.join " "
                                            |> Theme.gradientText 4 Gradients.yellowGradient
                                            |> el [ centerX ]
                                    )

                         else
                            [ Theme.gradientText 4 Gradients.yellowGradient n ]
                        )
            , Types.slotToImage slot
                |> Theme.image [ width <| px 40 ]
                |> el [ alignBottom ]
            ]
        , content =
            case details.race of
                RaceNeutral ->
                    [ Theme.blocks [] content
                    , el [ Font.bold ] <| text "Pick your main class:"
                    , classPicker color mainClass classes
                    ]

                _ ->
                    [ Theme.blocks [] content ]
        , onPress = Just (ChoiceTypePerk ( details.race, not isSelected ))
        }


classPicker : Color -> Maybe Class -> List Class -> Element Choice
classPicker color mainClass classes =
    let
        viewClass : Class -> Element Choice
        viewClass class =
            let
                ( attrs, newValue ) =
                    if Just class == mainClass then
                        ( [ Theme.backgroundColor color ], Nothing )

                    else
                        ( [], Just class )
            in
            Theme.button (width fill :: Font.center :: attrs)
                { label =
                    Theme.row [ centerX, centerY ]
                        [ ("[" ++ Types.classToString class ++ "]")
                            |> Theme.blocks []
                        , Types.classToString class
                            |> text
                            |> el [ centerY ]
                        ]
                , onPress = Just <| ChoiceClasses newValue
                }
    in
    if List.length classes > 1 then
        classes
            |> List.map viewClass
            |> Theme.wrappedRow [ width fill ]

    else
        Element.none


raceToTypePerkImage : Race -> Images.Image
raceToTypePerkImage race =
    case race of
        RaceJotun ->
            Images.typePerkJotun

        RaceXeno ->
            Images.typePerkXeno

        RaceSpider ->
            Images.typePerkSpider

        RacePixie ->
            Images.typePerkPixie

        RaceFairy ->
            Images.typePerkFairy

        RaceGenie _ ->
            Images.typePerkGenie

        RaceGemini _ ->
            Images.typePerkGemini

        _ ->
            Types.raceToImage race


stringWidth : String -> Float
stringWidth s =
    String.foldl
        (\c acc ->
            if c == 'I' || c == 'i' then
                acc + 0.5

            else
                acc + 1
        )
        0
        s
