module View.TypePerk exposing (viewTypePerks)

import Color
import Element exposing (Element, alignBottom, alignRight, centerX, el, fill, moveDown, moveLeft, moveUp, paddingXY, px, rgb, spacing, width)
import Element.Font as Font
import Generated.Gradient as Gradient
import Generated.Image as Image exposing (Image)
import Generated.TypePerk as TypePerk
import Generated.Types as Types exposing (Race(..), Slot)
import Set exposing (Set)
import Theme
import Types exposing (Choice(..), Display, IdKind(..))
import View
import View.Race


viewTypePerks : Set String -> List Race -> Display -> List Race -> Element Choice
viewTypePerks hideDLC witchRaces display typePerks =
    let
        filtered : List TypePerk.Details
        filtered =
            TypePerk.all
                |> View.filterDLC hideDLC
    in
    if List.isEmpty filtered then
        Element.none

    else
        let
            boxes : List (Element ( Race, Bool ))
            boxes =
                filtered
                    |> List.filterMap (typePerkBox witchRaces display typePerks)
        in
        View.collapsible
            [ Theme.style "background-image" <| "url(\"" ++ Image.typePerkBackground.src ++ "\"), url(\"" ++ Image.typePerkBottomBackground.src ++ "\")"
            , Theme.style "background-repeat" "no-repeat, no-repeat"
            , Theme.style "background-position" "top, bottom"
            , Theme.style "background-size" "100%, 100%"
            ]
            display
            DisplayTypePerks
            ChoiceTypePerk
            IdKindTypePerk
            "# Type Perks"
            [ Theme.blocks
                [ Font.color <| rgb 0 0 0
                , Theme.backgroundColor Theme.colors.white
                , Theme.padding
                , centerX
                , width <| Element.maximum 600 fill
                , Theme.rounded
                ]
                IdKindTypePerk
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
    List Race
    -> Display
    -> List Race
    -> TypePerk.Details
    -> Maybe (Element ( Race, Bool ))
typePerkBox witchRaces display selected { name, race, cost, content, dlc } =
    let
        isSelected : Bool
        isSelected =
            List.member race selected

        slot : Slot
        slot =
            Types.gainToSlot cost

        raceString : String
        raceString =
            View.Race.raceToShortString race

        nameLength : Length
        nameLength =
            if stringWidth raceString < 9 then
                Short

            else if stringWidth raceString < 12 then
                Average

            else
                Long
    in
    Theme.card [ Theme.id IdKindTypePerk raceString ]
        { display = display
        , forceShow = List.any (Types.isSameRace race) witchRaces
        , glow = Color.rgb255 0xF3 0xEA 0x6F
        , isSelected = isSelected
        , imageAttrs = [ Theme.style "background-position" "top" ]
        , imageHeight = 360
        , image = raceToTypePerkImage race
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
                Gradient.yellowGradient
                raceString
            , Theme.gradientTextWrapped
                [ alignRight
                , Theme.captureIt
                , moveLeft 28
                , moveDown 16
                , Font.size 30
                ]
                6
                Gradient.yellowGradient
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
                        Gradient.purpleGradient
                        dlcName
            , case name of
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
                                            |> Theme.gradientText 4 Gradient.yellowGradient
                                            |> el [ centerX ]
                                    )

                         else
                            [ Theme.gradientText 4 Gradient.yellowGradient n ]
                        )
            , Types.slotToImage slot
                |> Theme.image [ width <| px 40 ]
                |> el [ alignBottom ]
            ]
        , content = [ Theme.blocks [] IdKindTypePerk content ]
        , onPress = Just ( race, not isSelected )
        }


raceToTypePerkImage : Race -> Image
raceToTypePerkImage race =
    case race of
        RaceJotun ->
            Image.typePerkJotun

        RaceXeno ->
            Image.typePerkXeno

        RaceSpider ->
            Image.typePerkSpider

        RacePixie ->
            Image.typePerkPixie

        RaceFairy ->
            Image.typePerkFairy

        RaceGenie _ ->
            Image.typePerkGenie

        RaceGemini _ ->
            Image.typePerkGemini

        RaceSwarm ->
            Image.typePerkSwarm

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
