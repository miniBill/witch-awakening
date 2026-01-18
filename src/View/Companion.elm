module View.Companion exposing (viewCompanions)

import Color exposing (Color)
import Data.Companion as Companion exposing (MaybeClass(..))
import Element exposing (Attribute, Element, alignBottom, alignRight, alignTop, centerX, centerY, column, el, fill, fillPortion, height, inFront, moveDown, moveLeft, moveRight, padding, paddingEach, px, shrink, spacing, table, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Generated.Companion as Companion
import Generated.Faction as Faction
import Generated.Gradient as Gradient
import Generated.Image as Image
import Generated.Types as Types exposing (Companion, Faction)
import Html.Attributes
import Set exposing (Set)
import Svg
import Svg.Attributes
import Theme exposing (Font(..))
import Types exposing (Choice(..), Display(..), IdKind(..))
import View
import View.Race


viewCompanions : Set String -> Display -> List Companion -> Element Choice
viewCompanions hideDLC display companions =
    let
        filtered : List ( Maybe Faction, List Companion.Details )
        filtered =
            Companion.all
                |> List.filterMap
                    (\( faction, factionCompanions ) ->
                        let
                            filteredCompanions : List Companion.Details
                            filteredCompanions =
                                View.filterDLC hideDLC factionCompanions
                        in
                        if List.isEmpty filteredCompanions then
                            Nothing

                        else
                            Just ( faction, filteredCompanions )
                    )
    in
    if List.isEmpty filtered then
        Element.none

    else
        let
            blocks : List (Element ( Companion, Bool ))
            blocks =
                filtered
                    |> List.concatMap (companionSection display companions)
        in
        View.collapsible (Theme.topBackground Image.companionIntro)
            display
            DisplayCompanions
            ChoiceCompanion
            IdKindCompanion
            "# Companions"
            [ introBlock
            , blocks
                |> Theme.column
                    [ width fill
                    , spacing <| Theme.rhythm * 3
                    ]
            ]
            [ blocks
                |> Theme.wrappedRow
                    [ width fill
                    , spacing <| Theme.rhythm * 3
                    ]
            ]


introBlock : Element msg
introBlock =
    Theme.column
        [ width fill
        , spacing <| Theme.rhythm * 2
        ]
        [ let
            color : Color
            color =
                Color.rgba 0 0 0 0.75
          in
          Theme.wrappedRow
            [ width <| Element.maximum 800 fill
            , centerX
            , spacing <| 2 * Theme.rhythm
            ]
            [ Theme.blocks
                [ width fill
                , Theme.backgroundColor color
                , Border.shadow
                    { offset = ( 0, 0 )
                    , size = 5
                    , blur = 5
                    , color = Theme.colorToElmUi color
                    }
                ]
                IdKindCompanion
                intro
            , Element.table
                [ width shrink
                , Theme.backgroundColor color
                , Border.shadow
                    { offset = ( 0, 0 )
                    , size = 5
                    , blur = 5
                    , color = Theme.colorToElmUi color
                    }
                , alignBottom
                ]
                { columns = tableColumns
                , data = tableData
                }
            ]
        , el [ height <| px 40 ] Element.none
        ]


companionSection : Display -> List Companion -> ( Maybe Faction, List Companion.Details ) -> List (Element ( Companion, Bool ))
companionSection display companions ( faction, section ) =
    let
        boxes : List (Element ( Companion, Bool ))
        boxes =
            section
                |> List.map (companionBox display companions)
    in
    if display == DisplayFull then
        [ Theme.gradientTextWrapped CelticHand
            [ Font.size 48
            , width fill
            ]
            2
            Gradient.yellowGradient
            (Faction.toCollectiveName faction ++ ":")
        , boxes
            |> Theme.wrappedRow
                [ width fill
                , spacing <| Theme.rhythm * 3
                , Theme.centerWrap
                ]
        ]

    else
        boxes


tableColumns : List { header : Element msg, width : Element.Length, view : ( ( Color, Color ), String ) -> Element msg }
tableColumns =
    [ { header = Element.none
      , width = px 30
      , view =
            \( ( color, _ ), _ ) ->
                el
                    [ Border.width 1
                    , Theme.borderColor Color.black
                    , Theme.backgroundColor color
                    , width fill
                    , height fill
                    ]
                    Element.none
      }
    , { header = Element.none
      , width = shrink
      , view =
            \( _, label ) ->
                Theme.blocks [] IdKindCompanion <| "{choice _" ++ label ++ "_}"
      }
    ]


tableData : List ( ( Color, Color ), String )
tableData =
    [ ( Theme.colors.companionBlack, "Black is Impaired" )
    , ( Theme.colors.companionRed, "Red is Lacking" )
    , ( Theme.colors.companionOrange, "Orange is Average" )
    , ( Theme.colors.companionBlue, "Blue is Exceptional" )
    , ( Theme.colors.companionGold, "Gold is Superb" )
    ]


companionBox :
    Display
    -> List Companion
    -> Companion.Details
    -> Element ( Companion, Bool )
companionBox display selected ({ name } as companion) =
    let
        isSelected : Bool
        isSelected =
            List.member name selected
    in
    if display /= DisplayFull && not isSelected then
        Element.none

    else
        let
            glow : Maybe Color
            glow =
                if isSelected then
                    Just (Color.rgb255 0xF3 0xEA 0x6F)

                else
                    Nothing
        in
        Theme.button
            [ Theme.id IdKindCompanion (Companion.toString name)
            , height fill
            , if display == DisplayFull then
                width <| Element.maximum 760 fill

              else
                width fill
            , Theme.fontColor Color.black
            , Border.rounded Theme.cardRoundness
            , case glow of
                Just color ->
                    Theme.backgroundColor <| Theme.colorToBackground color

                Nothing ->
                    Theme.backgroundColor Color.white
            , case glow of
                Just color ->
                    Theme.borderGlow color

                Nothing ->
                    Border.width 0
            , padding 0
            ]
            { label =
                Element.wrappedRow
                    [ height fill
                    , width fill
                    , Element.htmlAttribute
                        (Html.Attributes.classList
                            [ ( "min-660-if-wide", True )
                            , ( "do-not-overflow", True )
                            ]
                        )
                    ]
                    [ image companion
                    , content companion
                    ]
            , onPress = Just ( name, not isSelected )
            }


image : Companion.Details -> Element msg
image { name, races, hasPerk, cost } =
    let
        raceNames : List String
        raceNames =
            List.map
                (\r ->
                    if hasPerk then
                        View.Race.raceToShortString r ++ "+"

                    else
                        View.Race.raceToShortString r
                )
                races

        raceLabel : List String
        raceLabel =
            if
                (raceNames
                    |> List.map String.length
                    |> List.sum
                )
                    > 12
            then
                raceNames

            else
                case String.join " - " raceNames of
                    "Neutral" ->
                        []

                    "" ->
                        [ "Any" ]

                    joined ->
                        [ joined ]

        inFront : List (Element msg)
        inFront =
            [ cost
                |> Maybe.map String.fromInt
                |> Maybe.withDefault "X"
                |> Theme.gradientText CaptureIt
                    [ alignRight
                    , Font.size 32
                    , moveLeft 8
                    , moveDown 4
                    ]
                    4
                    Gradient.blueGradient
            , raceLabel
                |> List.map
                    (\race ->
                        el [ centerX ]
                            (Theme.gradientText CaptureIt
                                [ Font.size 32 ]
                                4
                                Gradient.yellowGradient
                                race
                            )
                    )
                |> Theme.column
                    [ alignBottom
                    , centerX
                    ]
            , cost
                |> Maybe.map Types.gainToSlot
                |> Maybe.withDefault Types.SlotWhite
                |> Types.slotToImage
                |> Theme.image [ width <| px 40 ]
                |> el [ moveRight 4 ]
            ]
    in
    el
        (width (Element.minimum 200 fill)
            :: height (Element.minimum 200 fill)
            :: Border.roundEach
                { topLeft = Theme.cardRoundness
                , bottomLeft = Theme.cardRoundness
                , topRight = 0
                , bottomRight = 0
                }
            :: Background.image
                (Types.companionToImage name).src
            :: List.map Element.inFront inFront
        )
        Element.none


content : Companion.Details -> Element msg
content ({ name, quote, class, description, positives, mixed, negatives, has, dlc } as companion) =
    let
        toBlocks : List String -> List (Element msg)
        toBlocks lines =
            List.map
                (\line ->
                    if String.startsWith "-" line then
                        Theme.blocks [] IdKindCompanion ("\\" ++ line)

                    else
                        Theme.blocks [] IdKindCompanion line
                )
                lines

        toColumn : String -> List String -> Element msg
        toColumn label items =
            if List.isEmpty items then
                Element.none

            else
                column
                    [ width fill
                    , alignTop
                    , spacing <| Theme.rhythm // 2
                    ]
                    [ el [ Font.bold ] <| text <| label ++ ":"
                    , table [ width fill, spacing <| Theme.rhythm // 2 ]
                        { data =
                            List.map
                                (\line ->
                                    case String.split " " line of
                                        [] ->
                                            ( "", "" )

                                        head :: tail ->
                                            ( head, String.join " " tail )
                                )
                                items
                        , columns =
                            [ { header = Element.none
                              , width = shrink
                              , view = \( sign, _ ) -> text sign
                              }
                            , { header = Element.none
                              , width = fill
                              , view = \( _, tail ) -> Theme.blocks [] IdKindCompanion tail
                              }
                            ]
                        }
                    ]

        beforeBlock : Element msg
        beforeBlock =
            [ toColumn "Positives" positives
            , toColumn "Negatives" negatives
            ]
                |> Theme.row [ width fill ]

        classBadge : Element msg
        classBadge =
            case class of
                ClassOne c ->
                    Theme.viewClasses 32 [ c ]

                ClassAny ->
                    Theme.viewClasses 32 [ Types.ClassSorceress, Types.ClassWarlock, Types.ClassAcademic ]

                ClassNone ->
                    Element.none

                ClassSpecial ->
                    Theme.image
                        [ width <| px 32 ]
                        Image.badgeSpecial
    in
    Theme.column
        [ Theme.padding
        , height fill
        , width <| Element.minimum 200 <| fillPortion 2
        ]
        [ Theme.wrappedRow
            [ width fill, Theme.centerWrap ]
            [ classBadge
                |> el
                    [ paddingEach { left = 24, top = 0, right = 0, bottom = 0 }
                    , Element.htmlAttribute (Html.Attributes.style "visibility" "hidden")
                    , Element.htmlAttribute (Html.Attributes.style "min-width" "0px")
                    ]
            , Theme.gradientTextWrapped Theme.NoFont
                [ Font.size 36
                , width fill
                ]
                4
                Gradient.yellowGradient
                (Companion.toString name)
            , classBadge
                |> el
                    [ alignRight
                    , paddingEach { right = 24, top = 0, left = 0, bottom = 0 }
                    ]
            ]
        , case dlc of
            Nothing ->
                Element.none

            Just dlcName ->
                Theme.gradientTextWrapped CaptureIt
                    [ centerX
                    , Font.size 24
                    ]
                    4
                    Gradient.purpleGradient
                    dlcName
        , statsTable companion
        , Theme.blocks [ Font.size 14 ] IdKindCompanion quote
        , Theme.blocks [] IdKindCompanion description
        , column
            [ width fill
            , spacing <| Theme.rhythm // 2
            ]
            (beforeBlock :: toBlocks mixed)
        , if String.isEmpty has then
            Element.none

          else
            Theme.blocks []
                IdKindCompanion
                ("*" ++ has ++ ".*")
        ]


statsTable : Companion.Details -> Element msg
statsTable details =
    Element.table
        [ width fill
        , Element.htmlAttribute (Html.Attributes.class "smol")
        ]
        { columns =
            { header = Element.none
            , width = shrink
            , view =
                \( label, _, _ ) ->
                    cellWithLeftBorder [ Font.bold ]
                        label
                        1
                        (text <| label ++ ":")
            }
                :: List.map
                    statColumn
                    (List.reverse <| List.range 1 10)
        , data =
            [ ( "Power", details.power )
            , ( "Teamwork", details.teamwork )
            , ( "Sociability", details.sociability )
            , ( "Morality", details.morality )
            , ( "Ranking", Companion.NormalScore -1 )
            ]
                |> List.map
                    (\( label, value ) ->
                        ( label
                        , value
                        , case value of
                            Companion.SpecialEffect { better, worse } ->
                                scoreToColor (Maybe.withDefault better worse)

                            Companion.NormalScore p ->
                                scoreToColor p
                        )
                    )
        }


scoreToColor : Int -> ( Color, Color )
scoreToColor p =
    if p == 10 then
        Theme.colors.companionGold

    else if p >= 8 then
        Theme.colors.companionBlue

    else if p >= 5 then
        Theme.colors.companionOrange

    else if p > 1 then
        Theme.colors.companionRed

    else
        Theme.colors.companionBlack


cellWithLeftBorder : List (Attribute msg) -> String -> Int -> Element msg -> Element msg
cellWithLeftBorder attrs label leftBorder cellContent =
    el
        ([ padding <| Theme.rhythm // 2
         , width fill
         , height fill
         , Border.widthEach
            { top = 1
            , left = leftBorder
            , bottom =
                if label == "Ranking" then
                    1

                else
                    0
            , right = 1
            }
         ]
            ++ attrs
        )
        cellContent


statColumn : Int -> Element.Column ( String, Companion.Score, ( Color, Color ) ) msg
statColumn ranking =
    let
        view : ( String, Companion.Score, ( Color, Color ) ) -> Element msg
        view ( label, rawScore, ( mainColor, otherColor ) ) =
            let
                ( backgroundColor, attrs, cellContent ) =
                    case rawScore of
                        Companion.SpecialEffect { better, worse } ->
                            ( case worse of
                                Nothing ->
                                    if ranking <= better then
                                        mainColor

                                    else
                                        Color.white

                                Just w ->
                                    if ranking > better then
                                        Color.white

                                    else if ranking == better then
                                        Tuple.first Theme.colors.companionBlack

                                    else if ranking > w then
                                        Tuple.second Theme.colors.companionBlack

                                    else if ranking == w then
                                        mainColor

                                    else
                                        otherColor
                            , let
                                halfRanking : Int
                                halfRanking =
                                    (better + Maybe.withDefault 1 worse) // 2
                              in
                              if ranking == halfRanking then
                                [ padding 0
                                , inFront <|
                                    el [ width fill, height fill ] <|
                                        Theme.gradientText NoFont
                                            [ Font.italic
                                            , Theme.style "z-index" "1"
                                            , centerX
                                            , centerY
                                            , Element.moveLeft 24
                                            ]
                                            2
                                            Gradient.blueGradient
                                            "Special Effect"
                                ]

                              else
                                [ padding 0 ]
                            , if better == 10 then
                                cross

                              else
                                Element.none
                            )

                        Companion.NormalScore score ->
                            if score == 0 then
                                grayRow ranking

                            else
                                ( if score == ranking then
                                    mainColor

                                  else if score > ranking then
                                    otherColor

                                  else
                                    Color.white
                                , []
                                , if label == "Ranking" then
                                    text <| String.fromInt ranking

                                  else
                                    Element.none
                                )
            in
            cellWithLeftBorder
                ([ Theme.borderColor Color.black
                 , Font.center
                 , Theme.backgroundColor backgroundColor
                 ]
                    ++ attrs
                )
                label
                0
                cellContent
    in
    { header = Element.none
    , width = fill
    , view = view
    }


grayRow : Int -> ( Color, List (Attribute msg), Element msg )
grayRow ranking =
    let
        ( mainColor, otherColor ) =
            Theme.colors.companionBlack
    in
    if ranking == 1 then
        ( mainColor
        , []
        , el [ Theme.fontColor Color.white ] <| text "N/A"
        )

    else
        ( otherColor
        , [ padding 0 ]
        , cross
        )


cross : Element msg
cross =
    Element.html <|
        Svg.svg
            [ Svg.Attributes.viewBox "0 0 25 25"
            , Html.Attributes.style "width" "100%"
            , Html.Attributes.style "height" "25px"
            , Svg.Attributes.preserveAspectRatio "none meet"
            ]
            [ Svg.line
                [ Svg.Attributes.x1 "0"
                , Svg.Attributes.y1 "0"
                , Svg.Attributes.x2 "25"
                , Svg.Attributes.y2 "25"
                , Svg.Attributes.stroke "black"
                ]
                []
            , Svg.line
                [ Svg.Attributes.x1 "0"
                , Svg.Attributes.y1 "25"
                , Svg.Attributes.x2 "25"
                , Svg.Attributes.y2 "0"
                , Svg.Attributes.stroke "black"
                ]
                []
            ]


intro : String
intro =
    """
    It’s dangerous to go alone, take this! A Companions section. {choice You have one free companion from your chosen faction, and one free that shares the same witch type} as you (either Race or Class). Companions have 4 main statistics giving a _rough idea_ of their general impact in a group dynamic, except the {choice POWER stat, which primarily serves to give free floating Power points to spend to _customize_ your companion’s abilities} on top of the default abilities they come with. Beyond that, interactions are inferred in how you interpret their personality and how different stats play out in relation to it and others. {choice You can buy additional companions using Reward Points}, listed to the left of their name. A {choice +} indicates they have their Type Perk.

    {choice You can spend your own Power on behalf of a member if you so choose, although they receive 1/2 the Reward value of Quests, so you don’t _need to_.}

    **Really, don’t stress the details too much and feel free to fill in the blanks. I do what I can to provide a reasonable summary of an array of characters, but it’s up to you to interpret it as you will.**

    Take your pick:
    """
