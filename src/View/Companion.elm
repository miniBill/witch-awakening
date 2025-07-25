module View.Companion exposing (viewCompanions)

import Data.Companion as Companion exposing (MaybeClass(..))
import Element exposing (Attribute, Element, alignBottom, alignRight, alignTop, centerX, centerY, column, el, fill, fillPortion, height, inFront, moveDown, moveLeft, moveRight, padding, px, rgb, rgba, shrink, spacing, table, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Generated.Companion
import Generated.Types as Types exposing (Companion, Faction)
import Gradients
import Html.Attributes
import Images
import Svg
import Svg.Attributes
import Theme
import Types exposing (Choice(..), Display(..))
import View
import View.Race


viewCompanions : Display -> List Companion -> Element Choice
viewCompanions display companions =
    let
        blocks : List (Element ( Companion, Bool ))
        blocks =
            Generated.Companion.all
                |> List.concatMap (companionSection display companions)
    in
    View.collapsible (Theme.topBackground Images.companionIntro)
        display
        DisplayCompanions
        ChoiceCompanion
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
            color : Element.Color
            color =
                rgba 0 0 0 0.75
          in
          Theme.wrappedRow
            [ width <| Element.maximum 800 fill
            , centerX
            , spacing <| 2 * Theme.rhythm
            ]
            [ Theme.blocks
                [ width fill
                , Background.color color
                , Border.shadow
                    { offset = ( 0, 0 )
                    , size = 5
                    , blur = 5
                    , color = color
                    }
                ]
                Companion.intro
            , Element.table
                [ width shrink
                , Background.color color
                , Border.shadow
                    { offset = ( 0, 0 )
                    , size = 5
                    , blur = 5
                    , color = color
                    }
                , alignBottom
                ]
                { columns =
                    tableColumns
                , data =
                    tableData
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
        [ (Companion.factionToCollectiveName faction ++ ":")
            |> Theme.gradientText 2 Gradients.yellowGradient
            |> el [ Theme.celticHand, Font.size 48 ]
        , boxes
            |> Theme.wrappedRow
                [ width fill
                , spacing <| Theme.rhythm * 3
                ]
        ]

    else
        boxes


tableColumns : List { header : Element msg, width : Element.Length, view : ( ( Int, Int ), String ) -> Element msg }
tableColumns =
    [ { header = Element.none
      , width = px 30
      , view =
            \( ( color, _ ), _ ) ->
                el
                    [ Border.width 1
                    , Border.color <| rgb 0 0 0
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
                Theme.blocks [] <| "{choice _" ++ label ++ "_}"
      }
    ]


tableData : List ( ( Int, Int ), String )
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
companionBox display selected ({ name, races, hasPerk, quote, cost, class, description, positives, mixed, negatives, has, dlc } as companion) =
    let
        isSelected : Bool
        isSelected =
            List.member name selected
    in
    if display /= DisplayFull && not isSelected then
        Element.none

    else
        let
            glow : Maybe Int
            glow =
                if isSelected then
                    Just 0x00F3EA6F

                else
                    Nothing

            image : Element msg
            image =
                el
                    (width fill
                        :: height fill
                        :: Border.roundEach
                            { topLeft = Theme.cardRoundness
                            , bottomLeft = Theme.cardRoundness
                            , topRight = 0
                            , bottomRight = 0
                            }
                        :: Background.image
                            (Types.companionToImage name).src
                        :: List.map Element.inFront
                            [ cost
                                |> Maybe.map String.fromInt
                                |> Maybe.withDefault "X"
                                |> Theme.gradientText 4 Gradients.yellowGradient
                                |> el
                                    [ alignRight
                                    , Font.size 32
                                    , Theme.captureIt
                                    , moveLeft 8
                                    , moveDown 4
                                    ]
                            , let
                                joined : String
                                joined =
                                    races
                                        |> List.map View.Race.raceToShortString
                                        |> String.join " - "

                                normal : String
                                normal =
                                    if hasPerk then
                                        joined ++ "+"

                                    else
                                        joined
                              in
                              (case normal of
                                "Neutral" ->
                                    ""

                                "" ->
                                    "Any"

                                _ ->
                                    normal
                              )
                                |> Theme.gradientText 4 Gradients.yellowGradient
                                |> el
                                    [ alignBottom
                                    , centerX
                                    , Font.size 32
                                    , Theme.captureIt
                                    ]
                            , cost
                                |> Maybe.map Types.gainToSlot
                                |> Maybe.withDefault Types.White
                                |> Types.slotToImage
                                |> Theme.image [ width <| px 40 ]
                                |> el [ moveRight 4 ]
                            ]
                    )
                    Element.none

            content : Element msg
            content =
                Theme.column
                    [ Theme.padding
                    , height fill
                    , width <| fillPortion 2
                    ]
                    [ Theme.row [ width fill ]
                        [ Types.companionToString name
                            |> Theme.gradientText 4 Gradients.yellowGradient
                            |> el [ Font.size 36 ]
                        , case class of
                            ClassOne c ->
                                c
                                    |> Theme.classToBadge
                                    |> Theme.image [ width <| px 32, alignRight, moveLeft 24 ]

                            ClassAny ->
                                Theme.viewClasses 32 [ Types.Sorceress, Types.Warlock, Types.Academic ]
                                    |> el [ alignRight, moveLeft 24 ]

                            ClassNone ->
                                Element.none

                            ClassSpecial ->
                                Images.badgeSpecial
                                    |> Theme.image [ width <| px 32, alignRight, moveLeft 24 ]
                        ]
                    , case dlc of
                        Nothing ->
                            Element.none

                        Just dlcName ->
                            el
                                [ centerX
                                , Theme.captureIt
                                , Font.size 24
                                ]
                                (Theme.gradientText 4 Gradients.purpleGradient dlcName)
                    , statsTable companion
                    , Theme.blocks [ Font.size 14 ] quote
                    , Theme.blocks [] description
                    , let
                        toBlocks : List String -> List (Element msg)
                        toBlocks lines =
                            List.map
                                (\line ->
                                    if String.startsWith "-" line then
                                        Theme.blocks [] <| "\\" ++ line

                                    else
                                        Theme.blocks [] line
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
                                              , view = \( _, tail ) -> Theme.blocks [] tail
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
                      in
                      column
                        [ width fill
                        , spacing <| Theme.rhythm // 2
                        ]
                        (beforeBlock :: toBlocks mixed)
                    , if String.isEmpty has then
                        Element.none

                      else
                        Theme.blocks []
                            ("*" ++ has ++ ".*")
                    ]
        in
        Theme.button
            [ Theme.id (Types.companionToString name)
            , height fill
            , if display == DisplayFull then
                width <| Element.minimum 660 <| Element.maximum 760 fill

              else
                width <| Element.minimum 660 fill
            , Font.color <| rgb 0 0 0
            , Border.rounded Theme.cardRoundness
            , case glow of
                Just color ->
                    Background.color <| Theme.intToBackground color

                Nothing ->
                    Background.color <| rgb 1 1 1
            , case glow of
                Just color ->
                    Theme.borderGlow color

                Nothing ->
                    Border.width 0
            ]
            { label =
                Element.row
                    [ height fill
                    , width fill
                    ]
                    [ image
                    , content
                    ]
            , onPress = Just ( name, not isSelected )
            }


statsTable : Companion.Details -> Element msg
statsTable details =
    Element.table [ width fill ]
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


scoreToColor : Int -> ( Int, Int )
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
cellWithLeftBorder attrs label leftBorder content =
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
        content


statColumn : Int -> Element.Column ( String, Companion.Score, ( Int, Int ) ) msg
statColumn ranking =
    let
        view : ( String, Companion.Score, ( Int, Int ) ) -> Element msg
        view ( label, rawScore, ( mainColor, otherColor ) ) =
            let
                ( backgroundColor, attrs, content ) =
                    case rawScore of
                        Companion.SpecialEffect { better, worse } ->
                            ( case worse of
                                Nothing ->
                                    if ranking <= better then
                                        mainColor

                                    else
                                        0x00FFFFFF

                                Just w ->
                                    if ranking > better then
                                        0x00FFFFFF

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
                                        el
                                            [ centerX
                                            , centerY
                                            , Font.italic
                                            , Element.moveLeft 24
                                            , Theme.style "z-index" "1"
                                            ]
                                        <|
                                            Theme.gradientText 2 Gradients.blueGradient "Special Effect"
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
                                    0x00FFFFFF
                                , []
                                , if label == "Ranking" then
                                    text <| String.fromInt ranking

                                  else
                                    Element.none
                                )
            in
            cellWithLeftBorder
                ([ Border.color <| rgb 0 0 0
                 , Font.center
                 , Theme.backgroundColor backgroundColor
                 ]
                    ++ attrs
                )
                label
                0
                content
    in
    { header = Element.none
    , width = fill
    , view = view
    }


grayRow : Int -> ( Int, List (Attribute msg), Element msg )
grayRow ranking =
    let
        ( mainColor, otherColor ) =
            Theme.colors.companionBlack
    in
    if ranking == 1 then
        ( mainColor
        , []
        , el [ Font.color <| rgb 1 1 1 ] <| text "N/A"
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
