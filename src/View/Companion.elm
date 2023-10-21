module View.Companion exposing (viewCompanions)

import Data.Companion as Companion
import Element exposing (Element, alignBottom, alignRight, centerX, el, fill, fillPortion, height, moveDown, moveLeft, moveRight, padding, px, rgb, rgba, shrink, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Generated.Types as Types exposing (Companion)
import Gradients
import Images
import Theme
import Types exposing (Choice(..))


viewCompanions : List Companion -> Element Choice
viewCompanions companions =
    Theme.column
        [ width fill
        , spacing <| Theme.rythm * 2
        ]
        [ Theme.column
            ([ width fill
             , spacing <| Theme.rythm * 2
             ]
                ++ Theme.topBackground Images.companionIntro
            )
            [ Theme.blocks [] "# Companions"
            , let
                color : Element.Color
                color =
                    rgba 0 0 0 0.75
              in
              Theme.wrappedRow
                [ width <| Element.maximum 800 fill
                , centerX
                , spacing <| 2 * Theme.rythm
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
            , el [ height <| px 200 ] Element.none
            ]
        , Companion.all
            |> List.concatMap (companionSection companions)
            |> Theme.column
                [ width fill
                , spacing <| Theme.rythm * 3
                ]
            |> Element.map (\( companion, selected ) -> ChoiceCompanion companion selected)
        ]


companionSection : List Companion -> ( String, List Companion.Details ) -> List (Element ( Companion, Bool ))
companionSection companions ( label, section ) =
    let
        title : Element msg
        title =
            Theme.gradientText 2 Gradients.yellowGradient (label ++ ":")
    in
    el [ Theme.celticHand, Font.size 40 ] title
        :: List.map (companionBox companions) section


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
    List Companion
    -> Companion.Details
    -> Element ( Companion, Bool )
companionBox selected ({ name, quote, cost, class, description } as companion) =
    let
        isSelected : Bool
        isSelected =
            List.member name selected

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
                        [ String.fromInt cost
                            |> Theme.gradientText 4 Gradients.yellowGradient
                            |> el
                                [ alignRight
                                , Font.size 32
                                , Theme.captureIt
                                , moveLeft 4
                                , moveDown 4
                                ]
                        , Types.gainToSlot cost
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
                    , Theme.classToBadge class
                        |> Theme.image [ width <| px 32, alignRight, moveLeft 24 ]
                    ]
                , statsTable companion
                , Theme.blocks [ Font.size 14 ] <| "_*" ++ quote ++ "*_"
                , Theme.blocks [] description
                ]
    in
    Theme.maybeButton
        [ height fill
        , width <| Element.minimum 480 <| Element.maximum 680 fill
        , Font.color <| rgb 0 0 0
        , Border.rounded Theme.cardRoundness
        , case glow of
            Just color ->
                Background.color <| Theme.intToBackground color

            Nothing ->
                Background.color <| rgb 1 1 1
        , case glow of
            Just color ->
                Border.glow (Theme.intToColor color) 8

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
    let
        cellWithLeftBorder attrs label leftBorder content =
            el
                ([ padding <| Theme.rythm // 2
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
    in
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
                    (\v ->
                        { header = Element.none
                        , width = fill
                        , view =
                            \( label, w, ( mainColor, otherColor ) ) ->
                                cellWithLeftBorder
                                    [ Theme.backgroundColor <|
                                        if w == v then
                                            mainColor

                                        else if w > v then
                                            otherColor

                                        else
                                            0x00FFFFFF
                                    , Font.center
                                    ]
                                    label
                                    0
                                    (if label == "Ranking" then
                                        text <| String.fromInt v

                                     else
                                        text ""
                                    )
                        }
                    )
                    (List.reverse <| List.range 1 10)
        , data =
            [ ( "Power", details.power )
            , ( "Teamwork", details.teamwork )
            , ( "Sociability", details.sociability )
            , ( "Morality", details.morality )
            , ( "Ranking", -1 )
            ]
                |> List.map
                    (\( label, value ) ->
                        ( label
                        , value
                        , if value == 10 then
                            Theme.colors.companionGold

                          else if value >= 8 then
                            Theme.colors.companionBlue

                          else if value >= 5 then
                            Theme.colors.companionOrange

                          else if value > 1 then
                            Theme.colors.companionRed

                          else
                            Theme.colors.companionBlack
                        )
                    )
        }
