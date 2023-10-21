module View.Companion exposing (viewCompanions)

import Data.Companion as Companion
import Element exposing (Element, alignBottom, alignRight, centerX, el, fill, height, moveDown, moveLeft, moveUp, paragraph, px, rgb, rgba, shrink, spacing, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Generated.Types as Types exposing (Companion, Slot)
import Gradients
import Images
import String.Extra
import Theme exposing (gradientText)
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
            |> List.map (companionBox companions)
            |> Theme.wrappedRow
                [ centerX
                , spacing <| Theme.rythm * 3
                ]
            |> Element.map (\( companion, selected ) -> ChoiceCompanion companion selected)
        ]


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
companionBox selected { name, cost, class, description } =
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

        msg : Maybe ( Companion, Bool )
        msg =
            Just ( name, not isSelected )

        costGradient : Element msg
        costGradient =
            gradientText 4 Gradients.yellowGradient <|
                String.fromInt cost

        viewSlot : Slot -> Element msg
        viewSlot slot =
            Types.slotToImage slot
                |> Theme.image [ width <| px 40 ]
                |> el [ alignBottom, moveUp 48 ]
    in
    Theme.card []
        { glow = glow
        , imageAttrs = []
        , imageHeight = 400
        , image = Types.companionToImage name
        , inFront =
            [ el
                [ alignRight
                , Font.size 32
                , Theme.captureIt
                , moveLeft 4
                , moveDown 4
                ]
                costGradient
            , Theme.classToBadge class
                |> Theme.image [ width <| px 40 ]
                |> el [ alignBottom ]
            , viewSlot (Types.gainToSlot cost)
            , Types.companionToString name
                |> String.Extra.softBreak 16
                |> List.map (gradientText 4 Gradients.blueGradient)
                |> paragraph
                    [ alignBottom
                    , Theme.celticHand
                    , Font.size 36
                    , centerX
                    , Font.center
                    ]
            ]
        , content =
            [ Theme.blocks
                [ height fill
                , Theme.padding
                ]
                description
            ]
        , onPress = msg
        }
