module View.FactionalMagic exposing (viewFactionalMagics)

import Data.FactionalMagic as FactionalMagic
import Element exposing (Element, centerX, column, el, fill, height, padding, spacing, width)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Generated.Types exposing (Slot(..))
import Gradients
import List.Extra
import Theme
import Types exposing (Choice(..), RankedMagic)
import View.Magic as Magic


viewFactionalMagics : List RankedMagic -> Element Choice
viewFactionalMagics selected =
    Theme.column
        [ width fill
        , spacing <| Theme.rythm * 2
        ]
        [ el
            [ Theme.morpheus
            , centerX
            , Font.size 48
            ]
            (Theme.gradientText 2 Gradients.blueGradient "Factional Magic")
        , Theme.blocks [ centerX, width <| Element.maximum 800 fill ] FactionalMagic.intro
        , FactionalMagic.all
            |> List.indexedMap (factionalMagicBox selected)
            |> Theme.column []
            |> Element.map (\( ranked, select ) -> ChoiceMagic ranked select)
        ]


factionalMagicBox : List RankedMagic -> Int -> FactionalMagic.Details -> Element ( RankedMagic, Bool )
factionalMagicBox selected index details =
    if modBy 2 index == 0 then
        Theme.row []
            [ Magic.magicImage details
            , viewContent selected details
            ]

    else
        Theme.row []
            [ viewContent selected details
            , Magic.magicImage details
            ]


viewContent : List RankedMagic -> FactionalMagic.Details -> Element ( RankedMagic, Bool )
viewContent selected ({ name, description, ranks } as details) =
    let
        isSelected : Maybe RankedMagic
        isSelected =
            List.Extra.find (\sel -> sel.name == name) selected

        msg : Maybe ( RankedMagic, Bool )
        msg =
            Maybe.map (\s -> ( s, False )) isSelected
    in
    Theme.maybeButton [ width fill ]
        { label =
            Theme.column [ width fill ]
                [ Magic.magicTitle details
                , Theme.column [ height fill, Theme.padding ] <|
                    Theme.blocks [] description
                        :: List.indexedMap
                            (viewRank selected details)
                            ranks
                ]
        , onPress = msg
        }


viewRank : List RankedMagic -> FactionalMagic.Details -> Int -> String -> Element ( RankedMagic, Bool )
viewRank selected { name, class } rankIndex label =
    if String.isEmpty label then
        Element.none

    else
        let
            rank : Int
            rank =
                rankIndex + 1

            rankedMagic : RankedMagic
            rankedMagic =
                { name = name
                , rank = rank
                }

            isTierSelected : Bool
            isTierSelected =
                List.member rankedMagic selected
        in
        Input.button
            [ if isTierSelected then
                let
                    color : Int
                    color =
                        Theme.classToColor class
                in
                Theme.backgroundColor color

              else
                Border.width 1
            , Border.width 1
            , Border.rounded 4
            , padding 4
            , width fill
            ]
            { label =
                column []
                    [ el
                        [ Theme.captureIt
                        , Font.size 20
                        , centerX
                        ]
                      <|
                        Theme.gradientText 2
                            Gradients.yellowGradient
                            ("Rank " ++ String.fromInt rank)
                    , Theme.blocks [] label
                    ]
            , onPress = Just ( rankedMagic, not isTierSelected )
            }
