module View.Magic exposing (viewMagics)

import Data.Magic as Magic exposing (Affinities(..))
import Element exposing (Element, alignBottom, centerX, centerY, column, el, fill, fillPortion, height, moveDown, moveLeft, moveRight, moveUp, padding, px, rgb, rgba, spacing, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Generated.Types as Types exposing (Slot(..))
import Gradients
import Html
import Html.Attributes
import Images
import List.Extra
import Theme
import Types exposing (Choice(..), RankedMagic)


viewMagics : List RankedMagic -> Element Choice
viewMagics selected =
    Theme.column
        [ width fill
        , spacing <| Theme.rythm * 2
        ]
        [ Theme.blocks [] Magic.intro
        , Theme.wrappedRow [ width fill ]
            [ costTable
                |> Element.html
                |> el
                    [ centerX
                    , Background.color <| rgb 1 1 1
                    , Font.color <| rgb 0 0 0
                    ]
            ]
        , Theme.blocks
            [ width <| Element.maximum 600 fill
            , centerX
            , Border.width 1
            , Theme.padding
            , Theme.borderColor Theme.colors.gameMode
            , Font.color <| rgb 1 1 1
            ]
            Magic.slotDescription
        , Magic.all
            |> List.indexedMap (magicBox selected)
            |> Theme.column []
            |> Element.map (\( ranked, select ) -> ChoiceMagic ranked select)
        , Theme.row
            [ Theme.padding
            , Theme.style "background-image" <| "url(\"" ++ Images.magicElementalism.src ++ "\")"
            , Theme.style "background-repeat" "no-repeat"
            , Theme.style "background-position" "top"
            , Theme.style "background-size" "100%, 100%"
            ]
            [ let
                color : Element.Color
                color =
                    rgba 0 0 0 0.5
              in
              Theme.column
                [ Theme.padding
                , width <| Element.minimum 480 <| fillPortion 1
                , Background.color color
                , Border.shadow
                    { offset = ( 0, 0 )
                    , size = 5
                    , blur = 5
                    , color = color
                    }
                ]
                [ el [ Font.size 40, Theme.morpheus ] <|
                    Theme.gradientText 1 Gradients.blueGradient "Elementalism"
                , Theme.blocks [] Magic.elementalismIntro
                ]
            , el [ width <| fillPortion 3 ] Element.none
            ]
        , Magic.elementalism
            |> List.indexedMap (magicBox selected)
            |> Theme.column []
            |> Element.map (\( ranked, select ) -> ChoiceMagic ranked select)
        ]


costTable : Html.Html msg
costTable =
    let
        padding : Html.Attribute msg
        padding =
            Html.Attributes.style "padding" "4px"

        textAlign : String -> Html.Attribute msg
        textAlign =
            Html.Attributes.style "text-align"

        borderLeft : String -> Html.Attribute msg
        borderLeft =
            Html.Attributes.style "border-left"

        borderBottom : String -> Html.Attribute msg
        borderBottom =
            Html.Attributes.style "border-bottom"

        th : String -> Html.Html msg
        th value =
            Html.th
                [ padding
                , textAlign "left"
                ]
                [ Html.text value ]

        th1 : String -> Html.Html msg
        th1 value =
            Html.th
                [ padding
                , textAlign "left"
                , borderBottom "1px solid black"
                ]
                [ Html.text value ]

        th2 : String -> Html.Html msg
        th2 value =
            Html.th
                [ padding
                , Html.Attributes.colspan 2
                , borderLeft "1px solid black"
                , borderBottom "1px solid black"
                ]
                [ Html.text value ]

        td : String -> Html.Html msg
        td value =
            Html.td
                [ padding
                , textAlign "right"
                , if String.startsWith "(" value then
                    Html.Attributes.class ""

                  else
                    borderLeft "1px solid black"
                ]
                [ Html.text value ]

        lastLine : Html.Html msg
        lastLine =
            let
                viewSlot : Slot -> Html.Html msg
                viewSlot slot =
                    Html.td
                        [ padding
                        , Html.Attributes.colspan 2
                        , textAlign "center"
                        ]
                        [ Html.img
                            [ Html.Attributes.src (Types.slotToImage slot).src ]
                            []
                        ]
            in
            Html.tr
                [ padding
                , Html.Attributes.style "background" "black"
                , Html.Attributes.style "color" "white"
                ]
                (Html.td [ textAlign "center" ]
                    [ Theme.gradientTextHtml 1 Gradients.yellowGradient "Slot costs" ]
                    :: List.map viewSlot [ White, Folk, Noble, Heroic, Epic ]
                )

        row : String -> List Int -> Html.Html msg
        row header content =
            content
                |> List.foldl
                    (\i ( nacc, lacc ) ->
                        let
                            next : Int
                            next =
                                nacc + i
                        in
                        ( next
                        , td ("(" ++ String.fromInt next ++ ")")
                            :: td (String.fromInt i)
                            :: lacc
                        )
                    )
                    ( 0, [] )
                |> Tuple.second
                |> List.reverse
                |> (::) (th header)
                |> Html.tr []
    in
    [ Html.tr [] [ th1 "RANKS", th2 "1", th2 "2", th2 "3", th2 "4", th2 "5" ]
    , row "Basic Costs (Totals)" [ 1, 2, 3, 4, 5 ]
    , row "Affinity Costs" [ 1, 1, 2, 2, 3 ]
    , row "Class Costs" [ -1, 2, 3, 4, 5 ]
    , row "Affinity + Class" [ -1, 1, 2, 2, 3 ]
    , lastLine
    ]
        |> Html.table
            [ Html.Attributes.style "border-collapse" "collapse" ]


magicBox : List RankedMagic -> Int -> Magic.Details -> Element ( RankedMagic, Bool )
magicBox selected index details =
    if modBy 2 index == 0 then
        Theme.row []
            [ magicImage details
            , viewContent selected details
            ]

    else
        Theme.row []
            [ viewContent selected details
            , magicImage details
            ]


magicImage : Magic.Details -> Element msg
magicImage { name } =
    el
        [ height fill
        , width <| px 320
        , Background.image (Types.magicToImage name).src
        ]
        Element.none


viewContent : List RankedMagic -> Magic.Details -> Element ( RankedMagic, Bool )
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
                [ magicTitle details
                , Theme.column [ height fill, Theme.padding ] <|
                    Theme.blocks [] description
                        :: List.indexedMap
                            (viewRank selected details)
                            ranks
                ]
        , onPress = msg
        }


magicTitle : Magic.Details -> Element msg
magicTitle { name, star, class, affinities } =
    el
        [ Theme.morpheus
        , Font.size 40
        , centerX
        , Element.onLeft <|
            Theme.row [ moveLeft 8, alignBottom ]
                [ Theme.image
                    [ width <| px 32, centerY ]
                    (Theme.classToBadge class)
                , if star then
                    el [ Font.size 48, moveUp 8, centerY ] <|
                        Theme.gradientText 1 Gradients.yellowGradient "★"

                  else
                    Element.none
                ]
        , Element.onRight <|
            Theme.row
                [ moveRight 8
                , moveDown 4
                , centerY
                ]
                (viewAffinities affinities)
        ]
        (Theme.gradientText 4 Gradients.yellowGradient <|
            Types.magicToString name
        )


viewAffinities : Affinities -> List (Element msg)
viewAffinities affinities =
    case affinities of
        Regular afs ->
            List.map Theme.viewAffinity afs

        Alternative alternatives ->
            alternatives
                |> List.map
                    (\afs ->
                        afs
                            |> List.map Theme.viewAffinity
                            |> List.intersperse
                                (el [ Font.size 24, moveUp 8 ] <|
                                    Theme.gradientText 2 Gradients.yellowGradient " + "
                                )
                    )
                |> List.intersperse
                    [ el [ Font.size 24 ] <|
                        Theme.gradientText 2 Gradients.yellowGradient " OR "
                    ]
                |> List.concat


viewRank : List RankedMagic -> Magic.Details -> Int -> String -> Element ( RankedMagic, Bool )
viewRank selected { name, class } rankIndex label =
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
