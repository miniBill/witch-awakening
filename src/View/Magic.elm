module View.Magic exposing (magicBox, viewMagics)

import Data.Magic as Magic exposing (Affinities(..))
import Element exposing (Element, centerX, centerY, column, el, fill, fillPortion, height, moveDown, moveRight, moveUp, padding, px, rgb, rgba, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Generated.Types as Types exposing (Class, Magic, Slot(..))
import Gradients
import Html
import Html.Attributes
import Images
import List.Extra
import Theme
import Types exposing (Choice(..), Display(..), RankedMagic)
import View


viewMagics : Display -> List RankedMagic -> Element Choice
viewMagics display selected =
    View.collapsible []
        display
        DisplayMagic
        (\( ranked, select ) -> ChoiceMagic ranked select)
        Magic.title
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
        , Magic.nonElemental
            |> List.indexedMap (magicBox display False selected)
            |> Theme.column []
        , elementalIntro
        , Magic.elementalism
            |> List.indexedMap (magicBox display False selected)
            |> Theme.column []
        ]
        [ (Magic.nonElemental ++ Magic.elementalism)
            |> List.indexedMap (magicBox display False selected)
            |> Theme.column []
        ]


elementalIntro : Element msg
elementalIntro =
    Theme.row
        (Theme.padding
            :: Theme.topBackground Images.magicElementalism
        )
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
            [ el
                [ Font.size 58
                , Theme.morpheus
                , Theme.style "letter-spacing" ".15em"
                ]
              <|
                Theme.gradientText 4 Gradients.blueGradient "Elementalism"
            , Theme.blocks [] Magic.elementalismIntro
            ]
        , el [ width <| fillPortion 3 ] Element.none
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


magicBox :
    Display
    -> Bool
    -> List RankedMagic
    -> Int
    -> { a | star : Bool, class : Maybe Class, affinities : Affinities, description : String, ranks : List String, name : Magic }
    -> Element ( RankedMagic, Bool )
magicBox display factional selected index details =
    if display == DisplayCompact && List.all (\sel -> sel.name /= details.name) selected then
        Element.none

    else if modBy 2 index == 0 || factional then
        Theme.row []
            [ magicImage details
            , viewContent display selected details
            ]

    else
        Theme.row []
            [ viewContent display selected details
            , magicImage details
            ]


magicImage : { a | name : Magic } -> Element msg
magicImage { name } =
    el
        [ height fill
        , width <| px 320
        , Background.image (Types.magicToImage name).src
        ]
        Element.none


viewContent : Display -> List RankedMagic -> { a | name : Magic, description : String, ranks : List String, star : Bool, class : Maybe Class, affinities : Affinities } -> Element ( RankedMagic, Bool )
viewContent display selected ({ name, description, ranks } as details) =
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
                [ magicTitle display details
                , Theme.column [ height fill, Theme.padding ] <|
                    Theme.blocks [] description
                        :: List.indexedMap
                            (viewRank selected details)
                            (if display == DisplayFull then
                                ranks

                             else
                                List.take (Maybe.withDefault 0 <| Maybe.map .rank isSelected) ranks
                            )
                ]
        , onPress = msg
        }


magicTitle : Display -> { a | name : Magic, star : Bool, class : Maybe Class, affinities : Affinities } -> Element msg
magicTitle display { name, star, class, affinities } =
    Theme.wrappedRow
        [ Theme.morpheus
        , Font.size 40
        , centerX
        ]
        [ Theme.row [ centerX ]
            [ case class of
                Nothing ->
                    Element.none

                Just c ->
                    Theme.image
                        [ width <| px 32, centerY ]
                        (Theme.classToBadge c)
            , if star then
                el [ Font.size 48, moveUp 8, centerY ] <|
                    Theme.gradientText 1 Gradients.yellowGradient "★"

              else
                Element.none
            , Types.magicToString name
                |> Theme.gradientText 4 Gradients.yellowGradient
                |> el []
            ]
        , if display == DisplayFull then
            Theme.row
                [ moveRight 8
                , moveDown 4
                , centerY
                , centerX
                ]
                (viewAffinities affinities)

          else
            Element.none
        ]


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
                                (Theme.gradientText 2 Gradients.yellowGradient " + "
                                    |> el [ Font.size 24, moveUp 8 ]
                                )
                    )
                |> List.intersperse
                    [ Theme.gradientText 2 Gradients.yellowGradient " OR "
                        |> el [ Font.size 24 ]
                    ]
                |> List.concat


viewRank :
    List RankedMagic
    -> { a | name : Magic, class : Maybe Class }
    -> Int
    -> String
    -> Element ( RankedMagic, Bool )
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
                        class
                            |> Maybe.map Theme.classToColor
                            |> Maybe.withDefault Theme.colors.epic
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
