module View.Magic exposing (viewMagics)

import Data.Magic as Magic
import Element exposing (Element, centerX, centerY, column, el, fill, height, moveDown, moveLeft, moveRight, padding, px, rgb, spacing, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Generated.Types as Types exposing (Slot(..))
import Gradients
import Html
import Html.Attributes
import List.Extra
import Theme
import Types exposing (Choice(..), RankedMagic)


viewMagics : List RankedMagic -> Element Choice
viewMagics selected =
    Theme.column
        [ width fill
        , spacing <| Theme.rythm * 2
        ]
        [ Theme.blocks [] """
            # The Magic

            "Time for the fun part. We've isolated your true form and primed it for emergence, but that's only a small part of what you can actually do. The magic varies from witch to witch, we'll run through the possibilities so I can get a sense of what's resonating with you. It's a pretty reliable method of detecting what you'll be capable of in the future as you explore your abilities and grow your talents."

            Note that these are the possibilities isolated for you, not all witches would have the same opportunities you do. You are exceptional and have more options, and to a higher ceiling than most. Rank 3 in one or two specializations would be considered a capable witch. There are whispers of witches with rank 6 magic, while 7+ are the domain of gods, who are very real.

            {choice Each rank in a magic specialization below costs power equal to its rank, in sequential order}. le; Rank 5 magic costs 15 points in total, rank 3 would be a total of 6p. {choice All Specializations have associated Affinities tagged}. If you have one of these affinities, the magic costs half the power, {choice *rounding up*}. [???] are universally discounted to all affinities.

            For every Rank 5 magic, you must have at least one other magic at Rank 4. For every rank 4, you need 1 magic of rank 3 or less. This does not apply to the either Slot game mode changes, which behave in isolation. Slots stand on their own. This applies to the player, but non-player characters need not adhere to player mechanics and can be presumed to have various less notable magical traits not listed.

            {choice [star] next to the name show that a magic specialization has a universal “Rank 0” effect, which IS available to nearly every witch}, though this does not imply any innate skill with the magic specialization that was built on top of some aspect of that magic.

            If you have at least 1 rank in a Magic Specialization, you can spend _Focus_, _Might_, or _Favor_ to temporarily “power up” to use the higher rank of magic, equal to what the Power cost would be to unlock that rank (e.g.: 5 power = 5 Focus). You can use it for 10 minutes, or extend it for an additional 10 minutes by pushing past your limits resulting in unconsciousness when time runs out.

            {center} {choice Don't like math? Have a reference table.}"""
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
            "If playing in a Slot Mode (Skill Tree or Constellation), Magic instead costs a Slot as shown. Folk slots can buy rank 2 magics. You can have white “Free” slots granting rank 1 magic as granted by your Class discount on options that would cost 0."
        , Magic.all
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
magicTitle { name, class, affinities } =
    el
        [ Theme.morpheus
        , Font.size 40
        , centerX
        , Element.onLeft <|
            Theme.image
                [ width <| px 32
                , moveLeft 8
                , moveDown 4
                , centerY
                ]
            <|
                Theme.classToBadge class
        , Element.onRight <|
            Theme.row
                [ moveRight 8
                , moveDown 8
                ]
            <|
                List.map Theme.viewAffinity affinities
        ]
        (Theme.gradientText 4 Gradients.yellowGradient <|
            Types.magicToString name
        )


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
