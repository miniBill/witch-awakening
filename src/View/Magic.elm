module View.Magic exposing (magicBox, viewMagics)

import Data.Magic as Magic exposing (Affinities(..))
import Element exposing (Element, centerX, centerY, column, el, fill, fillPortion, height, moveDown, moveRight, moveUp, px, rgb, rgba, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Generated.Classes
import Generated.Magic
import Generated.Types as Types exposing (Class, Magic, Slot(..))
import Gradients
import Html
import Html.Attributes
import Images
import List.Extra
import Set exposing (Set)
import Theme
import Types exposing (Choice(..), Display(..), RankedMagic)
import View


viewMagics : Set String -> Display -> List RankedMagic -> Element Choice
viewMagics hideDLC display selected =
    let
        filtered : List Magic.Details
        filtered =
            Generated.Magic.all
                |> View.filterDLC hideDLC
                |> List.filter (\{ faction } -> faction == Nothing)
    in
    if List.isEmpty filtered then
        Element.none

    else
        View.collapsible []
            display
            DisplayMagic
            ChoiceMagic
            "# The Magic"
            [ Theme.blocks [] intro
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
                slotDescription
            , filtered
                |> List.Extra.removeWhen .isElementalism
                |> List.indexedMap (magicBox display False selected)
                |> Theme.column []
            , elementalIntro
            , filtered
                |> List.filter .isElementalism
                |> List.indexedMap (magicBox display False selected)
                |> Theme.column []
            ]
            [ filtered
                |> List.sortBy
                    (\{ isElementalism } ->
                        if isElementalism then
                            1

                        else
                            0
                    )
                |> List.indexedMap (magicBox display False selected)
                |> Theme.column []
            ]


intro : String
intro =
    """
    "Time for the fun part. We’ve isolated your true form and primed it for emergence, but that’s only a small part of what you can actually do. The magic varies from witch to witch, we’ll run through the possibilities so I can get a sense of what’s resonating with you. It’s a pretty reliable method of detecting what you’ll be capable of in the future as you explore your abilities and grow your talents."

    Note that these are the possibilities isolated for you, not all witches would have the same opportunities you do. You are exceptional and have more options, and to a higher ceiling than most. Rank 3 in one or two specializations would be considered a capable witch. There are whispers of witches with rank 6 magic, while 7+ are the domain of gods, who are very real.

    {choice Each rank in a magic specialization below costs power equal to its rank, in sequential order}. le; Rank 5 magic costs 15 points in total, rank 3 would be a total of 6p. {choice All Specializations have associated Affinities tagged}. If you have one of these affinities, the magic costs half the power, {choice *rounding up*}. [All] are universally discounted to all affinities.

    For every Rank 5 magic, you must have at least one other magic at Rank 4. For every rank 4, you need 1 magic of rank 3 or less. This does not apply to the either Slot game mode changes, which behave in isolation. Slots stand on their own. This applies to the player, but non-player characters need not adhere to player mechanics and can be presumed to have various less notable magical traits not listed.

    {choice [star] next to the name show that a magic specialization has a universal “Rank 0” effect, which IS available to nearly every witch}, though this does not imply any innate skill with the magic specialization that was built on top of some aspect of that magic.

    If you have at least 1 rank in a Magic Specialization, you can spend _Focus_, _Might_, or _Favor_ to temporarily “power up” to use the higher rank of magic, equal to what the Power cost would be to unlock that rank (e.g.: 5 power = 5 Focus). You can use it for 10 minutes, or extend it for an additional 10 minutes by pushing past your limits resulting in unconsciousness when time runs out.

    {center} {choice Don’t like math? Have a reference table.}
    """


slotDescription : String
slotDescription =
    "If playing in a Slot Mode (Skill Tree or Constellation), Magic instead costs a Slot as shown. Folk slots can buy rank 2 magics. You can have white “Free” slots granting rank 1 magic as granted by your Class discount on options that would cost 0."


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
            , Theme.blocks [] elementalismIntro
            ]
        , el [ width <| fillPortion 3 ] Element.none
        ]


elementalismIntro : String
elementalismIntro =
    """
    The magic presented here in this section is referred to as Elementalism. These single-affinity or monotype magics operate just a little differently from other schools of magic, in that {choice any witch other than sorceresses are incapable of learning more than one elementalist magic _that they do not have the affinity for_}.

    Each magic will show an OR clause. {choice You can qualify for Affinity discounts if you possess both component affinities}, including taking it without the primary monotype affinity.

    If you have more than one elementalist magic, you can combine effects to create combination magic. Such as Firecalling and Earthmoving for lava effects, Windkeeping and Waterworking for storm effects. This is beyond the scope of what the cyoa can detail and up to reasonable interpretation with wiggle room for creative liberties. {choice You can also perform _Harmony_ magic, combining elements of cooperating witches, twice as potent as normal}.

    Witches with an elemental magic at rank 3 are often called _Hazards_. Rank 4 are _Disasters_, and Rank 5 can be labeled as _Calamities_. This designation can influence the weight of consequences placed on you for reckless behavior, people don’t trust living weapons with a track record of being careless when many lives can be at stake if you prove unstable, or outright malevolent. Witches with _Curses_ also receive this designation, as well as others on a case by case basis, such as some grenadier alchemists.

    {choice Elementalist magics cannot be taken for the _Restriction_} complication unless you have the affinity for it.
    """


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
    -> { a | hasRankZero : Bool, class : Maybe Class, affinities : Affinities, description : String, ranks : List String, name : Magic, dlc : Maybe String }
    -> Element ( RankedMagic, Bool )
magicBox display factional selected index details =
    if display == DisplayCompact && List.all (\sel -> sel.name /= details.name) selected then
        Element.none

    else if modBy 2 index == 0 || factional then
        Theme.row [ Theme.id (Types.magicToString details.name) ]
            [ magicImage details
            , viewContent display selected details
            ]

    else
        Theme.row [ Theme.id (Types.magicToString details.name) ]
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


viewContent : Display -> List RankedMagic -> { a | name : Magic, description : String, ranks : List String, hasRankZero : Bool, class : Maybe Class, affinities : Affinities, dlc : Maybe String } -> Element ( RankedMagic, Bool )
viewContent display selected ({ name, description, ranks, dlc } as details) =
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
            Theme.column
                [ width fill
                , Background.color (rgba 0.1 0.1 0.1 0.8)
                , Theme.rounded
                ]
                [ magicTitle display details
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


magicTitle : Display -> { a | name : Magic, hasRankZero : Bool, class : Maybe Class, affinities : Affinities } -> Element msg
magicTitle display { name, hasRankZero, class, affinities } =
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
            , if hasRankZero then
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

            attrs : List (Element.Attribute msg)
            attrs =
                if isTierSelected then
                    let
                        color : Int
                        color =
                            class
                                |> Maybe.map Generated.Classes.classToColor
                                |> Maybe.withDefault Theme.colors.epic
                    in
                    [ Theme.backgroundColor color ]

                else
                    []
        in
        Theme.button
            (width fill :: attrs)
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
