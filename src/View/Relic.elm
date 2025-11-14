module View.Relic exposing (relicToShortString, viewRelics)

import Color exposing (Color)
import Data.Affinity as Affinity
import Data.Relic exposing (Content(..))
import Element exposing (Attribute, Element, alignBottom, alignRight, centerX, centerY, el, fill, height, moveDown, moveLeft, px, spacing, text, width)
import Element.Font as Font
import Generated.Gradient as Gradient
import Generated.Image as Image
import Generated.Relic as Relic
import Generated.Types as Types exposing (Affinity(..), CosmicPearlData, Race, Relic(..), Slot(..))
import List.Extra
import Set exposing (Set)
import Theme
import Types exposing (Choice(..), Display, IdKind(..), RankedRelic)
import View
import View.Affinity as Affinity


viewRelics : Set String -> Display -> Maybe Race -> List Race -> List Race -> List RankedRelic -> Element Choice
viewRelics hideDLC display mainRace races typePerks relics =
    let
        filtered : List Relic.Details
        filtered =
            Relic.all relics
                |> View.filterDLC hideDLC
    in
    if List.isEmpty filtered then
        Element.none

    else
        let
            sorted : List (Element Choice)
            sorted =
                filtered
                    |> List.filterMap (relicBox mainRace display relics races typePerks)
        in
        View.collapsible []
            display
            DisplayRelics
            identity
            IdKindRelic
            "# Relics"
            [ Theme.blocks [ centerX ] IdKindRelic intro
            , sorted
                |> Theme.wrappedRow
                    [ centerX
                    , spacing <| Theme.rhythm * 3
                    ]
            , Theme.image
                [ width fill
                , Theme.style "mask-image" "linear-gradient(to bottom, rgba(0,0,0,0), rgba(0,0,0,1) 50%)"
                ]
                Image.relicFooter
            ]
            [ sorted
                |> Theme.wrappedRow
                    [ centerX
                    , spacing <| Theme.rhythm * 3
                    ]
            ]


relicBox :
    Maybe Race
    -> Display
    -> List RankedRelic
    -> List Race
    -> List Race
    -> Relic.Details
    -> Maybe (Element Choice)
relicBox mainRace display relics races typePerks ({ name, classes, content, dlc } as relic) =
    let
        isSelected : Maybe RankedRelic
        isSelected =
            List.Extra.find (\sel -> Types.isSameRelic sel.name name) relics

        msg : Maybe Choice
        msg =
            case ( name, content, isSelected ) of
                ( RelicCosmicPearl _, _, _ ) ->
                    Nothing

                ( _, _, Just selectedRelic ) ->
                    Just <| ChoiceRelic ( selectedRelic, False )

                ( _, Single cost _, Nothing ) ->
                    Just <| ChoiceRelic ( { name = name, cost = cost }, True )

                ( _, WithChoices _ _, Nothing ) ->
                    Nothing

        costs : List Int
        costs =
            (case content of
                WithChoices choices _ ->
                    choices

                Single cost _ ->
                    case name of
                        RelicCosmicPearl _ ->
                            List.map ((*) cost) (List.range 1 4)

                        _ ->
                            [ cost ]
            )
                |> List.filter ((/=) 0)
                |> List.Extra.unique

        costToString : Int -> String
        costToString cost =
            if cost > 0 then
                "-" ++ String.fromInt cost

            else
                "+" ++ String.fromInt -cost

        costGradient : Element msg
        costGradient =
            if List.length costs >= 4 then
                (List.take 1 costs ++ List.take 1 (List.reverse costs))
                    |> List.map costToString
                    |> String.join "/.../"
                    |> Theme.gradientText 4 Gradient.blueGradient

            else
                costs
                    |> List.map costToString
                    |> String.join "/"
                    |> Theme.gradientText 4 Gradient.blueGradient

        viewSlot : Slot -> Element msg
        viewSlot slot =
            Types.slotToImage slot
                |> Theme.image [ width <| px 40 ]
                |> el [ moveDown 4 ]

        color : Color
        color =
            Color.rgb255 0xF3 0xEA 0x6F

        nameString : String
        nameString =
            relicToShortString name
    in
    Theme.card [ Theme.id IdKindRelic nameString ]
        { display = display
        , forceShow = False
        , glow = color
        , isSelected = isSelected /= Nothing
        , imageAttrs = []
        , imageHeight = 400
        , image = Types.relicToImage name
        , inFront =
            [ el
                [ alignRight
                , Font.size 32
                , Theme.captureIt
                , moveLeft 4
                , moveDown 4
                ]
                costGradient
            , case dlc of
                Nothing ->
                    Element.none

                Just dlcName ->
                    el
                        [ Element.paddingXY 52 0
                        , width fill
                        ]
                    <|
                        Theme.gradientTextWrapped
                            [ centerX
                            , Theme.captureIt
                            , Font.size 24
                            , moveDown 12
                            ]
                            4
                            Gradient.purpleGradient
                            dlcName
            , Theme.viewClasses 40 classes
                |> el [ Element.alignBottom ]
            , case costs of
                [] ->
                    Element.none

                [ g ] ->
                    viewSlot (Types.gainToSlot g)

                _ ->
                    viewSlot SlotWhite
            , el
                [ alignBottom
                , Element.paddingXY 30 8
                , width fill
                ]
              <|
                Theme.gradientTextWrapped
                    [ Theme.captureIt
                    , Font.size 36
                    , centerX
                    ]
                    4
                    Gradient.yellowGradient
                    nameString
            ]
        , content =
            case relic.requires of
                Nothing ->
                    viewContent mainRace isSelected relics races typePerks relic color

                Just req ->
                    View.viewRequirements IdKindRelic req :: viewContent mainRace isSelected relics races typePerks relic color
        , onPress = msg
        }


relicToShortString : Relic -> String
relicToShortString name =
    Relic.toString name
        |> String.split "-"
        |> List.take 1
        |> String.concat


viewContent : Maybe Race -> Maybe RankedRelic -> List RankedRelic -> List Race -> List Race -> Relic.Details -> Color -> List (Element Choice)
viewContent mainRace isSelected relics races typePerks { content, name } color =
    case ( name, content ) of
        ( RelicCosmicPearl pearl, Single cost block ) ->
            viewCosmicPearl mainRace isSelected pearl races typePerks name cost block

        ( _, Single _ block ) ->
            [ Theme.blocks [] IdKindRelic block ]

        ( _, WithChoices choices before ) ->
            let
                choicesView : List (Element Choice)
                choicesView =
                    [ el [ Font.bold ] <| text "Cost:"
                    , choices
                        |> List.map viewChoice
                        |> Theme.wrappedRow []
                    ]

                viewChoice : Int -> Element Choice
                viewChoice cost =
                    let
                        relic : RankedRelic
                        relic =
                            { name = name
                            , cost = cost
                            }

                        isChoiceSelected : Bool
                        isChoiceSelected =
                            List.member relic relics

                        attrs : List (Attribute msg)
                        attrs =
                            if isChoiceSelected then
                                [ Theme.backgroundColor color ]

                            else
                                []
                    in
                    Theme.button
                        ((width <| px 24) :: attrs)
                        { label =
                            String.fromInt cost
                                |> Theme.gradientText 4 Gradient.yellowGradient
                                |> el [ centerX, centerY, Theme.captureIt ]
                        , onPress = Just <| ChoiceRelic ( relic, not isChoiceSelected )
                        }
            in
            Theme.blocks [] IdKindRelic before :: choicesView


viewCosmicPearl :
    Maybe Race
    -> Maybe RankedRelic
    -> CosmicPearlData
    -> List Race
    -> List Race
    -> Types.Relic
    -> Int
    -> String
    -> List (Element Choice)
viewCosmicPearl mainRace isSelected pearl races typePerks name baseCost block =
    let
        toMsg : CosmicPearlData -> Choice
        toMsg newPearl =
            ( { name = RelicCosmicPearl newPearl
              , cost =
                    baseCost * (List.length newPearl.add + List.length newPearl.change)
              }
            , True
            )
                |> ChoiceRelic

        swapAffinityRow : Affinity -> Element Choice
        swapAffinityRow from =
            let
                existing : Maybe Affinity
                existing =
                    List.Extra.find
                        (\( cFrom, _ ) -> cFrom == from)
                        pearl.change
                        |> Maybe.map Tuple.second

                viewSwap : Affinity -> Element Choice
                viewSwap to =
                    if from == to then
                        Element.none

                    else
                        let
                            isButtonSelected : Bool
                            isButtonSelected =
                                Just to == existing

                            removed : List ( Affinity, Affinity )
                            removed =
                                List.filter (\( f, _ ) -> f /= from) pearl.change

                            msg : Choice
                            msg =
                                { pearl
                                    | change =
                                        if isButtonSelected then
                                            removed

                                        else
                                            ( from, to ) :: removed
                                }
                                    |> toMsg
                        in
                        Affinity.button isButtonSelected msg to
            in
            Theme.column [ width fill ] <|
                [ Theme.row []
                    [ Theme.blocks [] IdKindRelic "Swap"
                    , Theme.viewAffinity from
                    , Theme.blocks [] IdKindRelic "with"
                    ]
                , Affinity.selectable
                    |> List.map (\affinity -> viewSwap affinity.name)
                    |> Theme.wrappedRow []
                ]

        addAffinityRow : Int -> Affinity -> Element Choice
        addAffinityRow index existing =
            let
                viewAdd : Affinity -> Element Choice
                viewAdd to =
                    let
                        isButtonSelected : Bool
                        isButtonSelected =
                            to == existing

                        removed : List Affinity
                        removed =
                            List.filter (\t -> t /= existing) pearl.add

                        msg : Choice
                        msg =
                            { pearl
                                | add =
                                    if isButtonSelected then
                                        removed

                                    else if index == 0 then
                                        List.Extra.unique <| to :: removed

                                    else
                                        List.Extra.unique <| removed ++ [ to ]
                            }
                                |> toMsg
                    in
                    Affinity.button isButtonSelected msg to
            in
            Theme.column [ width fill ]
                [ Theme.blocks [ width fill ] IdKindRelic "Add an affinity: "
                , Affinity.selectable
                    |> List.map (\affinity -> viewAdd affinity.name)
                    |> Theme.wrappedRow []
                ]

        swapBlock : Race -> List (Element Choice)
        swapBlock race =
            List.map swapAffinityRow
                (Affinity.toList (Affinity.affinitiesForRace race)
                    ++ Affinity.toList (Affinity.affinitiesForTypePerks typePerks)
                )

        addBlock : List (Element Choice)
        addBlock =
            List.indexedMap addAffinityRow
                (if List.length pearl.add == 2 then
                    pearl.add

                 else
                    pearl.add ++ [ AffinityAll ]
                )
    in
    [ Theme.blocks [ height fill ] IdKindRelic block
    , case isSelected of
        Just { cost } ->
            Theme.button
                [ width fill
                , Font.center
                ]
                { onPress =
                    Just (ChoiceRelic ( { name = name, cost = cost }, False ))
                , label = text "Remove"
                }

        Nothing ->
            Element.none
    , Theme.column
        [ width fill
        , Theme.backgroundColor Color.black
        , Theme.fontColor Color.white
        , Theme.rounded
        , Theme.padding
        ]
      <|
        case ( mainRace, races ) of
            ( Just main, _ ) ->
                swapBlock main ++ addBlock

            ( _, [ main ] ) ->
                swapBlock main ++ addBlock

            _ ->
                Theme.blocks [] IdKindRelic "You need to select a main race to change its affinities" :: addBlock
    ]


intro : String
intro =
    """
    {center} Relics are like perks, but are external boons in the form of magical artifacts, as a rest they aren’t things inherent to yourself, but are things you can acquire over time.

    {center} "Let’s see if we can detect any Relics in your future, they sometimes show up in these tests..."

    {center} {choice *Relics cost REWARD points obtained from Quests, as shown, however you can buy them with POWER instead, if you so choose.*}
    """
