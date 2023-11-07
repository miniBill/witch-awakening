module View.Relic exposing (viewRelics)

import Data.Relic as Relic exposing (Content(..))
import Element exposing (Attribute, Element, alignBottom, alignRight, centerX, centerY, el, fill, height, moveDown, moveLeft, padding, paragraph, px, spacing, text, width)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Generated.Types as Types exposing (Affinity(..), Race, Slot(..))
import Gradients
import List.Extra
import String.Extra
import Theme exposing (gradientText)
import Types exposing (Choice(..), CosmicPearlData, Display, RankedRelic)
import View


viewRelics : Display -> CosmicPearlData -> Maybe Race -> List Race -> List RankedRelic -> Element Choice
viewRelics display pearl mainRace races relics =
    View.collapsible []
        display
        DisplayRelics
        identity
        "# Relics"
        [ Theme.blocks [ centerX ] Relic.intro
        , Relic.all
            |> List.map (relicBox mainRace display relics pearl races)
            |> Theme.wrappedRow
                [ centerX
                , spacing <| Theme.rythm * 3
                ]
        ]
        [ Relic.all
            |> List.map (relicBox mainRace display relics pearl races)
            |> Theme.column
                [ centerX
                , spacing <| Theme.rythm * 3
                ]
        ]


relicBox :
    Maybe Race
    -> Display
    -> List RankedRelic
    -> CosmicPearlData
    -> List Race
    -> Relic.Details
    -> Element Choice
relicBox mainRace display selected pearl races ({ name, class, content } as relic) =
    let
        isSelected : Maybe RankedRelic
        isSelected =
            List.Extra.find (\sel -> sel.name == name) selected

        msg : Maybe Choice
        msg =
            case ( content, isSelected ) of
                ( CosmicPearlContent _ _, Just _ ) ->
                    Nothing

                ( CosmicPearlContent cost _, Nothing ) ->
                    Just <| ChoiceRelic ( { name = name, cost = cost }, True )

                ( _, Just selectedRelic ) ->
                    Just <| ChoiceRelic ( selectedRelic, False )

                ( Single cost _, Nothing ) ->
                    Just <| ChoiceRelic ( { name = name, cost = cost }, True )

                ( WithChoices _ _, Nothing ) ->
                    Nothing

        costs : List Int
        costs =
            (case content of
                WithChoices _ choices ->
                    choices

                Single cost _ ->
                    [ cost ]

                CosmicPearlContent cost _ ->
                    List.map ((*) cost) (List.range 1 4)
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
                    |> gradientText 4 Gradients.yellowGradient

            else
                costs
                    |> List.map costToString
                    |> String.join "/"
                    |> gradientText 4 Gradients.yellowGradient

        viewSlot : Slot -> Element msg
        viewSlot slot =
            Types.slotToImage slot
                |> Theme.image [ width <| px 40 ]
                |> el [ moveDown 4 ]

        color : Int
        color =
            0x00F3EA6F
    in
    Theme.card []
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
            , Theme.classToBadge class
                |> Theme.image [ width <| px 40 ]
                |> el [ alignBottom ]
            , case costs of
                [] ->
                    Element.none

                [ g ] ->
                    viewSlot (Types.gainToSlot g)

                _ ->
                    viewSlot White
            , Types.relicToString name
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
        , content = viewContent mainRace (isSelected /= Nothing) selected pearl races relic color
        , onPress = msg
        }


viewContent : Maybe Race -> Bool -> List RankedRelic -> CosmicPearlData -> List Race -> Relic.Details -> Int -> List (Element Choice)
viewContent mainRace isSelected selected pearl races { content, name } color =
    case content of
        Single _ block ->
            [ Theme.blocks
                [ height fill
                , Theme.padding
                ]
                block
            ]

        CosmicPearlContent cost block ->
            viewCosmicPearl mainRace isSelected pearl races name cost block

        WithChoices before choices ->
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
                            List.member relic selected

                        attrs : List (Attribute msg) -> List (Attribute msg)
                        attrs =
                            if isChoiceSelected then
                                (::) (Theme.backgroundColor color)

                            else
                                identity
                    in
                    Input.button
                        (attrs
                            [ Border.rounded 4
                            , padding 4
                            , Border.width 1
                            , width <| px 24
                            ]
                        )
                        { label =
                            String.fromInt cost
                                |> Theme.gradientText 4 Gradients.yellowGradient
                                |> el [ centerX, centerY, Theme.captureIt ]
                        , onPress = Just <| ChoiceRelic ( relic, not isChoiceSelected )
                        }
            in
            [ Theme.column [ height fill, Theme.padding ] <|
                Theme.blocks [] before
                    :: choicesView
            ]


viewCosmicPearl :
    Maybe Race
    -> Bool
    -> CosmicPearlData
    -> List Race
    -> Types.Relic
    -> Int
    -> String
    -> List (Element Choice)
viewCosmicPearl mainRace isSelected pearl races name cost block =
    let
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
                            isSwapSelected : Bool
                            isSwapSelected =
                                Just to == existing

                            removed : List ( Affinity, Affinity )
                            removed =
                                List.filter (\( f, _ ) -> f /= from) pearl.change
                        in
                        Theme.maybeButton
                            [ if isSwapSelected then
                                Border.glow (Theme.intToColor <| Theme.affinityToColor to) 4

                              else
                                Border.width 0
                            , Border.rounded 999
                            ]
                            { onPress =
                                (if isSwapSelected then
                                    { pearl | change = removed }

                                 else
                                    { pearl | change = ( from, to ) :: removed }
                                )
                                    |> ChoiceCosmicPearl
                                    |> Just
                            , label = Theme.viewAffinity to
                            }
            in
            Theme.column [ width fill ] <|
                [ Theme.row []
                    [ Theme.blocks [] "Swap"
                    , Theme.viewAffinity from
                    , Theme.blocks [] "with"
                    ]
                , Theme.wrappedRow [] <| List.map viewSwap Types.allAffinities
                ]

        addAffinityRow : Int -> Affinity -> Element Choice
        addAffinityRow index existing =
            let
                viewAdd : Affinity -> Element Choice
                viewAdd to =
                    let
                        isAddSelected : Bool
                        isAddSelected =
                            to == existing

                        removed : List Affinity
                        removed =
                            List.filter (\t -> t /= existing) pearl.add
                    in
                    Theme.maybeButton
                        [ if isAddSelected then
                            Border.glow (Theme.intToColor <| Theme.affinityToColor to) 4

                          else
                            Border.width 0
                        , Border.rounded 999
                        ]
                        { onPress =
                            (if isAddSelected then
                                { pearl | add = removed }

                             else
                                { pearl
                                    | add =
                                        if index == 0 then
                                            List.Extra.unique <| to :: removed

                                        else
                                            List.Extra.unique <| removed ++ [ to ]
                                }
                            )
                                |> ChoiceCosmicPearl
                                |> Just
                        , label = Theme.viewAffinity to
                        }
            in
            Theme.column [ width fill ]
                [ Theme.blocks [ width fill ] "Add an affinity: "
                , Theme.wrappedRow [] <| List.map viewAdd Types.allAffinities
                ]

        swapBlock : Race -> List (Element Choice)
        swapBlock race =
            List.map swapAffinityRow (Types.baseAffinities race)

        addBlock : List (Element Choice)
        addBlock =
            List.indexedMap addAffinityRow
                (if List.length pearl.add == 2 then
                    pearl.add

                 else
                    pearl.add ++ [ All ]
                )
    in
    [ Theme.column [ width fill, Theme.padding ]
        [ Theme.blocks
            [ height fill
            ]
            block
        , if isSelected then
            Input.button
                [ width fill
                , Border.width 1
                , Theme.padding
                , Theme.rounded
                , Font.center
                ]
                { onPress =
                    Just (ChoiceRelic ( { name = name, cost = cost }, False ))
                , label = text "Remove"
                }

          else
            Element.none
        , Theme.column
            [ width fill
            , Theme.backgroundColor 0
            , Font.color <| Element.rgb 1 1 1
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
                    Theme.blocks [] "You need to select a main race to change its affinities" :: addBlock
        ]
    ]
