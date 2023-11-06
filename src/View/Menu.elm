module View.Menu exposing (viewMenu)

import Data.Companion as Companion
import Data.Complication as Complication exposing (Content(..))
import Data.FactionalMagic as FactionalMagic
import Data.Magic as Magic exposing (Affinities(..))
import Data.Perk as Perk
import Data.Relic as Relic
import Data.TypePerk as TypePerk
import Element exposing (Element, alignBottom, alignRight, alignTop, centerX, centerY, el, fill, height, padding, paragraph, px, rgb, scrollbarY, shrink, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Generated.Types as Types exposing (Affinity, Class(..), Faction, GameMode(..), Race, Relic(..))
import Gradients
import List.Extra
import Maybe.Extra
import String.Extra
import Theme
import Types exposing (Choice(..), ComplicationKind(..), CosmicPearlData, Model, Msg(..), RankedMagic, RankedPerk, RankedRelic)


viewMenu : Model -> Element Msg
viewMenu model =
    let
        power : Maybe Int
        power =
            calculatePower model

        menuLabel : String
        menuLabel =
            case
                ( Maybe.map String.fromInt power
                , Maybe.map String.fromInt (powerCap model)
                )
            of
                ( Just p, Just c ) ->
                    "[" ++ p ++ "] [/] [" ++ c ++ "]"

                _ ->
                    "(epic)"
    in
    Theme.column
        [ alignTop
        , alignRight
        , height <|
            if model.menuOpen then
                fill

            else
                shrink
        , padding 16
        ]
        [ Input.button
            [ Border.width 1
            , Background.color <| rgb 0.7 0.7 1
            , Border.rounded 999
            , Theme.padding
            , alignRight
            ]
            { onPress =
                if model.menuOpen then
                    Just CloseMenu

                else
                    Just OpenMenu
            , label =
                Theme.blocks
                    [ centerX
                    , centerY
                    ]
                    menuLabel
            }
        , if model.menuOpen then
            viewCalculations model

          else
            Element.none
        ]


viewCalculations : Model -> Element Msg
viewCalculations model =
    let
        row : String -> (Model -> Maybe Int) -> Maybe String -> Element Msg
        row label toValue target =
            case toValue model of
                Nothing ->
                    Theme.row [ width fill ]
                        [ linkLabel label target
                        , rightNumber "?"
                        ]

                Just value ->
                    Theme.row [ width fill ]
                        [ linkLabel label target
                        , rightNumber <| String.fromInt value
                        ]

        link : String -> Maybe String -> Element Msg
        link label target =
            Theme.row [ width fill ]
                [ linkLabel label target
                , rightNumber "-"
                ]

        rightNumber : String -> Element msg
        rightNumber value =
            el
                [ alignRight
                , Theme.captureIt
                , Font.size 20
                ]
                (Theme.gradientText 4 Gradients.yellowGradient value)

        button : { onPress : Maybe msg, label : Element msg } -> Element msg
        button =
            Input.button
                [ Border.width 1
                , padding 4
                , Border.rounded Theme.rythm
                , width fill
                , Font.center
                ]
    in
    Theme.column
        [ Background.color <| rgb 1 1 1
        , Theme.padding
        , scrollbarY
        , height fill
        , Border.rounded Theme.rythm
        , width <| Element.maximum 200 shrink
        ]
        [ paragraph
            [ Font.bold
            , Font.center
            , Font.size 24
            ]
            [ text "🐱culations" ]
        , row "Class" classPower <| Just "True Form - Class"
        , link "Race" <| Just "True Form - Race"
        , row "Initial power" initialPower <| Just "Game Mode"
        , row "Power cap" powerCap <| Just "Game Mode"
        , row "Complications" complicationsValue Nothing
        , row "Type perks" typePerksValue Nothing
        , Input.slider
            [ width fill
            , Element.behindContent <|
                el
                    [ width fill
                    , height (px 2)
                    , centerY
                    , Background.color <| rgb 0.7 0.7 0.7
                    , Border.rounded 2
                    ]
                    Element.none
            ]
            { onChange = Choice << TowardsCap << round
            , label =
                Input.labelAbove [] <|
                    Theme.row []
                        [ paragraph [ Font.bold ]
                            [ text "Assign power to cap instead of initial"
                            , rightNumber <| String.fromInt model.towardsCap
                            ]
                        ]
            , min = 0
            , max = toFloat <| Maybe.withDefault 0 <| complicationsValue model
            , value = toFloat model.towardsCap
            , step = Just 1
            , thumb = Input.defaultThumb
            }
        , row "Magic" magicsValue <| Just "The Magic"
        , row "Perks" perksValue Nothing
        , row "Faction" factionValue Nothing
        , row "Companions" companionsValue Nothing
        , row "Relics" relicsValue Nothing
        , el [ alignBottom, width fill ] <| row "Result" calculatePower Nothing
        , button
            { onPress = Just CompactAll
            , label = text "Compact all"
            }
        , button
            { onPress = Just ExpandAll
            , label = text "Expand all"
            }
        ]


linkLabel : String -> Maybe String -> Element Msg
linkLabel label target =
    Input.button []
        { onPress = Just <| ScrollTo <| String.Extra.underscored <| Maybe.withDefault label target
        , label = el [ Font.bold ] <| text label
        }


calculatePower : Model -> Maybe Int
calculatePower model =
    [ classPower
    , initialPower
    , typePerksValue
    , magicsValue
    , perksValue
    , factionValue
    , companionsValue
    , relicsValue
    ]
        |> Maybe.Extra.traverse (\f -> f model)
        |> Maybe.map List.sum


classPower : Model -> Maybe Int
classPower model =
    Maybe.map
        (\class ->
            case class of
                Warlock ->
                    20

                Academic ->
                    0

                Sorceress ->
                    0
        )
        model.class


initialPower : Model -> Maybe Int
initialPower model =
    case ( model.gameMode, complicationsValue model ) of
        ( Nothing, Just complications ) ->
            Just <| 30 + complications - model.towardsCap

        ( Just StoryArc, Just complications ) ->
            Just <| 10 + complications - model.towardsCap

        ( Just EarlyBird, Just complications ) ->
            Just <| 75 + complications - model.towardsCap

        _ ->
            Nothing


powerCap : Model -> Maybe Int
powerCap model =
    case model.gameMode of
        Nothing ->
            Just <| 100 + model.towardsCap

        Just StoryArc ->
            Just <| 150 + model.towardsCap

        Just EarlyBird ->
            Just <| 75 + model.towardsCap

        Just SkillTree ->
            Nothing

        Just Constellation ->
            Nothing


complicationsValue : Model -> Maybe Int
complicationsValue model =
    maybeSum (complicationValue model) .complications model


complicationValue : Model -> Types.RankedComplication -> Maybe Int
complicationValue model complication =
    let
        get : Int -> List ( a, Int ) -> Maybe Int
        get tier list =
            List.Extra.getAt (tier - 1) list
                |> Maybe.map Tuple.second
    in
    Complication.all
        |> List.Extra.find (\{ name } -> name == complication.name)
        |> Maybe.andThen
            (\details ->
                let
                    raw : Maybe Int
                    raw =
                        case ( details.content, complication.kind ) of
                            ( Single value _, _ ) ->
                                Just value

                            ( WithTiers _ tiers _, Tiered tier ) ->
                                get tier tiers

                            ( WithChoices _ choices _, Tiered tier ) ->
                                get tier choices

                            ( WithCosts _ costs, Tiered tier ) ->
                                List.Extra.getAt (tier - 1) costs

                            ( _, Nontiered ) ->
                                Nothing
                in
                Maybe.map
                    (\r ->
                        let
                            bonus : Int
                            bonus =
                                if details.class == model.class && details.class /= Nothing then
                                    2

                                else
                                    0
                        in
                        r + bonus
                    )
                    raw
            )


typePerksValue : Model -> Maybe Int
typePerksValue =
    maybeSum typePerkValue .typePerks


typePerkValue : Race -> Maybe Int
typePerkValue race =
    List.Extra.find (\perk -> perk.race == race) TypePerk.all
        |> Maybe.map (\{ cost } -> -cost)


magicsValue : Model -> Maybe Int
magicsValue model =
    let
        affinities : List Affinity
        affinities =
            Types.affinities model
    in
    maybeSum (magicValue affinities model) .magic model


magicValue : List Affinity -> Model -> RankedMagic -> Maybe Int
magicValue affinities { faction, class } { name, rank } =
    case
        Magic.all
            |> List.Extra.find (\magic -> magic.name == name)
            |> Maybe.andThen (magicCost affinities class rank)
    of
        Just cost ->
            Just cost

        Nothing ->
            FactionalMagic.all
                |> List.Extra.find (\magic -> magic.name == name)
                |> Maybe.andThen
                    (\magic ->
                        let
                            cost : Maybe Int
                            cost =
                                magicCost affinities class rank magic
                        in
                        if Just ( magic.faction, True ) == faction then
                            Maybe.map (\c -> (c + 1) // 2) cost

                        else
                            Maybe.map ((*) 2) cost
                    )


magicCost :
    List Affinity
    -> Maybe Class
    -> Int
    -> { d | class : Maybe Class, affinities : Affinities }
    -> Maybe Int
magicCost affinities class rank magic =
    let
        isClass : Bool
        isClass =
            (magic.class == class)
                && (class /= Nothing)

        isAffinity : Bool
        isAffinity =
            case magic.affinities of
                Regular regular ->
                    List.any
                        (\affinity -> List.member affinity affinities)
                        regular

                Alternative alternatives ->
                    alternatives
                        |> List.any
                            (\alternative ->
                                List.all
                                    (\affinity -> List.member affinity affinities)
                                    alternative
                            )

        cases : Int -> Int -> Int -> Int -> Int
        cases basicCost affinityCost classCost bothCost =
            if isClass then
                if isAffinity then
                    bothCost

                else
                    classCost

            else if isAffinity then
                affinityCost

            else
                basicCost
    in
    case rank of
        1 ->
            Just <| cases -1 -1 1 1

        2 ->
            Just <| cases -3 -2 -1 0

        3 ->
            Just <| cases -6 -4 -4 -2

        4 ->
            Just <| cases -10 -6 -8 -4

        5 ->
            Just <| cases -15 -9 -13 -7

        _ ->
            Nothing


perksValue : Model -> Maybe Int
perksValue model =
    let
        affinities : List Affinity
        affinities =
            Types.affinities model
    in
    maybeSum (perkValue affinities model.class) .perks model


perkValue : List Affinity -> Maybe Class -> RankedPerk -> Maybe Int
perkValue affinities class { name, cost } =
    Perk.all
        |> List.Extra.find (\perk -> perk.name == name)
        |> Maybe.map
            (\perk ->
                let
                    isClass : Bool
                    isClass =
                        Just perk.class == class

                    isAffinity : Bool
                    isAffinity =
                        List.member perk.affinity affinities
                in
                if isClass then
                    if isAffinity then
                        (-cost + 2) // 2

                    else
                        -cost + 2

                else if isAffinity then
                    -cost // 2

                else
                    -cost
            )


factionValue : Model -> Maybe Int
factionValue model =
    case model.faction of
        Nothing ->
            Just 4

        Just ( _, False ) ->
            Just 2

        Just ( _, True ) ->
            Just 0


companionsValue : Model -> Maybe Int
companionsValue model =
    let
        totalCost : List ( Maybe Faction, Companion.Details ) -> Maybe Int
        totalCost companions =
            companions
                |> Maybe.Extra.traverse (\( _, { cost } ) -> cost)
                |> Maybe.map List.sum

        forFree : List ( Maybe Faction, Companion.Details ) -> Int
        forFree companions =
            let
                byCost : List ( Maybe Faction, Companion.Details )
                byCost =
                    companions
                        |> List.sortBy (\( _, { cost } ) -> -(Maybe.withDefault -1 cost))

                sameFaction : List Companion.Details
                sameFaction =
                    case model.faction of
                        Nothing ->
                            []

                        Just ( f, _ ) ->
                            byCost
                                |> List.filterMap
                                    (\( faction, c ) ->
                                        if faction == Just f then
                                            Just c

                                        else
                                            Nothing
                                    )
                                |> List.take 2

                sameKind : List Companion.Details
                sameKind =
                    byCost
                        |> List.filterMap
                            (\( _, companion ) ->
                                if
                                    sameRace companion model.race
                                        || sameClass companion model.class
                                then
                                    Just companion

                                else
                                    Nothing
                            )
                        |> List.take 2
            in
            if List.isEmpty sameFaction then
                sameKind
                    |> List.head
                    |> Maybe.andThen (\{ cost } -> cost)
                    |> Maybe.withDefault 0

            else if List.isEmpty sameKind then
                sameFaction
                    |> List.head
                    |> Maybe.andThen (\{ cost } -> cost)
                    |> Maybe.withDefault 0

            else
                List.Extra.lift2
                    (\sf sk ->
                        if sf.name == sk.name then
                            sf.cost

                        else
                            Maybe.map2 (+) sf.cost sk.cost
                    )
                    sameFaction
                    sameKind
                    |> List.filterMap identity
                    |> List.maximum
                    |> Maybe.withDefault 0
    in
    model.companions
        |> Maybe.Extra.traverse getCompanion
        |> Maybe.andThen
            (\companions ->
                Maybe.map
                    (\calculatedCost -> forFree companions - calculatedCost)
                    (totalCost companions)
            )


sameClass : Companion.Details -> Maybe Class -> Bool
sameClass companion maybeClass =
    case companion.class of
        Companion.ClassOne class_ ->
            Just class_ == maybeClass

        Companion.ClassAny ->
            True

        Companion.ClassNone ->
            False

        Companion.ClassSpecial ->
            False


sameRace : Companion.Details -> Maybe Race -> Bool
sameRace companion maybeRace =
    List.isEmpty companion.races
        || (case maybeRace of
                Nothing ->
                    False

                Just race ->
                    List.member race companion.races
           )


getCompanion : Types.Companion -> Maybe ( Maybe Faction, Companion.Details )
getCompanion companion =
    List.Extra.findMap
        (\( _, faction, group ) ->
            List.Extra.findMap
                (\({ name } as c) ->
                    if name == companion then
                        Just ( faction, c )

                    else
                        Nothing
                )
                group
        )
        Companion.all


relicsValue : Model -> Maybe Int
relicsValue model =
    maybeSum (relicValue model.class model.cosmicPearl) .relics model


relicValue : Maybe Class -> CosmicPearlData -> RankedRelic -> Maybe Int
relicValue class pearl { name, cost } =
    Relic.all
        |> List.Extra.find (\relic -> relic.name == name)
        |> Maybe.map
            (\relic ->
                let
                    isClass : Bool
                    isClass =
                        Just relic.class == class

                    baseCost : Int
                    baseCost =
                        if isClass then
                            -cost + 2

                        else
                            -cost

                    multiplier : Int
                    multiplier =
                        if name == CosmicPearl then
                            max 1 <| List.length pearl.add + List.length pearl.change

                        else
                            1
                in
                baseCost * multiplier
            )


maybeSum : (item -> Maybe Int) -> (Model -> List item) -> Model -> Maybe Int
maybeSum toValue selector model =
    model
        |> selector
        |> Maybe.Extra.traverse toValue
        |> Maybe.map List.sum
