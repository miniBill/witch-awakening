module Dump exposing (main)

import Browser
import Data.Magic
import Data.Perk
import Data.Race
import Data.Relic
import Data.TypePerk
import Dict exposing (Dict)
import Dict.Extra
import Generated.Magics
import Generated.Perks
import Generated.Races
import Generated.Relics
import Generated.TypePerks
import Generated.Types exposing (classToString, magicToString, perkToString, raceToString, relicToString, sizeToString)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import List.Extra
import String.Multiline


type alias Model =
    { dlc : Maybe String
    , category : Category
    }


type Msg
    = DLC (Maybe String)
    | Category Category


type Category
    = Race
    | Perk
    | Magic
    | Relic


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


update : Msg -> Model -> Model
update msg model =
    case msg of
        DLC dlc ->
            { model | dlc = dlc }

        Category category ->
            { model | category = category }


init : Model
init =
    { dlc = Nothing
    , category = Relic
    }


view : Model -> Html Msg
view model =
    Html.div
        [ Html.Attributes.style "display" "flex"
        , Html.Attributes.style "gap" "8px"
        , Html.Attributes.style "flex-direction" "column"
        ]
        [ Html.div
            [ Html.Attributes.style "display" "flex"
            , Html.Attributes.style "gap" "8px"
            ]
            (List.map dlcButton dlcs)
        , Html.div
            [ Html.Attributes.style "display" "flex"
            , Html.Attributes.style "gap" "8px"
            ]
            (List.map categoryButton
                [ ( "Races", Race )
                , ( "Perks", Perk )
                , ( "Magic", Magic )
                ]
            )
        , Html.pre [] [ Html.text (dump model) ]
        ]


dump : Model -> String
dump model =
    let
        go :
            ({ a | dlc : Maybe String } -> Maybe (List (Maybe String)))
            -> List { a | dlc : Maybe String }
            -> String
        go inner list =
            list
                |> List.filterMap
                    (\item ->
                        if item.dlc == model.dlc then
                            inner item
                                |> Maybe.map
                                    (\lines ->
                                        lines
                                            |> List.filterMap identity
                                            |> String.join "\n"
                                    )

                        else
                            Nothing
                    )
                |> String.join "\n\n\n"
    in
    case model.category of
        Race ->
            let
                typePerks : Dict String Data.TypePerk.Details
                typePerks =
                    Generated.TypePerks.all
                        |> Dict.Extra.fromListBy (\typePerk -> raceToString typePerk.race)
            in
            go (dumpRace typePerks >> Just) (Generated.Races.all [])

        Perk ->
            go dumpPerk (Generated.Perks.all [])

        Magic ->
            go (dumpMagic >> Just) Generated.Magics.all

        Relic ->
            go dumpRelic Generated.Relics.all


dlcs : List (Maybe String)
dlcs =
    Generated.Races.all []
        |> List.map .dlc
        |> List.Extra.unique


dlcButton : Maybe String -> Html Msg
dlcButton dlc =
    Html.button
        [ Html.Events.onClick (DLC dlc) ]
        [ Html.text (Maybe.withDefault "Core" dlc) ]


categoryButton : ( String, Category ) -> Html Msg
categoryButton ( name, category ) =
    Html.button
        [ Html.Events.onClick (Category category) ]
        [ Html.text name ]


dumpRace : Dict String Data.TypePerk.Details -> Data.Race.Details -> List (Maybe String)
dumpRace typePerks details =
    [ Just <| "## Race: " ++ raceToString details.name
    , Just <| "- Elements: " ++ String.join ", " (List.map affinityToString details.affinities)
    , Just <| "- Mana capacity: " ++ sizeToString details.tank
    , Just <| "- Mana rate: " ++ sizeToString details.charge
    , Just ""
    , Just <| String.Multiline.here details.content
    ]
        ++ (case Dict.get (raceToString details.name) typePerks of
                Nothing ->
                    []

                Just typePerk ->
                    [ Just <| ""
                    , Just <| "### Perk"
                    , Just <| "- Cost: " ++ String.fromInt typePerk.cost
                    , Just <| ""
                    , Just <| String.Multiline.here typePerk.content
                    ]
           )


dumpPerk : Data.Perk.Details -> Maybe (List (Maybe String))
dumpPerk details =
    let
        ( maybeCost, maybeContent ) =
            case details.content of
                Data.Perk.Single cost c ->
                    ( Just [ cost ], Just (String.Multiline.here c) )

                Data.Perk.WithChoices before choices after ->
                    ( Nothing
                    , [ String.Multiline.here before
                      , choices
                            |> List.map (\( choice, cost ) -> "- [" ++ String.fromInt cost ++ "] " ++ choice)
                            |> String.join "\n"
                      , String.Multiline.here after
                      ]
                        |> List.Extra.removeWhen String.isEmpty
                        |> String.join "\n\n"
                        |> Just
                    )

                Data.Perk.WithCosts costs c ->
                    ( Just costs, Just (String.Multiline.here c) )

                Data.Perk.WithChoicesHybridize _ _ ->
                    ( Nothing, Nothing )

                Data.Perk.WithChoicesChargeSwap _ _ ->
                    ( Nothing, Nothing )
    in
    maybeContent
        |> Maybe.map
            (\content ->
                [ Just <| "## Perk: " ++ perkToString details.name
                , Just <| "- Element: " ++ affinityToString details.affinity
                , Just <| "- Class: " ++ classToString details.class
                , if details.isMeta then
                    Just "- Meta: True"

                  else
                    Nothing
                , maybeCost
                    |> Maybe.map
                        (\costs ->
                            case costs of
                                [ cost ] ->
                                    "- Cost: " ++ String.fromInt cost

                                _ ->
                                    "- Costs: " ++ String.join ", " (List.map String.fromInt costs)
                        )
                , Just ""
                , Just content
                ]
            )


dumpMagic : Data.Magic.Details -> List (Maybe String)
dumpMagic details =
    [ Just <| "## Magic: " ++ magicToString details.name
    , details.class
        |> Maybe.map (\class -> "- Class: " ++ classToString class)
    , Just <| "- Elements: " ++ affinitiesToString details.affinities
    , if details.hasRankZero then
        Just "- Has rank zero: True"

      else
        Nothing
    , if details.isElementalism then
        Just "- Elementalism: True"

      else
        Nothing
    , Just ""
    , Just <| String.Multiline.here details.description
    , Just ""
    , details.ranks
        |> List.indexedMap Tuple.pair
        |> List.filterMap
            (\( rank, description ) ->
                if String.isEmpty description then
                    Nothing

                else
                    [ "### Rank " ++ String.fromInt (rank + 1)
                    , String.Multiline.here description
                    ]
                        |> String.join "\n"
                        |> Just
            )
        |> String.join "\n\n"
        |> Just
    ]


dumpRelic : Data.Relic.Details -> Maybe (List (Maybe String))
dumpRelic relic =
    let
        maybeCostAndDescription : Maybe ( String, String )
        maybeCostAndDescription =
            case relic.content of
                Data.Relic.Single cost details ->
                    Just ( "- Cost: " ++ String.fromInt cost, details )

                Data.Relic.WithChoices costs details ->
                    Just ( "- Costs: " ++ String.join ", " (List.map String.fromInt costs), details )

                Data.Relic.CosmicPearlContent _ _ ->
                    Nothing
    in
    maybeCostAndDescription
        |> Maybe.map
            (\( costString, details ) ->
                [ Just <| "## Relic: " ++ relicToString relic.name
                , Just <| "- Class: " ++ classToString relic.class
                , Just costString
                , Just ""
                , Just (String.Multiline.here details)
                ]
            )


affinitiesToString : Data.Magic.Affinities -> String
affinitiesToString magicAffinities =
    case magicAffinities of
        Data.Magic.Regular affinities ->
            String.join ", " (List.map affinityToString affinities)

        Data.Magic.Alternative alternatives ->
            alternatives
                |> List.map
                    (\alternative ->
                        alternative
                            |> List.map affinityToString
                            |> String.join " + "
                    )
                |> String.join ", "


affinityToString : Generated.Types.Affinity -> String
affinityToString affinity =
    if affinity == Generated.Types.All then
        "All"

    else
        Generated.Types.affinityToString affinity
