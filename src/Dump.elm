module Dump exposing (main)

import Browser
import Data.Companion
import Data.Complication
import Data.Magic
import Data.Perk
import Data.Race
import Data.Relic
import Data.TypePerk
import Dict exposing (Dict)
import Dict.Extra
import Generated.Companion
import Generated.Complication
import Generated.Magic
import Generated.Perk
import Generated.Race
import Generated.Relic
import Generated.TypePerk
import Generated.Types exposing (Faction, classToString, companionToString, complicationCategoryToString, complicationToString, factionToString, magicToString, perkToString, raceToString, relicToString, sizeToString)
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
    | Complication
    | Companion


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
    , category = Complication
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
            (List.map (dlcButton model.dlc) dlcs)
        , Html.div
            [ Html.Attributes.style "display" "flex"
            , Html.Attributes.style "gap" "8px"
            ]
            (List.map (categoryButton model.category)
                [ ( "Races", Race )
                , ( "Perks", Perk )
                , ( "Magic", Magic )
                , ( "Relic", Relic )
                , ( "Complication", Complication )
                , ( "Companion", Companion )
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
                    (\element ->
                        if element.dlc == model.dlc then
                            inner element
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
                    Generated.TypePerk.all
                        |> Dict.Extra.fromListBy (\typePerk -> raceToString typePerk.race)
            in
            go (dumpRace typePerks >> Just) (Generated.Race.all [])

        Perk ->
            go dumpPerk (Generated.Perk.all [])

        Magic ->
            go (dumpMagic >> Just) Generated.Magic.all

        Relic ->
            go dumpRelic Generated.Relic.all

        Complication ->
            go dumpComplication Generated.Complication.all

        Companion ->
            Generated.Companion.all
                |> List.filterMap
                    (\( faction, companions ) ->
                        let
                            filtered : List Data.Companion.Details
                            filtered =
                                List.filter
                                    (\companion -> companion.dlc == model.dlc)
                                    companions
                        in
                        if List.isEmpty filtered then
                            Nothing

                        else
                            Just (dumpCompanions faction filtered)
                    )
                |> String.join "\n\n\n"


dlcs : List (Maybe String)
dlcs =
    Generated.Race.all []
        |> List.map .dlc
        |> List.Extra.unique


dlcButton : Maybe String -> Maybe String -> Html Msg
dlcButton selected dlc =
    Html.button
        [ Html.Events.onClick (DLC dlc)
        , if dlc == selected then
            Html.Attributes.style "background" "red"

          else
            Html.Attributes.style "background" "initial"
        ]
        [ Html.text (Maybe.withDefault "Core" dlc) ]


categoryButton : Category -> ( String, Category ) -> Html Msg
categoryButton selected ( name, category ) =
    Html.button
        [ Html.Events.onClick (Category category)
        , if category == selected then
            Html.Attributes.style "background" "red"

          else
            Html.Attributes.style "background" "initial"
        ]
        [ Html.text name ]


dumpRace : Dict String Data.TypePerk.Details -> Data.Race.Details -> List (Maybe String)
dumpRace typePerks details =
    [ Just <| "## Race: " ++ raceToString details.name
    , listItem "Elements" affinityToString details.affinities
    , item "Mana capacity" sizeToString details.tank
    , item "Mana rate" sizeToString details.charge
    , Just ""
    , Just <| String.Multiline.here details.content
    ]
        ++ (case Dict.get (raceToString details.name) typePerks of
                Nothing ->
                    []

                Just typePerk ->
                    [ Just <| ""
                    , Just <| "### Perk"
                    , item "Cost" String.fromInt typePerk.cost
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
                            |> List.map (\( choice, cost ) -> itemWithCost cost choice)
                            |> String.join "\n"
                      , String.Multiline.here after
                      ]
                        |> List.Extra.removeWhen String.isEmpty
                        |> String.join "\n\n"
                        |> Just
                    )

                Data.Perk.WithCosts costs c ->
                    ( Just costs, Just (String.Multiline.here c) )

                Data.Perk.WithChoicesChargeSwap _ _ ->
                    ( Nothing, Nothing )
    in
    maybeContent
        |> Maybe.map
            (\content ->
                [ Just <| "## Perk: " ++ perkToString details.name
                , item "Element" affinityToString details.affinity
                , item "Class" classToString details.class
                , flagItem "Meta" details.isMeta
                , maybeCost
                    |> Maybe.andThen
                        (\costs ->
                            case costs of
                                [ cost ] ->
                                    item "Cost" String.fromInt cost

                                _ ->
                                    intListItem "Costs" costs
                        )
                , Just ""
                , Just content
                ]
            )


dumpMagic : Data.Magic.Details -> List (Maybe String)
dumpMagic details =
    [ Just <| "## Magic: " ++ magicToString details.name
    , maybeItem "Class" classToString details.class
    , maybeItem "Faction" factionToString details.faction
    , item "Elements" affinitiesToString details.affinities
    , flagItem "Has rank zero" details.hasRankZero
    , flagItem "Elementalism" details.isElementalism
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
        ( cost, maybeDescription ) =
            case relic.content of
                Data.Relic.Single costValue details ->
                    ( item "Cost" String.fromInt costValue, Just details )

                Data.Relic.WithChoices costs details ->
                    ( intListItem "Costs" costs, Just details )

                Data.Relic.CosmicPearlContent _ _ ->
                    ( Nothing, Nothing )
    in
    maybeDescription
        |> Maybe.map
            (\details ->
                [ Just <| "## Relic: " ++ relicToString relic.name
                , case relic.classes of
                    [] ->
                        Nothing

                    [ class ] ->
                        item "Class" classToString class

                    classes ->
                        listItem "Classes" classToString classes
                , cost
                , Just ""
                , Just (String.Multiline.here details)
                ]
            )


dumpCompanions : Maybe Faction -> List Data.Companion.Details -> String
dumpCompanions faction companions =
    companions
        |> List.map (dumpCompanion faction)
        |> String.join "\n\n\n"


dumpCompanion : Maybe Faction -> Data.Companion.Details -> String
dumpCompanion faction companion =
    let
        class : Maybe String
        class =
            case companion.class of
                Data.Companion.ClassNone ->
                    Nothing

                Data.Companion.ClassAny ->
                    Just "Any"

                Data.Companion.ClassOne c ->
                    Just (classToString c)

                Data.Companion.ClassSpecial ->
                    Just "Special"

        score : String -> Data.Companion.Score -> Maybe String
        score label value =
            let
                valueString : String
                valueString =
                    case value of
                        Data.Companion.NormalScore i ->
                            String.fromInt i

                        Data.Companion.SpecialEffect { better, worse } ->
                            case worse of
                                Nothing ->
                                    "1-" ++ String.fromInt better

                                Just w ->
                                    String.fromInt w ++ "-" ++ String.fromInt better
            in
            item label identity valueString

        name : String
        name =
            case companionToString companion.name of
                "Xiao Liena 肖列娜" ->
                    "Xiao Liena"

                n ->
                    n
    in
    [ Just <| "## Companion: " ++ name
    , if companionToString companion.name == name then
        Nothing

      else
        item "Full name" companionToString companion.name
    , maybeItem "Faction" factionToString faction
    , maybeItem "Class" identity class
    , case List.map raceToString companion.races of
        [] ->
            Nothing

        [ race ] ->
            item "Race" identity race

        races ->
            stringListItem "Races" races
    , flagItem "Has Perk" companion.hasPerk
    , maybeItem "Cost" String.fromInt companion.cost
    , score "Power" companion.power
    , score "Teamwork" companion.teamwork
    , score "Sociability" companion.sociability
    , score "Morality" companion.morality
    , if List.isEmpty companion.positives then
        Nothing

      else
        companion.positives
            |> List.filterMap (\p -> item "Positive" identity p)
            |> String.join "\n"
            |> Just
    , if List.isEmpty companion.negatives then
        Nothing

      else
        companion.negatives
            |> List.filterMap (\p -> item "Negative" identity p)
            |> String.join "\n"
            |> Just
    , if List.isEmpty companion.mixed then
        Nothing

      else
        companion.mixed
            |> List.filterMap (\p -> item "Mixed" identity p)
            |> String.join "\n"
            |> Just
    , item "Has" identity companion.has
    , item "Quote" identity companion.quote
    , Just ""
    , Just (String.Multiline.here companion.description)
    ]
        |> List.filterMap identity
        |> String.join "\n"


dumpComplication : Data.Complication.Details -> Maybe (List (Maybe String))
dumpComplication complication =
    let
        ( maybeGain, tiered, maybeDescription ) =
            case complication.content of
                Data.Complication.Single gain details ->
                    ( item "Gain" String.fromInt gain
                    , False
                    , Just (String.Multiline.here details)
                    )

                Data.Complication.WithChoices before choices after ->
                    ( Nothing
                    , False
                    , [ String.Multiline.here before
                      , choices
                            |> List.map (\( choice, cost ) -> itemWithCost cost choice)
                            |> String.join "\n"
                      , String.Multiline.here after
                      ]
                        |> List.Extra.removeWhen String.isEmpty
                        |> String.join "\n\n"
                        |> Just
                    )

                Data.Complication.WithTiers before tiers after ->
                    ( Nothing
                    , True
                    , [ String.Multiline.here before
                      , tiers
                            |> List.map (\( choice, cost ) -> itemWithCost cost choice)
                            |> String.join "\n"
                      , String.Multiline.here after
                      ]
                        |> List.Extra.removeWhen String.isEmpty
                        |> String.join "\n\n"
                        |> Just
                    )

                Data.Complication.WithGains gains details ->
                    ( intListItem "Gains" gains
                    , False
                    , Just (String.Multiline.here details)
                    )
    in
    maybeDescription
        |> Maybe.map
            (\description ->
                [ Just <| "## Complication: " ++ complicationToString complication.name
                , flagItem "Tiered" tiered
                , maybeItem "Category" complicationCategoryToString complication.category
                , maybeItem "Class" classToString complication.class
                , maybeGain
                , Just ""
                , Just description
                ]
            )


item : String -> (a -> String) -> a -> Maybe String
item key toString value =
    Just ("- " ++ key ++ ": " ++ toString value)


itemWithCost : Int -> String -> String
itemWithCost cost choice =
    "- [" ++ String.fromInt cost ++ "] " ++ choice


flagItem : String -> Bool -> Maybe String
flagItem key value =
    if value then
        item key identity "True"

    else
        Nothing


maybeItem : String -> (a -> String) -> Maybe a -> Maybe String
maybeItem key toString value =
    Maybe.map (\v -> "- " ++ key ++ ": " ++ toString v) value


listItem : String -> (a -> String) -> List a -> Maybe String
listItem key toString value =
    item key (\raw -> raw |> List.map toString |> String.join ", ") value


intListItem : String -> List Int -> Maybe String
intListItem key value =
    listItem key String.fromInt value


stringListItem : String -> List String -> Maybe String
stringListItem key value =
    listItem key identity value


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
