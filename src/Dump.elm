module Dump exposing (main)

import Browser
import Data.Perk
import Data.Race
import Data.TypePerk
import Dict exposing (Dict)
import Dict.Extra
import Generated.Perks
import Generated.Races
import Generated.TypePerks
import Generated.Types exposing (classToString, perkToString, raceToString, sizeToString)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import List.Extra
import String.Multiline


type alias Model =
    { dlc : Maybe String, category : Category }


type Msg
    = DLC (Maybe String)
    | Category Category


type Category
    = Race
    | Perk


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
    , category = Perk
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
            (List.map categoryButton [ ( "Races", Race ), ( "Perks", Perk ) ])
        , Html.pre [] [ Html.text (dump model) ]
        ]


dump : Model -> String
dump model =
    case model.category of
        Race ->
            let
                typePerks : Dict String Data.TypePerk.Details
                typePerks =
                    Generated.TypePerks.all
                        |> Dict.Extra.fromListBy (\typePerk -> raceToString typePerk.race)
            in
            Generated.Races.all []
                |> List.filter (\race -> race.dlc == model.dlc)
                |> List.map (dumpRace typePerks)
                |> String.join "\n\n\n"

        Perk ->
            Generated.Perks.all []
                |> List.filter (\perk -> perk.dlc == model.dlc)
                |> List.filterMap dumpPerk
                |> String.join "\n\n\n"


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


dumpRace : Dict String Data.TypePerk.Details -> Data.Race.Details -> String
dumpRace typePerks details =
    ([ "## Race: " ++ raceToString details.name
     , "- Elements: " ++ String.join ", " (List.map affinityToString details.affinities)
     , "- Mana capacity: " ++ sizeToString details.tank
     , "- Mana rate: " ++ sizeToString details.charge
     , ""
     , String.Multiline.here details.content
     ]
        ++ (case Dict.get (raceToString details.name) typePerks of
                Nothing ->
                    []

                Just typePerk ->
                    [ ""
                    , "### Perk"
                    , "- Cost: " ++ String.fromInt typePerk.cost
                    , ""
                    , String.Multiline.here typePerk.content
                    ]
           )
    )
        |> String.join "\n"


dumpPerk : Data.Perk.Details -> Maybe String
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

                Data.Perk.WithCosts c costs ->
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
                    |> List.filterMap identity
                    |> String.join "\n"
            )


affinityToString : Generated.Types.Affinity -> String
affinityToString affinity =
    if affinity == Generated.Types.All then
        "All"

    else
        Generated.Types.affinityToString affinity
