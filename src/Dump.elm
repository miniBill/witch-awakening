module Dump exposing (main)

import Browser
import Data.Race
import Data.TypePerk
import Dict exposing (Dict)
import Dict.Extra
import Generated.Races
import Generated.TypePerks
import Generated.Types exposing (affinityToString, raceToString, sizeToString)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import List.Extra
import String.Multiline


type alias Model =
    Maybe String


type alias Msg =
    Maybe String


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = always
        }


init : Model
init =
    Nothing


view : Model -> Html Msg
view dlc =
    let
        typePerks : Dict String Data.TypePerk.Details
        typePerks =
            Generated.TypePerks.all
                |> Dict.Extra.fromListBy (\typePerk -> raceToString typePerk.race)

        raw : String
        raw =
            Generated.Races.all []
                |> List.filter (\race -> race.dlc == dlc)
                |> List.map (dumpRace typePerks)
                |> String.join "\n\n\n"
    in
    Html.div []
        [ Html.div
            [ Html.Attributes.style "display" "flex"
            , Html.Attributes.style "gap" "8px"
            ]
            (List.map dlcButton dlcs)
        , Html.pre [] [ Html.text raw ]
        ]


dlcs : List (Maybe String)
dlcs =
    Generated.Races.all []
        |> List.map .dlc
        |> List.Extra.unique


dlcButton : Msg -> Html Msg
dlcButton dlc =
    Html.button
        [ Html.Events.onClick dlc ]
        [ Html.text (Maybe.withDefault "Core" dlc) ]


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
