module UrlCodec exposing (parseUrl, toUrl)

import AppUrl exposing (AppUrl)
import Dict
import Generated.Class as Class
import Generated.Companion as Companion
import Generated.Complication as Complication
import Generated.Faction as Faction
import Generated.GameMode as GameMode
import Generated.Magic as Magic
import Generated.Perk as Perk
import Generated.Quest as Quest
import Generated.Race as Race
import Generated.Relic as Relic
import Generated.Types as Types exposing (Relic(..))
import List.Extra
import Maybe.Extra
import Set
import Types exposing (Display(..), Model)
import Url exposing (Url)
import Url.Builder exposing (QueryParameter)


parseUrl : key -> Url -> Model key
parseUrl navKey url =
    let
        appUrl : AppUrl
        appUrl =
            AppUrl.fromUrl url

        pair : (String -> Maybe a) -> (a -> Maybe Int -> Maybe b) -> String -> Maybe b
        pair parser builder value =
            let
                ( after, before ) =
                    value
                        |> String.toList
                        |> List.reverse
                        |> List.Extra.span (\c -> Char.isDigit c || c == '-')
                        |> Tuple.mapBoth List.reverse List.reverse
            in
            (parser <| String.fromList before)
                |> Maybe.andThen
                    (\parsed ->
                        let
                            number : Maybe Int
                            number =
                                String.toInt (String.fromList after)
                        in
                        builder parsed number
                    )

        parseComplication : String -> Maybe Types.RankedComplication
        parseComplication =
            pair Types.complicationFromString <|
                \name maybeTier ->
                    { name = name
                    , kind =
                        maybeTier
                            |> Maybe.map Types.Tiered
                            |> Maybe.withDefault
                                Types.Nontiered
                    }
                        |> Just

        parseMagic : String -> Maybe Types.RankedMagic
        parseMagic =
            pair Types.magicFromString <|
                \magic maybeRank ->
                    maybeRank
                        |> Maybe.map
                            (\rank ->
                                { name = magic
                                , rank = rank
                                }
                            )

        parsePerk : String -> Maybe Types.RankedPerk
        parsePerk =
            withCost
                (\raw ->
                    if String.startsWith "Charge Swap" raw && not (String.contains "-" raw) then
                        Types.perkFromString (raw ++ "-Neutral")

                    else
                        Types.perkFromString raw
                )

        parseRelic : String -> Maybe Types.RankedRelic
        parseRelic value =
            if String.startsWith "Cosmic Pearl" value then
                let
                    ( after, before ) =
                        value
                            |> String.toList
                            |> List.reverse
                            |> List.Extra.span Char.isDigit
                            |> Tuple.mapBoth
                                (List.reverse >> String.fromList)
                                (List.reverse >> String.fromList)
                in
                Maybe.map2
                    (\name cost ->
                        { name = name
                        , cost = cost
                        }
                    )
                    (Types.relicFromString before)
                    (String.toInt after)

            else
                withCost Types.relicFromString value

        withCost : (String -> Maybe a) -> String -> Maybe { name : a, cost : Int }
        withCost fromString =
            pair fromString <|
                \name maybeCost ->
                    maybeCost
                        |> Maybe.map
                            (\cost ->
                                { name = name
                                , cost = cost
                                }
                            )

        parseBool : String -> Bool
        parseBool key =
            parseOne key appUrl stringToBool
                |> Maybe.withDefault False

        parseInt : String -> Int
        parseInt key =
            parseOne key appUrl String.toInt
                |> Maybe.withDefault 0

        factions : List Types.Faction
        factions =
            parseMany "faction" appUrl Types.factionFromString

        maybeCosmicPearl : List Types.RankedRelic
        maybeCosmicPearl =
            -- Backward compat
            case
                ( parseMany "addAffinity" appUrl Types.affinityFromString
                , parseMany "changeAffinity" appUrl <|
                    \s ->
                        case String.split "-" s of
                            [ from, to ] ->
                                Maybe.map2 Tuple.pair
                                    (Types.affinityFromString from)
                                    (Types.affinityFromString to)

                            _ ->
                                Nothing
                )
            of
                ( [], [] ) ->
                    []

                ( add, change ) ->
                    [ { name = RelicCosmicPearl { add = add, change = change }
                      , cost = 10 * (List.length add + List.length change)
                      }
                    ]
    in
    { key = navKey
    , menuOpen = False
    , towardsCap = parseInt "towardsCap"
    , capBuild = parseBool "capBuild"
    , powerToRewards = parseInt "powerToRewards"
    , class = parseOne "class" appUrl Types.classFromString
    , classDisplay = DisplayFull
    , races = parseMany "race" appUrl Types.raceFromString
    , mainRace = parseOne "mainRace" appUrl Types.raceFromString
    , raceDisplay = DisplayFull
    , gameMode = parseOne "gameMode" appUrl Types.gameModeFromString
    , gameModeDisplay = DisplayFull
    , complications = parseMany "complication" appUrl parseComplication
    , complicationsDisplay = DisplayFull
    , typePerks = parseMany "typePerk" appUrl Types.raceFromString
    , typePerksDisplay = DisplayFull
    , magic = parseMany "magic" appUrl parseMagic
    , magicDisplay = DisplayFull
    , perks = parseMany "perk" appUrl parsePerk
    , perksDisplay = DisplayFull
    , factions = factions
    , factionPerks =
        parseMany "factionPerk" appUrl Types.factionFromString
            ++ -- Backward compat for builds made before Multi-Factional
               (case parseOne "factionPerk" appUrl stringToBool of
                    Just True ->
                        factions

                    Nothing ->
                        []

                    Just False ->
                        []
               )
    , factionDisplay = DisplayFull
    , factionalMagicDisplay = DisplayFull
    , companions = parseMany "companion" appUrl Types.companionFromString
    , companionsDisplay = DisplayFull
    , relics = parseMany "relic" appUrl parseRelic ++ maybeCosmicPearl
    , relicsDisplay = DisplayFull
    , quests = parseMany "quest" appUrl Types.questFromString
    , questsDisplay = DisplayFull
    , expandedMenuSections = Set.empty
    , hideDLCs = Set.empty
    , hideMeta = False
    }


parseOne : String -> AppUrl -> (String -> Maybe a) -> Maybe a
parseOne key appUrl parser =
    case parseMany key appUrl parser of
        [ one ] ->
            Just one

        _ ->
            Nothing


parseMany : String -> AppUrl -> (String -> Maybe a) -> List a
parseMany key appUrl parser =
    Dict.get key appUrl.queryParameters
        |> Maybe.withDefault []
        |> List.map parser
        |> Maybe.Extra.values


stringToBool : String -> Maybe Bool
stringToBool bool =
    case bool of
        "True" ->
            Just True

        "False" ->
            Just False

        _ ->
            Nothing


toUrl : Model key -> String
toUrl model =
    let
        one : String -> (a -> String) -> Maybe a -> List QueryParameter
        one key f value =
            case value of
                Just v ->
                    [ Url.Builder.string key (f v) ]

                Nothing ->
                    []

        list : String -> (a -> String) -> List a -> List QueryParameter
        list key f values =
            List.map
                (\value -> Url.Builder.string key (f value))
                values

        withDefault : a -> a -> Maybe a
        withDefault default value =
            if value == default then
                Nothing

            else
                Just value

        int : String -> Int -> List QueryParameter
        int key value =
            one key String.fromInt (withDefault 0 value)
    in
    [ one "capBuild" boolToString (withDefault False model.capBuild)
    , int "towardsCap" model.towardsCap
    , int "powerToRewards" model.powerToRewards
    , one "class" Class.toString model.class
    , list "race" Race.toString model.races
    , one "mainRace" Race.toString model.mainRace
    , one "gameMode" GameMode.toString model.gameMode
    , list "typePerk" Race.toString model.typePerks
    , list "complication"
        (\{ name, kind } ->
            Complication.toString name ++ Types.complicationKindToString kind
        )
        model.complications
    , list "magic"
        (\{ name, rank } ->
            Magic.toString name ++ String.fromInt rank
        )
        model.magic
    , list "perk"
        (\{ name, cost } ->
            Perk.toString name ++ String.fromInt cost
        )
        model.perks
    , list "faction" Faction.toString model.factions
    , list "factionPerk" Faction.toString model.factionPerks
    , list "companion" Companion.toString model.companions
    , list "quest" Quest.toString model.quests
    , list "relic"
        (\{ name, cost } ->
            Relic.toString name ++ String.fromInt cost
        )
        model.relics
    ]
        |> List.concat
        |> Url.Builder.toQuery
        |> (\s ->
                if String.isEmpty s then
                    "/"

                else
                    s
           )


boolToString : Bool -> String
boolToString bool =
    if bool then
        "True"

    else
        "False"
