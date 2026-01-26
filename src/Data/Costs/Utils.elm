module Data.Costs.Utils exposing (Requirement(..), affinityDiscountIf, applyClassBonusIf, capWithWarning, checkRequirements, find, hasMagicAtRank, requisitesParser, slotUnsupported)

import Data.Affinity exposing (InAffinity(..))
import Data.Costs.Monad as Monad exposing (Monad)
import Data.Costs.Points as Points exposing (Points)
import Generated.Class as Class
import Generated.Magic as Magic
import Generated.Perk as Perk
import Generated.Quest as Quest
import Generated.Types as Types exposing (Class, Magic, Perk, Quest)
import List.Extra
import Parser exposing ((|.), (|=), Parser)
import Types exposing (RankedMagic, RankedPerk)


applyClassBonusIf : Bool -> Int -> Int
applyClassBonusIf isClass cost =
    if isClass then
        cost + 2

    else
        cost


find : String -> (item -> key) -> key -> List item -> (key -> String) -> Monad item
find label toKey value list toString =
    case List.Extra.find (\candidate -> toKey candidate == value) list of
        Nothing ->
            Monad.error <| label ++ " " ++ toString value ++ " not found"

        Just v ->
            Monad.succeed v


slotUnsupported : Monad value
slotUnsupported =
    Monad.error "Slot modes not supported yet"


{-| Cap a value to a maximum. Emit a warning if the maximum is exceeded by the input.
-}
capWithWarning : Int -> String -> Int -> Monad Points
capWithWarning cap warning value =
    if value > cap then
        Points.fromPower cap
            |> Monad.succeed
            |> Monad.withWarning warning

    else
        Points.fromPower value
            |> Monad.succeed


affinityDiscountIf : InAffinity -> Int -> Int
affinityDiscountIf inAffinity value =
    if value >= 0 then
        value

    else
        case inAffinity of
            DoubleAffinity ->
                -- We're rounding down the _second_ halving but not the first,
                -- hence why the -1
                (value - 1) // 4

            InAffinity ->
                (value - 1) // 2

            OffAffinity ->
                value


requisiteParser : Parser Requirement
requisiteParser =
    let
        options :
            (name -> res)
            -> (name -> String)
            -> List { kind | name : name }
            -> Parser res
        options variant toString list =
            list
                |> List.map
                    (\m ->
                        let
                            nameString : String
                            nameString =
                                toString m.name
                        in
                        Parser.succeed (variant m.name)
                            |. Parser.keyword
                                (if String.endsWith "-" nameString then
                                    String.slice 0 -1 nameString

                                 else
                                    nameString
                                )
                    )
                |> Parser.oneOf

        magicParser : Parser Magic
        magicParser =
            options identity Magic.toString Magic.all

        classParser : Parser Requirement
        classParser =
            options RequiresClass Class.toString Class.all

        questParser : Parser Requirement
        questParser =
            options RequiresQuest Quest.toString Quest.all

        perkParser : Parser Requirement
        perkParser =
            options RequiresPerk Perk.toString (Perk.all [])
    in
    Parser.oneOf
        [ Parser.succeed RequiresMagic
            |= magicParser
            |. Parser.spaces
            |= Parser.int
            |. Parser.oneOf
                [ Parser.succeed () |. Parser.symbol "+"
                , Parser.succeed ()
                ]
        , classParser
        , questParser
        , perkParser
        ]


checkRequirements :
    { a | requires : Maybe String }
    -> String
    ->
        { model
            | class : Maybe Class
            , magic : List RankedMagic
            , quests : List Quest
            , perks : List RankedPerk
        }
    -> c
    -> Monad c
checkRequirements details nameString model res =
    case details.requires of
        Nothing ->
            Monad.succeed res

        Just req ->
            case Parser.run requisitesParser req of
                Err _ ->
                    Monad.succeed res
                        |> Monad.withWarning ("Failed to parse requisite for " ++ nameString ++ ": " ++ req)

                Ok requisites ->
                    requisites
                        |> Monad.combineMap
                            (\requisite ->
                                let
                                    check : Result String ()
                                    check =
                                        case requisite of
                                            RequiresMagic requiredName requiredRank ->
                                                if hasMagicAtRank model requiredName requiredRank then
                                                    Ok ()

                                                else
                                                    Err (Magic.toString requiredName ++ " " ++ String.fromInt requiredRank)

                                            RequiresClass class ->
                                                if model.class == Just class then
                                                    Ok ()

                                                else
                                                    Err (Class.toString class)

                                            RequiresQuest quest ->
                                                if List.member quest model.quests then
                                                    Ok ()

                                                else
                                                    Err (Quest.toString quest)

                                            RequiresPerk perk ->
                                                if List.any (\p -> Types.isSamePerk p.name perk) model.perks then
                                                    Ok ()

                                                else
                                                    Err (Perk.toString perk)
                                in
                                case check of
                                    Ok a ->
                                        Monad.succeed a

                                    Err e ->
                                        Monad.succeed ()
                                            |> Monad.withWarning
                                                ("Missing requisite for "
                                                    ++ nameString
                                                    ++ ": "
                                                    ++ e
                                                )
                            )
                        |> Monad.map (\_ -> res)


requisitesParser : Parser (List Requirement)
requisitesParser =
    Parser.sequence
        { start = ""
        , end = ""
        , trailing = Parser.Forbidden
        , spaces = Parser.spaces
        , item = requisiteParser
        , separator = ","
        }
        |. Parser.end


hasMagicAtRank : { a | magic : List RankedMagic } -> Magic -> Int -> Bool
hasMagicAtRank model requiredName requiredRank =
    List.any
        (\rankedMagic ->
            rankedMagic.name == requiredName && rankedMagic.rank >= requiredRank
        )
        model.magic


type Requirement
    = RequiresMagic Magic Int
    | RequiresClass Class
    | RequiresPerk Perk
    | RequiresQuest Types.Quest
