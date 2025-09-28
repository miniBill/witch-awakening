module Data.Costs.Utils exposing (Points, Requirement(..), affinityDiscountIf, applyClassBonusIf, capWithWarning, checkRequirements, combineAndSum, find, hasMagicAtRank, negate, powerToPoints, requisitesParser, rewardPointsToPoints, slotUnsupported, sum, sumPoints, zero, zeroOut)

import Data.Affinity exposing (InAffinity(..))
import Data.Costs.Monad as Monad exposing (Monad)
import Generated.Classes
import Generated.Magic
import Generated.Types as Types exposing (Class, Magic)
import List.Extra
import Parser exposing ((|.), (|=), Parser)
import Types exposing (RankedMagic)


type alias Points =
    { power : Int
    , rewardPoints : Int
    }


zero : Points
zero =
    { power = 0
    , rewardPoints = 0
    }


negate : Points -> Points
negate p =
    { p | power = -p.power, rewardPoints = -p.rewardPoints }


sum : Points -> Points -> Points
sum l r =
    { power = l.power + r.power
    , rewardPoints = l.rewardPoints + r.rewardPoints
    }


sumPoints : List Points -> Points
sumPoints =
    List.foldl sum zero


combineAndSum : List (Monad Points) -> Monad Points
combineAndSum list =
    list
        |> Monad.combine
        |> Monad.map sumPoints


rewardPointsToPoints : Int -> Points
rewardPointsToPoints value =
    { zero | rewardPoints = value }


powerToPoints : Int -> Points
powerToPoints value =
    { zero | power = value }


zeroOut : Points -> Points
zeroOut points =
    { points | power = 0, rewardPoints = 0 }


applyClassBonusIf : Bool -> Int -> Int
applyClassBonusIf isClass cost =
    if isClass then
        cost - 2

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
        { zero
            | power = cap
        }
            |> Monad.succeed
            |> Monad.withWarning warning

    else
        { zero | power = value }
            |> Monad.succeed


affinityDiscountIf : InAffinity -> Int -> Int
affinityDiscountIf inAffinity cost =
    if cost <= 0 then
        cost

    else
        case inAffinity of
            DoubleAffinity ->
                -- We're rounding down the _second_ halving but not the first
                (cost + 1) // 4

            InAffinity ->
                (cost + 1) // 2

            OffAffinity ->
                cost


requisiteParser : Parser Requirement
requisiteParser =
    let
        magicParser : Parser Magic
        magicParser =
            Generated.Magic.all
                |> List.map (\m -> Parser.succeed m.name |. Parser.keyword (Types.magicToString m.name))
                |> Parser.oneOf

        classParser : Parser Class
        classParser =
            Generated.Classes.all
                |> List.map (\m -> Parser.succeed m.name |. Parser.keyword (Types.classToString m.name))
                |> Parser.oneOf
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
        , Parser.succeed RequiresClass
            |= classParser
        ]


checkRequirements :
    { a | requires : Maybe String }
    -> String
    ->
        { model
            | mainClass : Maybe Class
            , magic : List RankedMagic
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
                                                    Err (Types.magicToString requiredName ++ " " ++ String.fromInt requiredRank)

                                            RequiresClass class ->
                                                if model.mainClass == Just class then
                                                    Ok ()

                                                else
                                                    Err (Types.classToString class)
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
