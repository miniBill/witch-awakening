module Data.Costs.Utils exposing (Points, affinityDiscountIf, applyClassBonusIf, capWithWarning, combineAndSum, find, negate, powerToPoints, rewardPointsToPoints, slotUnsupported, sum, sumPoints, zero, zeroOut)

import Data.Affinity exposing (InAffinity(..))
import Data.Costs.Monad as Monad exposing (Monad)
import List.Extra


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
