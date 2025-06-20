module Data.Costs.Utils exposing (Points, applyClassBonusIf, combineAndSum, halveIfPositiveAnd, negate, powerToPoints, sum, zero, zeroOut)

import Data.Costs.Monad as Monad exposing (Monad)


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


halveIfPositiveAnd : Bool -> Int -> Int
halveIfPositiveAnd condition cost =
    if condition && cost > 0 then
        (cost + 1) // 2

    else
        cost
