module Data.Costs.Value exposing (Value(..), fromPoints, fromPower, fromRewardPoints, toPoints)

import Data.Costs.Points as Points exposing (Points)


type Value
    = PowerAndRewardPoints Points
    | FreeBecause String


toPoints : Value -> Points
toPoints v =
    case v of
        FreeBecause _ ->
            Points.zero

        PowerAndRewardPoints p ->
            p


fromPoints : Points -> Value
fromPoints p =
    PowerAndRewardPoints p


fromPower : Int -> Value
fromPower p =
    PowerAndRewardPoints (Points.fromPower p)


fromRewardPoints : Int -> Value
fromRewardPoints r =
    PowerAndRewardPoints (Points.fromRewardPoints r)
