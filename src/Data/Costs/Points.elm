module Data.Costs.Points exposing (Points, add, fromPower, fromRewardPoints, sum, zero)


zero : Points
zero =
    { power = 0
    , rewardPoints = 0
    }


add : Points -> Points -> Points
add l r =
    { power = l.power + r.power
    , rewardPoints = l.rewardPoints + r.rewardPoints
    }


type alias Points =
    { power : Int
    , rewardPoints : Int
    }


sum : List Points -> Points
sum =
    List.foldl add zero


fromPower : Int -> Points
fromPower p =
    { power = p
    , rewardPoints = 0
    }


fromRewardPoints : Int -> Points
fromRewardPoints r =
    { power = 0
    , rewardPoints = r
    }
