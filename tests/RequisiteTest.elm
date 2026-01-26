module RequisiteTest exposing (summerSchool)

import Data.Costs.Utils as Utils
import Expect
import Generated.Types exposing (Perk(..))
import Parser
import Test exposing (Test, test)


summerSchool : Test
summerSchool =
    test "Summer School gets parsed correctly" <| \_ ->
    case Parser.run Utils.requisitesParser "Summer School" of
        Err e ->
            Expect.fail (Debug.toString e)

        Ok parsed ->
            parsed
                |> Expect.equal
                    [ Utils.RequiresPerk (PerkSummerSchool [])
                    ]
