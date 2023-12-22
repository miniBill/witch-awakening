module CostTest exposing (suite)

import Data.Costs
import Expect
import Generated.Types exposing (Class(..), Faction(..), Magic(..), Race(..))
import Results exposing (Results(..))
import Test exposing (Test, describe, test)
import Types exposing (CosmicPearlData, RankedMagic)


type alias MagicModel =
    { class : Maybe Class
    , races : List Race
    , mainRace : Maybe Race
    , typePerks : List Race
    , magic : List RankedMagic
    , faction : Maybe ( Faction, Bool )
    , cosmicPearl : CosmicPearlData
    }


sorceressDryad : MagicModel
sorceressDryad =
    { class = Just Sorceress
    , races = [ Dryad ] -- Nature and Earth affinities
    , mainRace = Just Dryad
    , typePerks = []
    , magic = []
    , faction = Nothing
    , cosmicPearl = { change = [], add = [] }
    }


academicDryad : MagicModel
academicDryad =
    { sorceressDryad
        | class = Just Academic
    }


expectPower : Int -> Results Data.Costs.Points -> Expect.Expectation
expectPower power value =
    Expect.equal
        ({ power = power
         , rewardPoints = 0
         , warnings = []
         }
            |> Oks
        )
        value


suite : Test
suite =
    describe "Magic costs"
        [ describe "Factionless"
            [ testRanks sorceressDryad "Off affinity, off class" Runes 1 3 6 10 15
            , testRanks sorceressDryad "In affinity, off class" Alchemy 1 2 4 6 9
            , testRanks sorceressDryad "Off affinity, in class" Firecalling -1 1 4 8 13
            , testRanks sorceressDryad "In affinity, in class" Earthmoving -1 0 2 4 7
            ]
        , describe "Off faction"
            [ testRanks sorceressDryad "Off affinity, off class" Digicasting 2 6 12 20 30
            , testRanks sorceressDryad "In affinity, off class" Wands 2 4 8 12 18
            , testRanks academicDryad "Off affinity, in class" Digicasting -1 2 8 16 26
            , testRanks academicDryad "In affinity, in class" Wands -1 0 4 8 14
            ]
        , describe "In faction, no perk"
            [ testRanks { sorceressDryad | faction = Just ( TheHespatianCoven, False ) } "Off affinity, off class" Occultism 1 3 6 10 15
            , testRanks { sorceressDryad | faction = Just ( Lunabella, False ) } "In affinity, off class" Dominion 1 2 4 6 9
            , testRanks { academicDryad | faction = Just ( TheCollegeOfArcadia, False ) } "Off affinity, in class" Digicasting -1 1 4 8 13
            , testRanks { academicDryad | faction = Just ( HawthorneAcademia, False ) } "In affinity, in class" Wands -1 0 2 4 7
            ]
        , describe "In faction, with perk"
            [ testRanks { sorceressDryad | faction = Just ( TheHespatianCoven, True ) } "Off affinity, off class" Occultism 1 2 3 5 8
            , testRanks { sorceressDryad | faction = Just ( Lunabella, True ) } "In affinity, off class" Dominion 1 1 2 3 5
            , testRanks { academicDryad | faction = Just ( TheCollegeOfArcadia, True ) } "Off affinity, in class" Digicasting -2 1 2 4 7
            , testRanks { academicDryad | faction = Just ( HawthorneAcademia, True ) } "In affinity, in class" Wands -2 0 1 2 4
            ]
        ]


testRanks : MagicModel -> String -> Magic -> Int -> Int -> Int -> Int -> Int -> Test
testRanks model label magic one two three four five =
    [ one, two, three, four, five ]
        |> List.indexedMap
            (\rank expected ->
                test ("Rank " ++ String.fromInt (rank + 1)) <|
                    \_ ->
                        Data.Costs.magicsValue
                            { model
                                | magic =
                                    [ { name = magic
                                      , rank = rank + 1
                                      }
                                    ]
                            }
                            |> expectPower -expected
            )
        |> describe label
