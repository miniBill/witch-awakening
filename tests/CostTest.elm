module CostTest exposing (magicCosts, perkCosts)

import Data.Costs
import Expect
import Generated.Types exposing (Class(..), Faction(..), Magic(..), Perk(..), Race(..))
import ResultME exposing (ResultME)
import Test exposing (Test, describe, test)
import Types exposing (CosmicPearlData, RankedMagic, RankedPerk)


type alias MagicModel =
    { class : Maybe Class
    , races : List Race
    , mainRace : Maybe Race
    , typePerks : List Race
    , magic : List RankedMagic
    , faction : Maybe ( Faction, Bool )
    , cosmicPearl : CosmicPearlData
    }


sorceress : Race -> MagicModel
sorceress race =
    { class = Just Sorceress
    , races = [ race ]
    , mainRace = Just race
    , typePerks = []
    , magic = []
    , faction = Nothing
    , cosmicPearl = { change = [], add = [] }
    }


sorceressDryad : MagicModel
sorceressDryad =
    -- Nature and Earth affinities
    sorceress Dryad


academicDryad : MagicModel
academicDryad =
    { sorceressDryad
        | class = Just Academic
    }


sorceressCyborg : MagicModel
sorceressCyborg =
    sorceress Cyborg


sorceressSpider : MagicModel
sorceressSpider =
    sorceress Spider


expectPower : Int -> ResultME String Data.Costs.Points -> Expect.Expectation
expectPower power value =
    Expect.equal
        ({ power = power
         , rewardPoints = 0
         , warnings = []
         , infos = []
         }
            |> Ok
        )
        value


perkCosts : Test
perkCosts =
    describe "Perk costs"
        [ jackOfAllTest
        ]


jackOfAllTest : Test
jackOfAllTest =
    let
        defaultModel :
            { class : Maybe Class
            , races : List Race
            , mainRace : Maybe Race
            , cosmicPearl : CosmicPearlData
            , typePerks : List Race
            , perks : List RankedPerk
            }
        defaultModel =
            { class = Just Warlock
            , races = [ Neutral ] -- Soul and Body
            , mainRace = Just Neutral
            , cosmicPearl =
                { add = []
                , change = []
                }
            , typePerks = []
            , perks = [ jack12 ]
            }

        changelingModel : { class : Maybe Class, races : List Race, mainRace : Maybe Race, cosmicPearl : CosmicPearlData, typePerks : List Race, perks : List RankedPerk }
        changelingModel =
            { defaultModel
                | races = [ Changeling ] -- Body and Mind
                , mainRace = Just Changeling
            }
    in
    describe "Jack-of-All"
        [ describe "12p version"
            [ testJack12 "Off affinity, off class" defaultModel 10
            , testJack12 "In affinity, off class" changelingModel 10
            , testJack12 "Off affinity, in class" { defaultModel | class = Just Academic } 12
            , testJack12 "In affinity, in class" { changelingModel | class = Just Academic } 12
            ]
        ]


jack12 : { name : Perk, cost : number }
jack12 =
    { name = JackOfAll, cost = -10 }


testJack12 : String -> { a | class : Maybe Class, races : List Race, mainRace : Maybe Race, cosmicPearl : CosmicPearlData, typePerks : List Race, perks : List RankedPerk } -> Int -> Test
testJack12 label model expected =
    test label <|
        \_ ->
            Data.Costs.perkCost model jack12
                |> Expect.equal (Ok -expected)


magicCosts : Test
magicCosts =
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
        , describe "Cyborgs with perk"
            [ testRanks { sorceressCyborg | typePerks = [ Cyborg ] } "Gadgetry is in affinity" Gadgetry 1 2 3 5 8
            , testRanks { sorceressCyborg | typePerks = [ Cyborg ] } "Integration is in affinity" Integration 1 2 3 5 8
            ]
        , describe "Spiders with perk"
            [ testRanks { sorceressSpider | typePerks = [ Spider ] } "Arachnescence is in affinity" Arachnescence 1 1 2 3 5
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
