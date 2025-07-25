module CostTest exposing (magicCosts, perkCosts)

import Data.Costs.Magic
import Data.Costs.Monad as CostsMonad
import Data.Costs.Perks
import Data.Costs.Utils
import Expect
import Generated.Types exposing (Class(..), Faction(..), Magic(..), Perk(..), Race(..))
import Test exposing (Test, describe, test)
import Types exposing (CosmicPearlData, RankedMagic, RankedPerk)


type alias MagicModel =
    { capBuild : Bool
    , class : Maybe Class
    , races : List Race
    , mainRace : Maybe Race
    , typePerks : List Race
    , magic : List RankedMagic
    , faction : Maybe ( Faction, Bool )
    , cosmicPearl : CosmicPearlData
    , perks : List RankedPerk
    }


sorceress : Race -> MagicModel
sorceress race =
    { capBuild = False
    , class = Just Sorceress
    , races = [ race ]
    , mainRace = Just race
    , typePerks = []
    , magic = []
    , perks = []
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


expectPower : Int -> CostsMonad.Monad Data.Costs.Utils.Points -> Expect.Expectation
expectPower power value =
    Expect.equal
        ({ power = power
         , rewardPoints = 0
         }
            |> Ok
        )
        (Result.map .value value)


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
            Data.Costs.Perks.perkValue model jack12
                |> Result.map (.value >> .points)
                |> Expect.equal (Ok expected)


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
            , testRanks sorceressDryad "In affinity, off class" Wands 1 3 6 10 15
            , testRanks academicDryad "Off affinity, in class" Digicasting 0 4 10 18 28
            , testRanks academicDryad "In affinity, in class" Wands -1 1 4 8 13
            ]
        , describe "In faction, no perk"
            [ testRanks { sorceressDryad | faction = Just ( TheHespatianCoven, False ) } "Off affinity, off class" Occultism 1 3 6 10 15
            , testRanks { sorceressDryad | faction = Just ( Lunabella, False ) } "In affinity, off class" Dominion 1 2 4 6 9
            , testRanks { academicDryad | faction = Just ( TheCollegeOfArcadia, False ) } "Off affinity, in class" Digicasting -1 1 4 8 13
            , testRanks { academicDryad | faction = Just ( HawthorneAcademia, False ) } "In affinity, in class" Wands -1 0 2 4 7
            ]
        , describe "In faction, with perk"
            [ testRanks { sorceressDryad | faction = Just ( TheHespatianCoven, True ) } "Off affinity, off class" Occultism 1 2 4 6 9
            , testRanks { sorceressDryad | faction = Just ( Lunabella, True ) } "In affinity, off class" Dominion 1 2 3 4 6
            , testRanks { academicDryad | faction = Just ( TheCollegeOfArcadia, True ) } "Off affinity, in class" Digicasting -1 0 2 4 7
            , testRanks { academicDryad | faction = Just ( HawthorneAcademia, True ) } "In affinity, in class" Wands -1 0 1 2 4
            ]
        , describe "Cyborgs with perk"
            [ testRanks { sorceressCyborg | typePerks = [ Cyborg ] } "Gadgetry is in affinity" Gadgetry 1 2 4 6 9
            , testRanks { sorceressCyborg | typePerks = [ Cyborg ] } "Integration is in affinity" Integration 1 2 4 6 9
            ]
        , describe "Spiders with perk"
            [ testRanks { sorceressSpider | typePerks = [ Spider ] } "Arachnescence is in affinity" Arachnescence 1 2 3 4 6
            ]
        ]


testRanks : MagicModel -> String -> Magic -> Int -> Int -> Int -> Int -> Int -> Test
testRanks model label magic one two three four five =
    [ one, two, three, four, five ]
        |> List.indexedMap
            (\rank expected ->
                test ("Rank " ++ String.fromInt (rank + 1)) <|
                    \_ ->
                        Data.Costs.Magic.value
                            { ignoreSorceressBonus = True }
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
