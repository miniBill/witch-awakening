module CostTest exposing (genieMagicalHeart, magicCosts, perkCosts)

import Data.Costs.Magic
import Data.Costs.Monad as CostsMonad
import Data.Costs.Perks
import Data.Costs.Utils
import Expect
import Generated.Types exposing (Affinity(..), Class(..), Faction(..), Magic(..), Perk(..), Race(..))
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
    , class = Just ClassSorceress
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
    sorceress RaceDryad


academicDryad : MagicModel
academicDryad =
    { sorceressDryad
        | class = Just ClassAcademic
    }


sorceressCyborg : MagicModel
sorceressCyborg =
    sorceress RaceCyborg


sorceressSpider : MagicModel
sorceressSpider =
    sorceress RaceSpider


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
            , magic : List RankedMagic
            }
        defaultModel =
            { class = Just ClassWarlock
            , races = [ RaceNeutral ] -- Soul and Body
            , mainRace = Just RaceNeutral
            , cosmicPearl =
                { add = []
                , change = []
                }
            , typePerks = []
            , perks = [ jack12 ]
            , magic = []
            }

        changelingModel :
            { class : Maybe Class
            , races : List Race
            , mainRace : Maybe Race
            , cosmicPearl : CosmicPearlData
            , typePerks : List Race
            , perks : List RankedPerk
            , magic : List RankedMagic
            }
        changelingModel =
            { defaultModel
                | races = [ RaceChangeling ] -- Body and Mind
                , mainRace = Just RaceChangeling
            }
    in
    describe "Jack-of-All"
        [ describe "12p version"
            [ testJack12 "Off affinity, off class" defaultModel 10
            , testJack12 "In affinity, off class" changelingModel 10
            , testJack12 "Off affinity, in class" { defaultModel | class = Just ClassAcademic } 12
            , testJack12 "In affinity, in class" { changelingModel | class = Just ClassAcademic } 12
            ]
        ]


jack12 : { name : Perk, cost : number }
jack12 =
    { name = PerkJackOfAll, cost = -10 }


testJack12 : String -> { a | class : Maybe Class, races : List Race, magic : List RankedMagic, mainRace : Maybe Race, cosmicPearl : CosmicPearlData, typePerks : List Race, perks : List RankedPerk } -> Int -> Test
testJack12 label model expected =
    test label <|
        \_ ->
            Data.Costs.Perks.perkValue model jack12
                |> Result.map (.value >> .points)
                |> Expect.equal (Ok expected)


genieMagicalHeart : Test
genieMagicalHeart =
    test "Genie - Magical Heart" <|
        \_ ->
            Data.Costs.Perks.perkValue
                (sorceress (RaceGenie AffinityFire))
                { name = PerkMagicalHeart
                , cost = 20
                }
                |> Result.map (.value >> .points)
                |> Expect.equal (Ok -4)


magicCosts : Test
magicCosts =
    describe "Magic costs"
        [ describe "Factionless"
            [ testRanks sorceressDryad "Off affinity, off class" MagicRunes 1 3 6 10 15
            , testRanks sorceressDryad "In affinity, off class" MagicAlchemy 1 2 4 6 9
            , testRanks sorceressDryad "Off affinity, in class" MagicFirecalling -1 1 4 8 13
            , testRanks sorceressDryad "In affinity, in class" MagicEarthmoving -1 0 2 4 7
            ]
        , describe "Off faction"
            [ testRanks sorceressDryad "Off affinity, off class" MagicDigicasting 2 6 12 20 30
            , testRanks sorceressDryad "In affinity, off class" MagicWands 1 3 6 10 15
            , testRanks academicDryad "Off affinity, in class" MagicDigicasting 0 4 10 18 28
            , testRanks academicDryad "In affinity, in class" MagicWands -1 1 4 8 13
            ]
        , describe "In faction, no perk"
            [ testRanks { sorceressDryad | faction = Just ( FactionTheHespatianCoven, False ) } "Off affinity, off class" MagicOccultism 1 3 6 10 15
            , testRanks { sorceressDryad | faction = Just ( FactionLunabella, False ) } "In affinity, off class" MagicDominion 1 2 4 6 9
            , testRanks { academicDryad | faction = Just ( FactionTheCollegeOfArcadia, False ) } "Off affinity, in class" MagicDigicasting -1 1 4 8 13
            , testRanks { academicDryad | faction = Just ( FactionHawthorneAcademia, False ) } "In affinity, in class" MagicWands -1 0 2 4 7
            ]
        , describe "In faction, with perk"
            [ testRanks { sorceressDryad | faction = Just ( FactionTheHespatianCoven, True ) } "Off affinity, off class" MagicOccultism 1 2 4 6 9
            , testRanks { sorceressDryad | faction = Just ( FactionLunabella, True ) } "In affinity, off class" MagicDominion 1 2 3 4 6
            , testRanks { academicDryad | faction = Just ( FactionTheCollegeOfArcadia, True ) } "Off affinity, in class" MagicDigicasting -1 0 2 4 7
            , testRanks { academicDryad | faction = Just ( FactionHawthorneAcademia, True ) } "In affinity, in class" MagicWands -1 0 1 2 4
            ]
        , describe "Cyborgs with perk"
            [ testRanks { sorceressCyborg | typePerks = [ RaceCyborg ] } "Gadgetry is in affinity" MagicGadgetry 1 2 4 6 9
            , testRanks { sorceressCyborg | typePerks = [ RaceCyborg ] } "Integration is in affinity" MagicIntegration 1 2 4 6 9
            ]
        , describe "Spiders with perk"
            [ testRanks { sorceressSpider | typePerks = [ RaceSpider ] } "Arachnescence is in affinity" MagicArachnescence 1 2 3 4 6
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
