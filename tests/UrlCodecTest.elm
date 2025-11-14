module UrlCodecTest exposing (roundtrips)

import Expect
import Fuzz exposing (Fuzzer)
import Generated.Types
import Main
import Set
import Test exposing (Test)
import Types exposing (Model)
import Url


roundtrips : Test
roundtrips =
    Test.fuzz modelFuzzer "URL encoding/decoding roundtrips" <|
        \model ->
            ("http://localhost:8000" ++ Main.toUrl model)
                |> Url.fromString
                |> Maybe.map (Main.parseUrl ())
                |> Expect.equal (Just model)


modelFuzzer : Fuzzer (Model ())
modelFuzzer =
    Fuzz.constant Model
        |> Fuzz.andMap Fuzz.unit
        |> Fuzz.andMap Fuzz.bool
        |> Fuzz.andMap Fuzz.int
        |> Fuzz.andMap Fuzz.int
        |> Fuzz.andMap (Fuzz.constant False)
        |> Fuzz.andMap (Fuzz.maybe classFuzzer)
        |> Fuzz.andMap (Fuzz.constant Types.DisplayFull)
        |> Fuzz.andMap (Fuzz.list raceFuzzer)
        |> Fuzz.andMap (Fuzz.maybe raceFuzzer)
        |> Fuzz.andMap (Fuzz.constant Types.DisplayFull)
        |> Fuzz.andMap (Fuzz.maybe gameModeFuzzer)
        |> Fuzz.andMap (Fuzz.constant Types.DisplayFull)
        |> Fuzz.andMap (Fuzz.list rankedComplicationFuzzer)
        |> Fuzz.andMap (Fuzz.constant Types.DisplayFull)
        |> Fuzz.andMap (Fuzz.list raceFuzzer)
        |> Fuzz.andMap (Fuzz.constant Types.DisplayFull)
        |> Fuzz.andMap (Fuzz.list rankedMagicFuzzer)
        |> Fuzz.andMap (Fuzz.constant Types.DisplayFull)
        |> Fuzz.andMap (Fuzz.list rankedPerkFuzzer)
        |> Fuzz.andMap (Fuzz.constant Types.DisplayFull)
        |> Fuzz.andMap (Fuzz.list factionFuzzer)
        |> Fuzz.andMap (Fuzz.list factionFuzzer)
        |> Fuzz.andMap (Fuzz.constant Types.DisplayFull)
        |> Fuzz.andMap (Fuzz.constant Types.DisplayFull)
        |> Fuzz.andMap (Fuzz.list companionFuzzer)
        |> Fuzz.andMap (Fuzz.constant Types.DisplayFull)
        |> Fuzz.andMap (Fuzz.list questFuzzer)
        |> Fuzz.andMap (Fuzz.constant Types.DisplayFull)
        |> Fuzz.andMap (Fuzz.list rankedRelicFuzzer)
        |> Fuzz.andMap (Fuzz.constant Types.DisplayFull)
        |> Fuzz.andMap (Fuzz.constant Set.empty)
        |> Fuzz.andMap (Fuzz.constant Set.empty)
        |> Fuzz.andMap (Fuzz.constant False)


classFuzzer : Fuzzer Generated.Types.Class
classFuzzer =
    Fuzz.oneOfValues
        [ Generated.Types.ClassAcademic
        , Generated.Types.ClassSorceress
        , Generated.Types.ClassWarlock
        ]


raceFuzzer : Fuzzer Generated.Types.Race
raceFuzzer =
    Fuzz.oneOf
        [ Fuzz.constant Generated.Types.RaceNeutral
        , Fuzz.constant Generated.Types.RaceDaeva
        , Fuzz.constant Generated.Types.RaceIfrit
        , Fuzz.constant Generated.Types.RaceSiren
        , Fuzz.constant Generated.Types.RaceNaiad
        , Fuzz.constant Generated.Types.RaceDryad
        , Fuzz.constant Generated.Types.RaceOread
        , Fuzz.constant Generated.Types.RaceLamia
        , Fuzz.constant Generated.Types.RaceAurai
        , Fuzz.constant Generated.Types.RaceNymph
        , Fuzz.constant Generated.Types.RaceGorgon
        , Fuzz.constant Generated.Types.RaceLuxal
        , Fuzz.constant Generated.Types.RaceKekubi
        , Fuzz.constant Generated.Types.RaceSylph
        , Fuzz.constant Generated.Types.RaceUndine
        , Fuzz.constant Generated.Types.RaceSprite
        , Fuzz.constant Generated.Types.RaceEmpusa
        , Fuzz.constant Generated.Types.RaceLilin
        , Fuzz.constant Generated.Types.RaceErinyes
        , Fuzz.constant Generated.Types.RaceHannya
        , Fuzz.constant Generated.Types.RaceTaura
        , Fuzz.constant Generated.Types.RaceWulong
        , Fuzz.map Generated.Types.RaceDravir affinityFuzzer
        , Fuzz.constant Generated.Types.RaceDoll
        , Fuzz.constant Generated.Types.RaceVanir
        , Fuzz.constant Generated.Types.RaceChangeling
        , Fuzz.constant Generated.Types.RaceElf
        , Fuzz.constant Generated.Types.RaceOrc
        , Fuzz.constant Generated.Types.RacePharon
        , Fuzz.constant Generated.Types.RaceJotun
        , Fuzz.constant Generated.Types.RaceHollow
        , Fuzz.constant Generated.Types.RaceDwarf
        , Fuzz.constant Generated.Types.RaceWither
        , Fuzz.constant Generated.Types.RaceMimi
        , Fuzz.constant Generated.Types.RaceSword
        , Fuzz.constant Generated.Types.RaceCantor
        , Fuzz.constant Generated.Types.RacePhlegethon
        , Fuzz.constant Generated.Types.RaceMoorwalker
        , Fuzz.constant Generated.Types.RacePhantasm
        , Fuzz.constant Generated.Types.RaceGolem
        , Fuzz.constant Generated.Types.RaceMuspel
        , Fuzz.constant Generated.Types.RaceDictum
        , Fuzz.constant Generated.Types.RaceQareen
        , Fuzz.constant Generated.Types.RaceRusalka
        , Fuzz.constant Generated.Types.RaceLares
        , Fuzz.constant Generated.Types.RaceFirebird
        , Fuzz.constant Generated.Types.RaceFresco
        , Fuzz.constant Generated.Types.RaceSilverstream
        , Fuzz.constant Generated.Types.RaceRevenant
        , Fuzz.constant Generated.Types.RacePetrichor
        , Fuzz.constant Generated.Types.RaceGabion
        , Fuzz.constant Generated.Types.RaceSolar
        , Fuzz.constant Generated.Types.RaceIridian
        , Fuzz.constant Generated.Types.RaceHitodama
        , Fuzz.constant Generated.Types.RaceMummy
        , Fuzz.constant Generated.Types.RaceGulabaa
        , Fuzz.constant Generated.Types.RaceGuardian
        , Fuzz.constant Generated.Types.RaceXeno
        , Fuzz.constant Generated.Types.RaceCyborg
        , Fuzz.constant Generated.Types.RaceSpider
        , Fuzz.constant Generated.Types.RaceGnome
        , Fuzz.constant Generated.Types.RacePixie
        , Fuzz.constant Generated.Types.RaceFairy
        , Fuzz.map Generated.Types.RaceGenie affinityFuzzer
        , Fuzz.map Generated.Types.RaceGemini affinityFuzzer
        , Fuzz.constant Generated.Types.RaceHekatonkheire
        , Fuzz.map2 Generated.Types.RaceAmalgam affinityFuzzer affinityFuzzer
        , Fuzz.constant Generated.Types.RaceNyctimene
        , Fuzz.constant Generated.Types.RacePuazi
        , Fuzz.constant Generated.Types.RaceVargr
        , Fuzz.constant Generated.Types.RaceNightgaunt
        , Fuzz.constant Generated.Types.RaceAmazonDryad
        , Fuzz.constant Generated.Types.RaceNightlight
        , Fuzz.constant Generated.Types.RaceGearheart
        , Fuzz.constant Generated.Types.RaceJabberwocky
        , Fuzz.constant Generated.Types.RaceObscura
        , Fuzz.constant Generated.Types.RaceOrnian
        , Fuzz.constant Generated.Types.RaceJackalope
        , Fuzz.constant Generated.Types.RaceShadeglass
        , Fuzz.constant Generated.Types.RaceDraugr
        , Fuzz.constant Generated.Types.RaceCelestial
        , Fuzz.constant Generated.Types.RaceMarid
        , Fuzz.constant Generated.Types.RaceAbyssal
        ]


affinityFuzzer : Fuzzer Generated.Types.Affinity
affinityFuzzer =
    Fuzz.oneOfValues
        [ Generated.Types.AffinityAll
        , Generated.Types.AffinityBeast
        , Generated.Types.AffinityBlood
        , Generated.Types.AffinityBody
        , Generated.Types.AffinityEarth
        , Generated.Types.AffinityFire
        , Generated.Types.AffinityLife
        , Generated.Types.AffinityMetal
        , Generated.Types.AffinityMind
        , Generated.Types.AffinityNature
        , Generated.Types.AffinityNecro
        , Generated.Types.AffinitySoul
        , Generated.Types.AffinityWater
        , Generated.Types.AffinityWind
        , Generated.Types.AffinityShadows
        ]


gameModeFuzzer : Fuzzer Generated.Types.GameMode
gameModeFuzzer =
    Fuzz.oneOfValues
        [ Generated.Types.GameModeStoryArc
        , Generated.Types.GameModeEarlyBird
        , Generated.Types.GameModeSkillTree
        , Generated.Types.GameModeConstellation
        ]


rankedComplicationFuzzer : Fuzzer Types.RankedComplication
rankedComplicationFuzzer =
    Fuzz.map2 Types.RankedComplication complicationFuzzer complicationKindFuzzer


complicationFuzzer : Fuzzer Generated.Types.Complication
complicationFuzzer =
    Fuzz.oneOfValues
        [ Generated.Types.ComplicationBrutality
        , Generated.Types.ComplicationMasquerade
        , Generated.Types.ComplicationTrueNames
        , Generated.Types.ComplicationMonsters
        , Generated.Types.ComplicationPopulation
        , Generated.Types.ComplicationBonk
        , Generated.Types.ComplicationDysfunction
        , Generated.Types.ComplicationVulnerability
        , Generated.Types.ComplicationRejection
        , Generated.Types.ComplicationCrutch
        , Generated.Types.ComplicationRestriction
        , Generated.Types.ComplicationHunted
        , Generated.Types.ComplicationDislikeable
        , Generated.Types.ComplicationMonsterBait
        , Generated.Types.ComplicationBlackSwan
        , Generated.Types.ComplicationSpellSink
        , Generated.Types.ComplicationLikeADuck
        , Generated.Types.ComplicationLikeARock
        , Generated.Types.ComplicationEyeCatcher
        , Generated.Types.ComplicationSillyGoose
        , Generated.Types.ComplicationHardLessons
        , Generated.Types.ComplicationColdHeart
        , Generated.Types.ComplicationHideous
        , Generated.Types.ComplicationWitchMark
        , Generated.Types.ComplicationNemesis
        , Generated.Types.ComplicationAddiction
        , Generated.Types.ComplicationSensoryDisability
        , Generated.Types.ComplicationPhysicalDisability
        , Generated.Types.ComplicationSensoryShock
        , Generated.Types.ComplicationAdoringFan
        , Generated.Types.ComplicationVeryDere
        , Generated.Types.ComplicationRequirement
        , Generated.Types.ComplicationUnveiled
        , Generated.Types.ComplicationNightmares
        , Generated.Types.ComplicationKryptonite
        , Generated.Types.ComplicationFitWitch
        , Generated.Types.ComplicationBranded
        , Generated.Types.ComplicationNoPrivacy
        , Generated.Types.ComplicationBloodFeud
        , Generated.Types.ComplicationMarked
        , Generated.Types.ComplicationDefeated
        , Generated.Types.ComplicationFixation
        , Generated.Types.ComplicationAllNatural
        , Generated.Types.ComplicationWitchknight
        , Generated.Types.ComplicationInadequacy
        , Generated.Types.ComplicationDysphoria
        , Generated.Types.ComplicationBetrayal
        , Generated.Types.ComplicationCompulsion
        ]


complicationKindFuzzer : Fuzzer Types.ComplicationKind
complicationKindFuzzer =
    Fuzz.oneOf [ Fuzz.map Types.Tiered Fuzz.int, Fuzz.constant Types.Nontiered ]


rankedMagicFuzzer : Fuzzer Types.RankedMagic
rankedMagicFuzzer =
    Fuzz.map2 Types.RankedMagic magicFuzzer Fuzz.int


magicFuzzer : Fuzzer Generated.Types.Magic
magicFuzzer =
    Fuzz.oneOfValues
        [ Generated.Types.MagicDigicasting
        , Generated.Types.MagicWands
        , Generated.Types.MagicMinistration
        , Generated.Types.MagicOccultism
        , Generated.Types.MagicDominion
        , Generated.Types.MagicCovenants
        , Generated.Types.MagicMonstrosity
        , Generated.Types.MagicGadgetry
        , Generated.Types.MagicIntegration
        , Generated.Types.MagicAlchemy
        , Generated.Types.MagicRunes
        , Generated.Types.MagicCurses
        , Generated.Types.MagicHexes
        , Generated.Types.MagicWitchery
        , Generated.Types.MagicFamiliarity
        , Generated.Types.MagicNecromancy
        , Generated.Types.MagicConsortation
        , Generated.Types.MagicPortals
        , Generated.Types.MagicDivination
        , Generated.Types.MagicAethernautics
        , Generated.Types.MagicFirecalling
        , Generated.Types.MagicWindkeeping
        , Generated.Types.MagicWaterworking
        , Generated.Types.MagicEarthmoving
        , Generated.Types.MagicNaturalism
        , Generated.Types.MagicMetamorphosis
        , Generated.Types.MagicPsychotics
        , Generated.Types.MagicMetallurgy
        , Generated.Types.MagicLifeweaving
        , Generated.Types.MagicVisceramancy
        , Generated.Types.MagicArachnescence
        , Generated.Types.MagicWishcasting
        , Generated.Types.MagicBodyRefinement
        , Generated.Types.MagicSpiritPuppetry
        , Generated.Types.MagicShadowSinging
        , Generated.Types.MagicBlighting
        , Generated.Types.MagicConcealing
        , Generated.Types.MagicStellarism
        , Generated.Types.MagicAugmentation
        , Generated.Types.MagicSagaCalling
        , Generated.Types.MagicShapeShifting
        , Generated.Types.MagicCounterMagic
        , Generated.Types.MagicConstructs
        , Generated.Types.MagicManaShaping
        , Generated.Types.MagicEmpathics
        , Generated.Types.MagicReinforcement
        , Generated.Types.MagicKinomancy
        , Generated.Types.MagicIllusions
        , Generated.Types.MagicResonance
        , Generated.Types.MagicCrystalCrafting
        , Generated.Types.MagicHomunculusCraft
        , Generated.Types.MagicAdvancedGolemancy
        , Generated.Types.MagicSpiritWeaving
        , Generated.Types.MagicPlagues
        , Generated.Types.MagicEntomism
        , Generated.Types.MagicWorldShaping
        , Generated.Types.MagicTheHallowingEcho
        , Generated.Types.MagicTimewarping
        , Generated.Types.MagicReaping
        ]


rankedPerkFuzzer : Fuzzer Types.RankedPerk
rankedPerkFuzzer =
    Fuzz.map2 Types.RankedPerk perkFuzzer Fuzz.int


perkFuzzer : Fuzzer Generated.Types.Perk
perkFuzzer =
    Fuzz.oneOf
        [ Fuzz.constant Generated.Types.PerkOracle
        , Fuzz.constant Generated.Types.PerkJackOfAll
        , Fuzz.constant Generated.Types.PerkTransformationSequence
        , Fuzz.constant Generated.Types.PerkPoisoner
        , Fuzz.constant Generated.Types.PerkWitchflame
        , Fuzz.constant Generated.Types.PerkEnergized
        , Fuzz.constant Generated.Types.PerkConjuration
        , Fuzz.constant Generated.Types.PerkElephantTrunk
        , Fuzz.constant Generated.Types.PerkPrestidigitation
        , Fuzz.constant Generated.Types.PerkSuggestion
        , Fuzz.constant Generated.Types.PerkFascinate
        , Fuzz.constant Generated.Types.PerkPantomime
        , Fuzz.constant Generated.Types.PerkBeautySleep
        , Fuzz.constant Generated.Types.PerkThirdEye
        , Fuzz.constant Generated.Types.PerkSoulJellies
        , Fuzz.constant Generated.Types.PerkHatTrick
        , Fuzz.constant Generated.Types.PerkMoodWeather
        , Fuzz.constant Generated.Types.PerkImprovedFamiliar
        , Fuzz.constant Generated.Types.PerkHybridize
        , Fuzz.constant Generated.Types.PerkApex
        , Fuzz.constant Generated.Types.PerkCrystallize
        , Fuzz.constant Generated.Types.PerkMemorize
        , Fuzz.constant Generated.Types.PerkMaidHand
        , Fuzz.constant Generated.Types.PerkHotSwap
        , Fuzz.constant Generated.Types.PerkMenagerie
        , Fuzz.constant Generated.Types.PerkBloodWitch
        , Fuzz.constant Generated.Types.PerkGunwitch
        , Fuzz.constant Generated.Types.PerkLevitation
        , Fuzz.constant Generated.Types.PerkIsekaid
        , Fuzz.constant Generated.Types.PerkHeritage
        , Fuzz.constant Generated.Types.PerkMagicFriendship
        , Fuzz.constant Generated.Types.PerkWindsong
        , Fuzz.constant Generated.Types.PerkBroomBeast
        , Fuzz.constant Generated.Types.PerkIsekaiWorlds
        , Fuzz.constant Generated.Types.PerkIsekaiHeritage
        , Fuzz.constant Generated.Types.PerkSummerSchool
        , Fuzz.constant Generated.Types.PerkMagicalHeart
        , Fuzz.constant Generated.Types.PerkMiniaturization
        , Fuzz.constant Generated.Types.PerkSoulWarrior
        , Fuzz.constant Generated.Types.PerkComfyPocket
        , Fuzz.constant Generated.Types.PerkImprovedRod
        , Fuzz.constant Generated.Types.PerkWitchHut
        , Fuzz.constant Generated.Types.PerkCompany
        , Fuzz.constant Generated.Types.PerkPetBreak
        , Fuzz.constant Generated.Types.PerkMagicShop
        , Fuzz.constant Generated.Types.PerkKeeper
        , Fuzz.constant Generated.Types.PerkSoulGraft
        , Fuzz.constant Generated.Types.PerkFogSight
        , Fuzz.constant Generated.Types.PerkStasis
        , Fuzz.constant Generated.Types.PerkSkimmerDancing
        , Fuzz.constant Generated.Types.PerkWindWhisperer
        , Fuzz.constant Generated.Types.PerkDepthSearch
        , Fuzz.constant Generated.Types.PerkAbsoluteZeroMomentum
        , Fuzz.constant Generated.Types.PerkDiggyDiggyHole
        , Fuzz.constant Generated.Types.PerkAspicioMori
        , Fuzz.constant Generated.Types.PerkBloodline
        , Fuzz.constant Generated.Types.PerkTechnopath
        , Fuzz.constant Generated.Types.PerkBeaconGates
        , Fuzz.constant Generated.Types.PerkGreaterDragon
        , Fuzz.constant Generated.Types.PerkLavenderExaltation
        , Fuzz.constant Generated.Types.PerkTheMask
        , Fuzz.constant Generated.Types.PerkBeastFodder
        , Fuzz.constant Generated.Types.PerkCursedLineage
        , Fuzz.constant Generated.Types.PerkYouKnowWho
        , Fuzz.constant Generated.Types.PerkSerpentSGaze
        , Fuzz.constant Generated.Types.PerkBabel
        , Fuzz.constant Generated.Types.PerkDarkVision
        , Fuzz.constant Generated.Types.PerkDramaticFlair
        , Fuzz.constant Generated.Types.PerkPseudoSkinwalker
        , Fuzz.constant Generated.Types.PerkPathways
        , Fuzz.constant Generated.Types.PerkForbiddenKnowledge
        , Fuzz.constant Generated.Types.PerkAKindFate
        , Fuzz.constant Generated.Types.PerkChanceMage
        , Fuzz.constant Generated.Types.PerkFullSteamAhead
        , Fuzz.constant Generated.Types.PerkWhisperingWind
        , Fuzz.constant Generated.Types.PerkScrollMaking
        , Fuzz.constant Generated.Types.PerkSealingWards
        , Fuzz.constant Generated.Types.PerkMagicMuffinsAndCursedCookies
        , Fuzz.constant Generated.Types.PerkMonsterTamer
        , Fuzz.constant Generated.Types.PerkTechnomancy
        , Fuzz.constant Generated.Types.PerkBloodborne
        , Fuzz.constant Generated.Types.PerkLifeOnTheRim
        , Fuzz.constant Generated.Types.PerkNecromancer
        , Fuzz.constant Generated.Types.PerkFiendsAndFire
        , Fuzz.constant Generated.Types.PerkTheDarkZone
        , Fuzz.constant Generated.Types.PerkEldritchEstate
        , Fuzz.map Generated.Types.PerkChargeSwap raceFuzzer
        ]


factionFuzzer : Fuzzer Generated.Types.Faction
factionFuzzer =
    Fuzz.oneOfValues
        [ Generated.Types.FactionTheCollegeOfArcadia
        , Generated.Types.FactionHawthorneAcademia
        , Generated.Types.FactionTheWatchers
        , Generated.Types.FactionTheHespatianCoven
        , Generated.Types.FactionLunabella
        , Generated.Types.FactionAlfheimrAlliance
        , Generated.Types.FactionTheFourFamilies
        , Generated.Types.FactionTheOutsiders
        , Generated.Types.FactionTheORC
        , Generated.Types.FactionAlphazonIndustries
        , Generated.Types.FactionTheLodge
        , Generated.Types.FactionIndependents
        ]


companionFuzzer : Fuzzer Generated.Types.Companion
companionFuzzer =
    Fuzz.oneOfValues
        [ Generated.Types.CompanionRachelPool
        , Generated.Types.CompanionAnneLaurenchi
        , Generated.Types.CompanionCandayWesbank
        , Generated.Types.CompanionTessaMarieKudashov
        , Generated.Types.CompanionEvelynnPWillowcrane
        , Generated.Types.CompanionJohnDoe
        , Generated.Types.CompanionSayaKurosawa
        , Generated.Types.CompanionFrancescaAstrenichtys
        , Generated.Types.CompanionElaineAVictorica
        , Generated.Types.CompanionMaimonadaMajesteim
        , Generated.Types.CompanionAzurellieaAdMadelline
        , Generated.Types.CompanionMelissaVincimvitch
        , Generated.Types.CompanionHannahGrangely
        , Generated.Types.CompanionElizabellSinclaire
        , Generated.Types.CompanionAshleyLovenko
        , Generated.Types.CompanionSylvanneMaeKanzaki
        , Generated.Types.CompanionLauraDDevonshire
        , Generated.Types.CompanionCaroline
        , Generated.Types.CompanionSuzyTheMiasma
        , Generated.Types.CompanionNorikoDuNichols
        , Generated.Types.CompanionFrancisIsaacGiovanni
        , Generated.Types.CompanionIfraAlZahra
        , Generated.Types.CompanionSariahJSnow
        , Generated.Types.CompanionClaireBelMontegra
        , Generated.Types.CompanionSylvarraAsDomonina
        , Generated.Types.CompanionMadellineLPeach
        , Generated.Types.CompanionReinaAkatsuki
        , Generated.Types.CompanionMinnieAndrus
        , Generated.Types.CompanionLucilleMBright
        , Generated.Types.CompanionKingDaemianKain
        , Generated.Types.CompanionWhisper
        , Generated.Types.CompanionRedMother
        , Generated.Types.CompanionNova
        , Generated.Types.CompanionScarlet
        , Generated.Types.CompanionHuntress
        , Generated.Types.CompanionMalice
        , Generated.Types.CompanionDiana
        , Generated.Types.CompanionCassandra
        , Generated.Types.CompanionKingCulicarius
        , Generated.Types.CompanionEinodiaKate
        , Generated.Types.CompanionPersephone
        , Generated.Types.CompanionBetildaAraiBuckland
        , Generated.Types.CompanionNichteYIr
        , Generated.Types.CompanionAelfflaedNowDaphne
        , Generated.Types.CompanionVictoriaWatts
        , Generated.Types.CompanionRichardMaxJohnson
        , Generated.Types.CompanionBethadonnaRossbaum
        , Generated.Types.CompanionMirandaQuincy
        , Generated.Types.CompanionMeganMinosine
        , Generated.Types.CompanionJaneKitAdams
        , Generated.Types.CompanionAliciaRedVelvetine
        , Generated.Types.CompanionJuliaMayCaldwin
        , Generated.Types.CompanionSamanthaNatPonds
        , Generated.Types.CompanionJenniferKYoung
        , Generated.Types.CompanionAgent7Y
        , Generated.Types.CompanionAgent9s
        , Generated.Types.CompanionTwinsSaraKara
        , Generated.Types.CompanionVesuvianelleLalahon
        , Generated.Types.CompanionAmberOgdenVix
        , Generated.Types.CompanionXINDollmaker
        , Generated.Types.CompanionAlexKHalls
        , Generated.Types.CompanionIsabellaMableOaks
        , Generated.Types.CompanionEvangelinaRosaCostaval
        , Generated.Types.CompanionPenelope
        , Generated.Types.CompanionMandyHunts
        , Generated.Types.CompanionEskhanderMahabadi
        , Generated.Types.CompanionExperiment627
        , Generated.Types.CompanionAugustRoseOBare
        , Generated.Types.CompanionOpheliaReisha
        , Generated.Types.CompanionEstherReisha
        , Generated.Types.CompanionCustom
        , Generated.Types.CompanionErisJulianariStonefallen
        , Generated.Types.CompanionTheCaretaker
        , Generated.Types.CompanionLostQueen
        , Generated.Types.CompanionGiftFromBeyond
        , Generated.Types.CompanionAgent9sOriginal
        , Generated.Types.CompanionPrincessDaelEzraOfCharis
        , Generated.Types.CompanionAnaphalonGreenwield
        , Generated.Types.CompanionBriarGracehollow
        , Generated.Types.CompanionDuchessSaelAstraOfOdalle
        , Generated.Types.CompanionXiaoLiena
        , Generated.Types.CompanionJin
        , Generated.Types.CompanionSaraStar
        , Generated.Types.CompanionRedBetty
        , Generated.Types.CompanionMayraBroadleaf
        , Generated.Types.CompanionEmiliaBroadleaf
        , Generated.Types.CompanionEdwardAshwell
        , Generated.Types.CompanionMicheleCottantial
        , Generated.Types.CompanionSaundraAllard
        , Generated.Types.CompanionBethanyDavina
        , Generated.Types.CompanionFranciscaUveros
        , Generated.Types.CompanionIssacAshwell
        , Generated.Types.CompanionElodieLylou
        , Generated.Types.CompanionAvelineGrace
        , Generated.Types.CompanionIngvildDagny
        , Generated.Types.CompanionRose
        , Generated.Types.CompanionCathrineZarina
        , Generated.Types.CompanionLennethRisea
        , Generated.Types.CompanionGratianusGrantTertullian
        , Generated.Types.CompanionNazliMojdeh
        , Generated.Types.CompanionOldArthurHadford
        , Generated.Types.CompanionHelenCorinna
        ]


questFuzzer : Fuzzer Generated.Types.Quest
questFuzzer =
    Fuzz.oneOfValues
        [ Generated.Types.QuestCartTheory
        , Generated.Types.QuestHouseFire
        , Generated.Types.QuestDomesticated
        , Generated.Types.QuestStreetRacer
        , Generated.Types.QuestPartyAnimal
        , Generated.Types.QuestGoneFeral
        , Generated.Types.QuestJustDesserts
        , Generated.Types.QuestGhostStories
        , Generated.Types.QuestOnSilverWings
        , Generated.Types.QuestOnMyMark
        , Generated.Types.QuestTheMathematician
        , Generated.Types.QuestPackTactics
        , Generated.Types.QuestDeathRacer
        , Generated.Types.QuestBrokenPoint
        , Generated.Types.QuestNecropolis
        , Generated.Types.QuestTaintedGrail
        , Generated.Types.QuestDragonHunt
        , Generated.Types.QuestSanguineLight
        , Generated.Types.QuestMadBomber
        , Generated.Types.QuestDeepWhispers
        , Generated.Types.QuestHotSprings
        , Generated.Types.QuestTournamentArc
        , Generated.Types.QuestInquisition
        , Generated.Types.QuestPrettyLittleThings
        , Generated.Types.QuestDarksideCrater
        , Generated.Types.QuestCourtship
        , Generated.Types.QuestFragmentsOfSelf
        , Generated.Types.QuestRipAndTear
        , Generated.Types.QuestStarCrypt
        , Generated.Types.QuestDungeoneering
        , Generated.Types.QuestWitchesGotTalent
        , Generated.Types.QuestFamiliarPlaydate
        , Generated.Types.QuestAdventureTime
        , Generated.Types.QuestSpecialDelivery
        , Generated.Types.QuestSightseeing
        , Generated.Types.QuestDreamTrial
        , Generated.Types.QuestPinchOfStardust
        , Generated.Types.QuestIntroductions
        , Generated.Types.QuestSpookyStories
        , Generated.Types.QuestLaundryDay
        , Generated.Types.QuestDiscountGoods
        , Generated.Types.QuestFreeMedical
        , Generated.Types.QuestTheSwimsuitEpisode
        , Generated.Types.QuestTeaParty
        , Generated.Types.QuestMoonlitSerenade
        , Generated.Types.QuestYesTeacher
        , Generated.Types.QuestLateFees
        , Generated.Types.QuestVitalLotus
        , Generated.Types.QuestDistressBeacon
        , Generated.Types.QuestFamiliarTrouble
        , Generated.Types.QuestHalloweenTown
        , Generated.Types.QuestSpaceTentacles
        , Generated.Types.QuestTimeSlip
        , Generated.Types.QuestWitchesNInquisition
        , Generated.Types.QuestMaidenTrain
        , Generated.Types.QuestBloodstoneChalice
        , Generated.Types.QuestStargazing
        , Generated.Types.QuestFaerieKingSCourt
        , Generated.Types.QuestTentacleParty
        , Generated.Types.QuestPublicService
        , Generated.Types.QuestDigitalDeath
        , Generated.Types.QuestJobBoard
        , Generated.Types.QuestSaveChristmas
        , Generated.Types.QuestCelestialCode
        , Generated.Types.QuestWonderBazaar
        ]


rankedRelicFuzzer : Fuzzer Types.RankedRelic
rankedRelicFuzzer =
    Fuzz.map2 Types.RankedRelic relicFuzzer (Fuzz.intAtLeast 0)


relicFuzzer : Fuzzer Generated.Types.Relic
relicFuzzer =
    Fuzz.oneOf
        [ Fuzz.constant Generated.Types.RelicHexVPN
        , Fuzz.constant Generated.Types.RelicStormBrew
        , Fuzz.constant Generated.Types.RelicNightlight
        , Fuzz.constant Generated.Types.RelicStainedSliver
        , Fuzz.constant Generated.Types.RelicJadeBolt
        , Fuzz.constant Generated.Types.RelicGoldenFish
        , Fuzz.constant Generated.Types.RelicNecronomicon
        , Fuzz.constant Generated.Types.RelicAlchemistStone
        , Fuzz.constant Generated.Types.RelicYagaRoot
        , Fuzz.constant Generated.Types.RelicNymphVessel
        , Fuzz.constant Generated.Types.RelicLongingMirror
        , Fuzz.constant Generated.Types.RelicHellrider
        , Fuzz.constant Generated.Types.RelicArcherSBow
        , Fuzz.constant Generated.Types.RelicAssassinSEdge
        , Fuzz.constant Generated.Types.RelicWardenSMaul
        , Fuzz.constant Generated.Types.RelicDevilSTrident
        , Fuzz.constant Generated.Types.RelicGuardianSWall
        , Fuzz.constant Generated.Types.RelicAlchemistStash
        , Fuzz.constant Generated.Types.RelicGemOfRenewal
        , Fuzz.constant Generated.Types.RelicProsthesis
        , Fuzz.constant Generated.Types.RelicVioletLenses
        , Fuzz.constant Generated.Types.RelicManaCore
        , Fuzz.constant Generated.Types.RelicMagicTalisman
        , Fuzz.constant Generated.Types.RelicTreasurerSMint
        , Fuzz.constant Generated.Types.RelicCompanionBrick
        , Fuzz.constant Generated.Types.RelicHeirloom
        , Fuzz.constant Generated.Types.RelicRiftblade
        , Fuzz.constant Generated.Types.RelicLifeRecord
        , Fuzz.constant Generated.Types.RelicServantDolls
        , Fuzz.constant Generated.Types.RelicDollmakerSKit
        , Fuzz.constant Generated.Types.RelicThaumicSpikes
        , Fuzz.constant Generated.Types.RelicSecretElixir
        , Fuzz.map Generated.Types.RelicCosmicPearl cosmicPearlDataFuzzer
        , Fuzz.constant Generated.Types.RelicWitchDeck
        , Fuzz.constant Generated.Types.RelicBattleship
        , Fuzz.constant Generated.Types.RelicMythrilArmor
        , Fuzz.constant Generated.Types.RelicRitualInks
        , Fuzz.constant Generated.Types.RelicSpellBullets
        , Fuzz.constant Generated.Types.RelicGreatWarRifle
        , Fuzz.constant Generated.Types.RelicWitchPistol
        , Fuzz.constant Generated.Types.RelicJesterOniMask
        , Fuzz.constant Generated.Types.RelicFarTalisman
        , Fuzz.constant Generated.Types.RelicMasterKey
        , Fuzz.constant Generated.Types.RelicPewterCrown
        , Fuzz.constant Generated.Types.RelicSunShard
        , Fuzz.constant Generated.Types.RelicHydron
        , Fuzz.constant Generated.Types.RelicCollection
        , Fuzz.constant Generated.Types.RelicWitchKisses
        , Fuzz.constant Generated.Types.RelicTrainerBands
        , Fuzz.constant Generated.Types.RelicBlurForm
        , Fuzz.constant Generated.Types.RelicTomeOfRestore
        , Fuzz.constant Generated.Types.RelicCursedCards
        , Fuzz.constant Generated.Types.RelicShiftingSword
        , Fuzz.constant Generated.Types.RelicPocketLance
        , Fuzz.constant Generated.Types.RelicFlyingDagger
        , Fuzz.constant Generated.Types.RelicGemBlade
        , Fuzz.constant Generated.Types.RelicMansionSeed
        , Fuzz.constant Generated.Types.RelicBookOfBlooms
        , Fuzz.constant Generated.Types.RelicClockworkCreature
        , Fuzz.constant Generated.Types.RelicPaintedWorld
        ]


cosmicPearlDataFuzzer : Fuzzer Generated.Types.CosmicPearlData
cosmicPearlDataFuzzer =
    Fuzz.map2 Generated.Types.CosmicPearlData (Fuzz.list (Fuzz.pair affinityFuzzer affinityFuzzer)) (Fuzz.list affinityFuzzer)
