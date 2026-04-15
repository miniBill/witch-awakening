module Data exposing (Enums, enums)

import Generate.Enum exposing (Argument(..), Enum, Variant)
import Parsers


type alias Enums =
    { affinity : Enum
    , class : Enum
    , companion : Enum
    , complication : Enum
    , complicationCategory : Enum
    , faction : Enum
    , gameMode : Enum
    , magic : Enum
    , perk : Enum
    , quest : Enum
    , race : Enum
    , relic : Enum
    , size : Enum
    , slot : Enum
    }


enums : List Parsers.DLC -> Enums
enums parsedDLCs =
    let
        combinedDLC : DLC
        combinedDLC =
            List.foldl (\e acc -> acc |> withDLC e.name (fromParsed e))
                core
                parsedDLCs
    in
    { affinity = buildEnum "Affinity" combinedDLC.affinities
    , class =
        buildEnum "Class" combinedDLC.classes
            |> withImages
    , companion =
        buildEnum "Companion" combinedDLC.companions
            |> withImages
            |> withIsSame
    , complication =
        buildEnum "Complication" combinedDLC.complications
            |> withImages
    , complicationCategory =
        buildEnum "ComplicationCategory" combinedDLC.complicationCategories
    , faction =
        buildEnum "Faction" combinedDLC.factions
    , gameMode =
        buildEnum "GameMode" combinedDLC.gameModes
            |> withImages
    , magic =
        buildEnum "Magic" combinedDLC.magics
            |> withImages
    , perk =
        buildEnum "Perk" combinedDLC.perks
            |> withImages
            |> withIsSame
    , quest =
        buildEnum "Quest" combinedDLC.quests
            |> withImages
            |> withIsSame
    , race =
        buildEnum "Race" combinedDLC.races
            |> withImages
            |> withIsSame
    , relic =
        buildEnum "Relic" combinedDLC.relics
            |> withImages
            |> withIsSame
    , size = buildEnum "Size" (buildVariants sizes)
    , slot =
        buildEnum "Slot" (buildVariants slots)
            |> withImages
    }


withIsSame : Enum -> Enum
withIsSame enum =
    { enum | isSame = True }


fromParsed : Parsers.DLC -> DLC
fromParsed { name, items } =
    let
        variant : String -> Variant
        variant variantName =
            { name = variantName
            , arguments = []
            , toStringException = Nothing
            , dlc = name
            }
    in
    List.foldr
        (\item dlc ->
            case item of
                Parsers.DLCAffinity v ->
                    { dlc
                        | affinities =
                            { name = v.name
                            , arguments = []
                            , dlc = name
                            , toStringException = v.symbol
                            }
                                :: dlc.affinities
                    }

                Parsers.DLCClass v ->
                    { dlc | classes = variant v.name :: dlc.classes }

                Parsers.DLCGameMode v ->
                    { dlc | gameModes = variant v.name :: dlc.gameModes }

                Parsers.DLCCompanion v ->
                    { dlc
                        | companions =
                            { name = v.name
                            , arguments = []
                            , dlc = name
                            , toStringException = v.fullName
                            }
                                :: dlc.companions
                    }

                Parsers.DLCQuest v ->
                    { dlc | quests = variant v.name :: dlc.quests }

                Parsers.DLCComplication v ->
                    { dlc
                        | complications = variant v.name :: dlc.complications
                        , complicationCategories =
                            case v.category of
                                Just category ->
                                    if List.member (variant category) dlc.complicationCategories then
                                        dlc.complicationCategories

                                    else
                                        variant category :: dlc.complicationCategories

                                Nothing ->
                                    dlc.complicationCategories
                    }

                Parsers.DLCMagic v ->
                    { dlc | magics = variant v.name :: dlc.magics }

                Parsers.DLCPerk v ->
                    let
                        perk : Variant
                        perk =
                            { name = v.name
                            , arguments = v.arguments
                            , toStringException = Nothing
                            , dlc = name
                            }
                    in
                    { dlc | perks = perk :: dlc.perks }

                Parsers.DLCRace v ->
                    let
                        race : Variant
                        race =
                            { name = v.name
                            , arguments = List.repeat (2 - List.length v.elements) (ValueArgument "Affinity")
                            , toStringException = Nothing
                            , dlc = name
                            }
                    in
                    { dlc | races = race :: dlc.races }

                Parsers.DLCRelic v ->
                    let
                        relic : Variant
                        relic =
                            { name = v.name
                            , arguments = v.arguments
                            , toStringException = Nothing
                            , dlc = name
                            }
                    in
                    { dlc | relics = relic :: dlc.relics }

                Parsers.DLCFaction v ->
                    { dlc | factions = variant v.name :: dlc.factions }
        )
        emptyDLC
        items


withDLC : Maybe String -> DLC -> DLC -> DLC
withDLC dlcName original additional =
    let
        merge :
            (DLC -> List Variant)
            -> List Variant
        merge prop =
            prop original
                ++ (prop additional
                        |> List.map
                            (\variant -> { variant | dlc = dlcName })
                   )
    in
    { affinities = merge .affinities
    , classes = merge .classes
    , gameModes = merge .gameModes
    , companions = merge .companions
    , quests = merge .quests
    , complicationCategories = merge .complicationCategories
    , complications = merge .complications
    , factions = merge .factions
    , magics = merge .magics
    , perks = merge .perks
    , races = merge .races
    , relics = merge .relics
    }


buildEnum : String -> List Variant -> Enum
buildEnum name variants =
    { name = name
    , variants = variants
    , toImage = False
    , isSame = False
    }


buildVariants : List String -> List Variant
buildVariants variants =
    let
        toVariant : String -> Variant
        toVariant variant =
            { name = variant
            , arguments = []
            , toStringException = Nothing
            , dlc = Nothing
            }
    in
    List.map toVariant variants


withImages : Enum -> Enum
withImages enum =
    { enum | toImage = True }


type alias DLC =
    { affinities : List Variant
    , classes : List Variant
    , gameModes : List Variant
    , companions : List Variant
    , quests : List Variant
    , complicationCategories : List Variant
    , complications : List Variant
    , factions : List Variant
    , magics : List Variant
    , perks : List Variant
    , races : List Variant
    , relics : List Variant
    }


emptyDLC : DLC
emptyDLC =
    { affinities = []
    , classes = []
    , gameModes = []
    , companions = []
    , quests = []
    , complicationCategories = []
    , complications = []
    , factions = []
    , magics = []
    , perks = []
    , races = []
    , relics = []
    }


core : DLC
core =
    { emptyDLC
        | factions = [ "Independents" ] |> buildVariants
    }


sizes : List String
sizes =
    [ "Low", "Medium", "High" ]


slots : List String
slots =
    [ "White", "Folk", "Noble", "Heroic", "Epic" ]
