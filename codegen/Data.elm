module Data exposing (Enum, Enums, Variant, enums)

import List.Extra
import Parsers


type alias Enum =
    { name : String
    , variants : List Variant
    , toImage : Bool
    }


type alias Variant =
    { name : String
    , arguments : List String
    , dlc : Maybe String
    , toStringException : Maybe String
    }


type alias Enums =
    { affinity : Enum
    , class : Enum
    , faction : Enum
    , race : Enum
    , relic : Enum
    , others : List Enum
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
    , class = buildEnum "Class" combinedDLC.classes |> withImages
    , faction = buildEnum "Faction" combinedDLC.factions
    , race = buildEnum "Race" combinedDLC.races |> withImages
    , relic = buildEnum "Relic" combinedDLC.relics |> withImages
    , others =
        [ buildEnum "Perk" combinedDLC.perks |> withImages
        , buildEnum "Companion" combinedDLC.companions |> withImages
        , buildEnum "Magic" combinedDLC.magics |> withImages
        , buildEnum "Complication" combinedDLC.complications |> withImages
        , buildEnum "ComplicationCategory" combinedDLC.complicationCategories

        --
        , buildEnum "Size" (buildVariants coreSizes)
        , buildEnum "GameMode" (buildVariants coreGameModes) |> withImages
        , buildEnum "Slot" (buildVariants coreSlots) |> withImages
        ]
    }


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
                    { dlc | perks = variant v.name :: dlc.perks }

                Parsers.DLCRace v ->
                    let
                        race : Variant
                        race =
                            case v.elements of
                                [ _ ] ->
                                    { name = v.name
                                    , arguments = [ "Affinity" ]
                                    , toStringException = Nothing
                                    , dlc = name
                                    }

                                _ ->
                                    variant v.name
                    in
                    { dlc | races = race :: dlc.races }

                Parsers.DLCRelic v ->
                    { dlc | relics = variant v.name :: dlc.relics }

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
    , companions = merge .companions
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


withArguments : String -> List String -> List Variant -> List Variant
withArguments name arguments enum =
    List.Extra.updateIf
        (\variant -> variant.name == name)
        (\variant ->
            { variant | arguments = arguments }
        )
        enum


type alias DLC =
    { affinities : List Variant
    , classes : List Variant
    , companions : List Variant
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
    , companions = []
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
        | perks = corePerks
    }


coreSizes : List String
coreSizes =
    [ "Low", "Medium", "High" ]


coreGameModes : List String
coreGameModes =
    [ "Story Arc", "Early Bird", "Skill Tree", "Constellation" ]


coreSlots : List String
coreSlots =
    [ "White", "Folk", "Noble", "Heroic", "Epic" ]


corePerks : List Variant
corePerks =
    [ "Charge Swap" ]
        |> buildVariants
        |> withArguments "Charge Swap" [ "Race" ]
