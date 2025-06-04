module Data exposing (Enum, Variant, enums)

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


enums : List Parsers.DLC -> List Enum
enums parsedDLCs =
    let
        combinedDLC : DLC
        combinedDLC =
            List.foldl (\e acc -> acc |> withDLC e.name (fromParsed e))
                (core
                    |> withDLC (Just "Loose Assets") looseAssets
                )
                parsedDLCs
    in
    [ buildEnum "Class" combinedDLC.classes
        |> withImages
    , buildEnum "Race" combinedDLC.races
        |> withImages
    , buildEnum "Perk" combinedDLC.perks
        |> withImages
    , buildEnum "Affinity" combinedDLC.affinities
    , buildEnum "Companion" combinedDLC.companions
        |> withImages
    , buildEnum "Relic" combinedDLC.relics
        |> withImages
    , buildEnum "Magic" combinedDLC.magics
        |> withImages

    --
    , buildEnum "Size" (buildVariants coreSizes)
    , buildEnum "ComplicationCategory" (buildVariants coreComplicationCategories)
    , buildEnum "Complication" (buildVariants coreComplications)
        |> withImages
    , buildEnum "GameMode" (buildVariants coreGameModes)
        |> withImages
    , buildEnum "Slot" (buildVariants coreSlots)
        |> withImages
    , buildEnum "Faction" (buildVariants coreFactions)
    ]


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
                Parsers.DLCRace v ->
                    { dlc | races = variant v.name :: dlc.races }

                Parsers.DLCPerk v ->
                    { dlc | perks = variant v.name :: dlc.perks }

                Parsers.DLCMagic v ->
                    { dlc | magics = variant v.name :: dlc.magics }

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

                Parsers.DLCRelic v ->
                    { dlc | relics = variant v.name :: dlc.relics }

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
    { classes = merge .classes
    , races = merge .races
    , perks = merge .perks
    , affinities = merge .affinities
    , companions = merge .companions
    , relics = merge .relics
    , magics = merge .magics
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


withExceptions : List ( String, String ) -> List Variant -> List Variant
withExceptions exceptions enum =
    updateDict
        (\value variant ->
            { variant | toStringException = Just value }
        )
        exceptions
        enum


updateDict :
    (a -> Variant -> Variant)
    -> List ( String, a )
    -> List Variant
    -> List Variant
updateDict f pairs enum =
    List.foldl
        (\( key, value ) acc ->
            List.Extra.updateIf
                (\variant -> variant.name == key)
                (f value)
                acc
        )
        enum
        pairs


withArguments : String -> List String -> List Variant -> List Variant
withArguments name arguments enum =
    List.Extra.updateIf
        (\variant -> variant.name == name)
        (\variant ->
            { variant | arguments = arguments }
        )
        enum


type alias DLC =
    { classes : List Variant
    , races : List Variant
    , perks : List Variant
    , affinities : List Variant
    , companions : List Variant
    , relics : List Variant
    , magics : List Variant
    }


emptyDLC : DLC
emptyDLC =
    { classes = []
    , races = []
    , perks = []
    , affinities = []
    , companions = []
    , relics = []
    , magics = []
    }


core : DLC
core =
    { classes = coreClasses
    , races = coreRaces
    , perks = corePerks
    , affinities = []
    , companions = []
    , relics = []
    , magics = coreMagics
    }


coreClasses : List Variant
coreClasses =
    [ "Academic", "Sorceress", "Warlock" ]
        |> buildVariants


coreRaces : List Variant
coreRaces =
    [ "Dravir" ]
        |> buildVariants
        |> withArguments "Dravir" [ "Affinity" ]


coreSizes : List String
coreSizes =
    [ "Low", "Medium", "High" ]


coreComplicationCategories : List String
coreComplicationCategories =
    [ "WorldShift" ]


coreComplications : List String
coreComplications =
    [ "Brutality", "Masquerade", "True Names", "Monsters", "Population", "*Bonk*", "Dysfunction", "Vulnerability", "Rejection", "Crutch", "Restriction", "Hunted", "Dislikeable", "Monster Bait", "Black Swan", "Spell Sink", "Like a duck", "Like a rock", "Eye Catcher", "Silly Goose", "Hard Lessons", "Cold Heart", "Hideous", "Witch Mark", "Nemesis", "Addiction", "Sensory Disability", "Physical Disability", "Sensory Shock", "Adoring Fan", "Very Dere", "Requirement", "Unveiled", "Nightmares", "Kryptonite", "Fit Witch", "Branded", "No Privacy", "Blood Feud", "Marked", "Defeated", "Fixation", "All Natural", "Witchknight", "Inadequacy", "Dysphoria", "Betrayal", "Compulsion" ]


coreGameModes : List String
coreGameModes =
    [ "Story Arc", "Early Bird", "Skill Tree", "Constellation" ]


coreSlots : List String
coreSlots =
    [ "White", "Folk", "Noble", "Heroic", "Epic" ]


coreMagics : List Variant
coreMagics =
    [ "Digicasting", "Wands", "Ministration", "Occultism", "Dominion", "Covenants", "Monstrosity", "Gadgetry", "Integration" ]
        |> buildVariants


corePerks : List Variant
corePerks =
    [ "Hybridize", "Charge Swap" ]
        |> buildVariants
        |> withArguments "Charge Swap" [ "Race" ]


coreFactions : List String
coreFactions =
    [ "The College of Arcadia", "Hawthorne Academia", "The Watchers", "The Hespatian Coven", "Lunabella", "Alfheimr Alliance", "The Outsiders", "The O.R.C.", "Alphazon Industries" ]


looseAssets : DLC
looseAssets =
    { emptyDLC
        | races = looseAssetsRaces
    }


looseAssetsRaces : List Variant
looseAssetsRaces =
    [ "Genie", "Gemini" ]
        |> buildVariants
        |> withArguments "Genie" [ "Affinity" ]
        |> withArguments "Gemini" [ "Affinity" ]
