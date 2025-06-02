module Data exposing (Enum, Variant, enums)

import Dict exposing (Dict)
import Dict.Extra
import Parsers


type alias Enum =
    { name : String
    , variants : Dict String Variant
    , toImage : Bool
    }


type alias Variant =
    { arguments : List String
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
                    |> withDLC "Loose Assets" looseAssets
                    |> withDLC "Elemental Harmony" elementalHarmony
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
        |> withImages
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


fromParsed : { name : String, items : List Parsers.DLCItem } -> DLC
fromParsed { name, items } =
    let
        variant : Variant
        variant =
            { arguments = []
            , toStringException = Nothing
            , dlc = Just name
            }
    in
    List.foldl
        (\item dlc ->
            case item of
                Parsers.DLCRace v ->
                    { dlc | races = Dict.insert v.name variant dlc.races }

                Parsers.DLCPerk v ->
                    { dlc | perks = Dict.insert v.name variant dlc.perks }
        )
        emptyDLC
        items


withDLC : String -> DLC -> DLC -> DLC
withDLC dlcName original additional =
    let
        merge :
            (DLC -> Dict String Variant)
            -> Dict String Variant
        merge prop =
            prop original
                |> Dict.union
                    (prop additional
                        |> Dict.map
                            (\_ variant -> { variant | dlc = Just dlcName })
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


buildEnum : String -> Dict String Variant -> Enum
buildEnum name variants =
    { name = name
    , variants = variants
    , toImage = False
    }


buildVariants : List String -> Dict String Variant
buildVariants variants =
    let
        toVariant : String -> ( String, Variant )
        toVariant variant =
            ( variant
            , { arguments = []
              , toStringException = Nothing
              , dlc = Nothing
              }
            )
    in
    variants
        |> List.map toVariant
        |> Dict.fromList


withImages : Enum -> Enum
withImages enum =
    { enum | toImage = True }


withExceptions : List ( String, String ) -> Dict String Variant -> Dict String Variant
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
    -> Dict String Variant
    -> Dict String Variant
updateDict f pairs enum =
    List.foldl
        (\( key, value ) acc ->
            Dict.Extra.updateIfExists
                key
                (f value)
                acc
        )
        enum
        pairs


withArguments : String -> List String -> Dict String Variant -> Dict String Variant
withArguments name arguments enum =
    Dict.Extra.updateIfExists name
        (\variant ->
            { variant | arguments = arguments }
        )
        enum


type alias DLC =
    { classes : Dict String Variant
    , races : Dict String Variant
    , perks : Dict String Variant
    , affinities : Dict String Variant
    , companions : Dict String Variant
    , relics : Dict String Variant
    , magics : Dict String Variant
    }


emptyDLC : DLC
emptyDLC =
    { classes = Dict.empty
    , races = Dict.empty
    , perks = Dict.empty
    , affinities = Dict.empty
    , companions = Dict.empty
    , relics = Dict.empty
    , magics = Dict.empty
    }


core : DLC
core =
    { classes = coreClasses
    , races = coreRaces
    , perks = corePerks
    , affinities = coreAffinities
    , companions = coreCompanions
    , relics = coreRelics
    , magics = coreMagics
    }


coreClasses : Dict String Variant
coreClasses =
    [ "Academic", "Sorceress", "Warlock" ]
        |> buildVariants


coreRaces : Dict String Variant
coreRaces =
    [ "Neutral", "Daeva", "Ifrit", "Siren", "Naiad", "Dryad", "Oread", "Lamia", "Aurai", "Nymph", "Gorgon", "Luxal", "Kekubi", "Sylph", "Undine", "Sprite", "Empusa", "Lilin", "Erinyes", "Hannya", "Taura", "Wulong", "Dravir", "Doll", "Vanir", "Changeling", "Elf", "Orc", "Pharon", "Jotun", "Hollow", "Dwarf", "Wither", "Mimi", "Sword" ]
        |> buildVariants
        |> withArguments "Dravir" [ "Affinity" ]


coreSizes : List String
coreSizes =
    [ "Low", "Medium", "High" ]


coreAffinities : Dict String Variant
coreAffinities =
    [ "All", "Beast", "Blood", "Body", "Earth", "Fire", "Life", "Metal", "Mind", "Nature", "Necro", "Soul", "Water", "Wind" ]
        |> buildVariants
        |> withExceptions [ ( "All", "???" ) ]


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


coreMagics : Dict String Variant
coreMagics =
    [ "Aethernautics", "Alchemy", "Consortation", "Curses", "Divination", "Earthmoving", "Familiarity", "Firecalling", "Hexes", "Metallurgy", "Metamorphosis", "Naturalism", "Necromancy", "Portals", "Psychotics", "Runes", "Waterworking", "Windkeeping", "Witchery", "Digicasting", "Wands", "Ministration", "Occultism", "Dominion", "Covenants", "Monstrosity", "Gadgetry", "Integration" ]
        |> buildVariants


corePerks : Dict String Variant
corePerks =
    [ "Oracle", "Jack-of-All", "Transformation Sequence", "Poisoner", "Witchflame", "Energized", "Conjuration", "Elephant Trunk", "Prestidigitation", "Suggestion", "Fascinate", "Pantomime", "Beauty Sleep", "Third Eye", "Soul Jellies", "Hat Trick", "Mood Weather", "Improved Familiar", "Hybridize", "Apex", "Charge Swap", "Crystallize", "Memorize", "Maid Hand", "Hot Swap", "Menagerie", "Blood Witch", "Gunwitch", "Levitation", "Isekaid", "Heritage", "Magic Friendship", "Windsong", "Broom Beast", "Isekai Worlds", "Isekai Heritage", "Summer School", "Magical Heart", "Miniaturization", "Soul Warrior", "Comfy Pocket", "Improved Rod", "Witch... hut?", "Company", "Pet Break", "Magic Shop", "Keeper", "Soul Graft" ]
        |> buildVariants
        |> withArguments "Charge Swap" [ "Race" ]


coreFactions : List String
coreFactions =
    [ "The College of Arcadia", "Hawthorne Academia", "The Watchers", "The Hespatian Coven", "Lunabella", "Alfheimr Alliance", "The Outsiders", "The O.R.C.", "Alphazon Industries" ]


coreCompanions : Dict String Variant
coreCompanions =
    [ "Rachel Pool", "Anne Laurenchi", "Canday Wesbank", "Tessa-Marie Kudashov", "Evelynn P. Willowcrane", "John Doe", "Hannah Grangely", "Elizabell Sinclaire", "Ashley Lovenko", "Sylvanne Mae Kanzaki", "Francis Isaac Giovanni", "Ifra al-Zahra", "Sariah J. Snow", "Claire Bel’montegra", "Lucille M. Bright", "King Daemian Kain", "Whisper", "Red Mother", "Diana", "Cassandra", "King Culicarius", "Einodia - Kate", "Victoria Watts", "Richard Max Johnson", "Bethadonna Rossbaum", "Miranda Quincy", "Samantha Nat Ponds", "Jennifer K. Young", "Agent 7Y", "Agent 9s", "Alex K. Halls", "Isabella Mable Oaks", "Evangelina Rosa Costaval", "Penelope", "The Caretaker", "Lost Queen", "Gift from Beyond", "Agent 9s (Original)", "Princess Dael’ezra of Charis", "Anaphalon Greenwield", "Briar Gracehollow", "Duchess Sael’astra of Odalle", "Mandy Hunts", "Eskhander Mahabadi", "Experiment 627", "August Rose o’Bare", "Saya Kurosawa", "Francesca Astrenichtys", "Elaine A. Victorica", "Maimonada Majesteim", "Azurelliea Ad’Madelline", "Melissa Vincimvitch", "Laura D. Devonshire", "Caroline", "Suzy the Miasma", "Noriko du Nichols", "Sylvarra as’Domonina", "Madelline L. Peach", "Reina Akatsuki", "Minnie Andrus", "Nova", "Scarlet", "Huntress", "Malice", "Persephone", "Betilda Arai Buckland", "Nichte Y’ir", "Ælfflæd (now Daphne)", "Megan Minosine", "Jane “Kit” Adams", "Alicia Red Velvetine", "Julia May Caldwin", "Twins Sara & Kara", "Vesuvianelle Lalahon", "Amber Ogden “Vix”", "XIN: Dollmaker", "Ophelia Reisha", "Esther Reisha", "Custom", "Eris Julianari Stonefallen" ]
        |> buildVariants


coreRelics : Dict String Variant
coreRelics =
    [ "HexVPN", "Storm Brew", "Nightlight", "Stained Sliver", "Jade Bolt", "Golden Fish", "Necronomicon", "Alchemist Stone", "Yaga Root", "Nymph Vessel", "Longing Mirror", "Hellrider", "Archer’s Bow", "Assassin’s edge", "Warden’s Maul", "Devil’s Trident", "Guardian’s Wall", "Alchemist Stash", "Gem of Renewal", "Prosthesis", "Violet Lenses", "Mana Core", "Magic Talisman", "Treasurer’s Mint", "Companion Brick", "Heirloom", "Riftblade", "Life Record", "Servant Dolls", "Dollmaker’s Kit", "Thaumic Spikes", "Secret Elixir", "Cosmic Pearl", "Witch Deck", "Battleship", "Mythril Armor", "Ritual Inks", "Spell Bullets", "Great War Rifle", "Witch Pistol", "Jester Oni Mask", "Far Talisman", "Master Key", "Pewter Crown", "Sun Shard", "Hydron", "Collection", "Witch Kisses" ]
        -- |> withArguments "Magic Talisman" [ "Magic" ]
        |> buildVariants


looseAssets : DLC
looseAssets =
    { emptyDLC
        | races = looseAssetsRaces
        , companions = looseAssetsCompanions
        , magics = looseAssetsMagics
    }


looseAssetsRaces : Dict String Variant
looseAssetsRaces =
    [ "Xeno", "Cyborg", "Spider", "Gnome", "Pixie", "Fairy", "Genie", "Gemini" ]
        |> buildVariants
        |> withArguments "Genie" [ "Affinity" ]
        |> withArguments "Gemini" [ "Affinity" ]


looseAssetsCompanions : Dict String Variant
looseAssetsCompanions =
    [ "Xiao Liena", "\"Jin\" [Choose a Name]", "Sara Star", "Red Betty" ]
        |> buildVariants
        |> withExceptions [ ( "Xiao Liena", "Xiao Liena 肖列娜" ) ]


looseAssetsMagics : Dict String Variant
looseAssetsMagics =
    [ "Lifeweaving", "Visceramancy", "Arachnescence" ]
        |> buildVariants


elementalHarmony : DLC
elementalHarmony =
    { emptyDLC
        | races = elementalHarmonyRaces
    }


elementalHarmonyRaces : Dict String Variant
elementalHarmonyRaces =
    [ "Moorwalker", "Phantasm", "Golem", "Muspel", "Dictum", "Qareen", "Rusalka", "Lares", "Firebird", "Fresco", "Silverstream", "Revenant", "Petrichor" ]
        |> buildVariants
