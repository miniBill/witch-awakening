module Data exposing (Enum, enums)


type alias Enum =
    { name : String
    , variants : List ( String, List String )
    , exceptions : List ( String, String )
    , toImage : Bool
    }


enums : List Enum
enums =
    [ build "Class" classes |> withImages
    , build "Race" races
        |> withImages
        |> withArguments "Dravir" [ "Affinity" ]
        |> withArguments "Genie" [ "Affinity" ]
        |> withArguments "Gemini" [ "Affinity" ]
    , build "Size" sizes
    , build "Affinity" affinities
        |> withImages
        |> withExceptions [ ( "All", "???" ) ]
    , build "ComplicationCategory" [ "WorldShift" ]
    , build "Complication" complications |> withImages
    , build "GameMode" gameModes |> withImages
    , build "Slot" slots |> withImages
    , build "Magic" magics |> withImages
    , build "Perk" perks |> withImages
    , build "Faction" factions
    , build "Companion" companions
        |> withImages
        |> withExceptions [ ( "Xiao Liena", "Xiao Liena 肖列娜" ) ]
    , build "Relic" relics |> withImages
    ]


build : String -> List String -> Enum
build name variants =
    { name = name
    , variants = List.map (\variant -> ( variant, [] )) variants
    , exceptions = []
    , toImage = False
    }


withImages : Enum -> Enum
withImages enum =
    { enum | toImage = True }


withExceptions : List ( String, String ) -> Enum -> Enum
withExceptions exceptions enum =
    { enum | exceptions = exceptions }


withArguments : String -> List String -> Enum -> Enum
withArguments name arguments enum =
    { enum
        | variants =
            List.map
                (\( variant, orig ) ->
                    if variant == name then
                        ( variant, arguments )

                    else
                        ( variant, orig )
                )
                enum.variants
    }


classes : List String
classes =
    [ "Academic", "Sorceress", "Warlock" ]


races : List String
races =
    [ "Neutral", "Daeva", "Ifrit", "Siren", "Naiad", "Dryad", "Oread", "Lamia", "Aurai", "Nymph", "Gorgon", "Luxal", "Kekubi", "Sylph", "Undine", "Sprite", "Empusa", "Lilin", "Erinyes", "Hannya", "Taura", "Wulong", "Dravir", "Doll", "Vanir", "Changeling", "Elf", "Orc", "Pharon", "Jotun", "Hollow", "Dwarf", "Wither", "Mimi", "Sword", "Xeno", "Cyborg", "Spider", "Gnome", "Pixie", "Fairy", "Genie", "Gemini" ]


sizes : List String
sizes =
    [ "Low", "Med", "High" ]


affinities : List String
affinities =
    [ "All", "Beast", "Blood", "Body", "Earth", "Fire", "Life", "Metal", "Mind", "Nature", "Necro", "Soul", "Water", "Wind" ]


complications : List String
complications =
    [ "Brutality", "Masquerade", "True Names", "Monsters", "Population", "*Bonk*", "Dysfunction", "Vulnerability", "Rejection", "Crutch", "Restriction", "Hunted", "Dislikeable", "Monster Bait", "Black Swan", "Spell Sink", "Like a duck", "Like a rock", "Eye Catcher", "Silly Goose", "Hard Lessons", "Cold Heart", "Hideous", "Witch Mark", "Nemesis", "Addiction", "Sensory Disability", "Physical Disability", "Sensory Shock", "Adoring Fan", "Very Dere", "Requirement", "Unveiled", "Nightmares", "Kryptonite", "Fit Witch", "Branded", "No Privacy", "Blood Feud", "Marked", "Defeated", "Fixation", "All Natural", "Witchknight", "Inadequacy", "Dysphoria", "Betrayal", "Compulsion" ]


gameModes : List String
gameModes =
    [ "Story Arc", "Early Bird", "Skill Tree", "Constellation" ]


slots : List String
slots =
    [ "White", "Folk", "Noble", "Heroic", "Epic" ]


magics : List String
magics =
    [ "Aethernautics", "Alchemy", "Consortation", "Curses", "Divination", "Earthmoving", "Familiarity", "Firecalling", "Hexes", "Metallurgy", "Metamorphosis", "Naturalism", "Necromancy", "Portals", "Psychotics", "Runes", "Waterworking", "Windkeeping", "Witchery", "Digicasting", "Wands", "Ministration", "Occultism", "Dominion", "Covenants", "Monstrosity", "Gadgetry", "Integration", "Lifeweaving", "Visceramancy", "Arachnescence" ]


perks : List String
perks =
    [ "Oracle", "Jack-of-All", "Transformation Sequence", "Poisoner", "Witchflame", "Energized", "Conjuration", "Elephant Trunk", "Prestidigitation", "Suggestion", "Fascinate", "Pantomime", "Beauty Sleep", "Third Eye", "Soul Jellies", "Hat Trick", "Mood Weather", "Improved Familiar", "Hybridize", "Apex", "Charge Swap", "Crystallize", "Memorize", "Maid Hand", "Hot Swap", "Menagerie", "Blood Witch", "Gunwitch", "Levitation", "Isekaid", "Heritage", "Magic Friendship", "Windsong", "Broom Beast", "Isekai Worlds", "Isekai Heritage", "Summer School", "Magical Heart", "Miniaturization", "Soul Warrior", "Comfy Pocket", "Improved Rod", "Witch... hut?", "Company", "Pet Break", "Magic Shop", "Keeper", "Soul Graft" ]


factions : List String
factions =
    [ "The College of Arcadia", "Hawthorne Academia", "The Watchers", "The Hespatian Coven", "Lunabella", "Alfheimr Alliance", "The Outsiders", "The O.R.C.", "Alphazon Industries" ]


companions : List String
companions =
    [ "Rachel Pool", "Anne Laurenchi", "Canday Wesbank", "Tessa-Marie Kudashov", "Evelynn P. Willowcrane", "John Doe", "Hannah Grangely", "Elizabell Sinclaire", "Ashley Lovenko", "Sylvanne Mae Kanzaki", "Francis Isaac Giovanni", "Ifra al-Zahra", "Sariah J. Snow", "Claire Bel’montegra", "Lucille M. Bright", "King Daemian Kain", "Whisper", "Red Mother", "Diana", "Cassandra", "King Culicarius", "Einodia - Kate", "Victoria Watts", "Richard Max Johnson", "Bethadonna Rossbaum", "Miranda Quincy", "Samantha Nat Ponds", "Jennifer K. Young", "Agent 7Y", "Agent 9s", "Alex K. Halls", "Isabella Mable Oaks", "Evangelina Rosa Costaval", "Penelope", "The Caretaker", "Lost Queen", "Gift from Beyond", "Agent 9s (Original)", "Princess Dael’ezra of Charis", "Anaphalon Greenwield", "Briar Gracehollow", "Duchess Sael’astra of Odalle", "Mandy Hunts", "Eskhander Mahabadi", "Experiment 627", "August Rose o’Bare", "Saya Kurosawa", "Francesca Astrenichtys", "Elaine A. Victorica", "Maimonada Majesteim", "Azurelliea Ad’Madelline", "Melissa Vincimvitch", "Laura D. Devonshire", "Caroline", "Suzy the Miasma", "Noriko du Nichols", "Sylvarra as’Domonina", "Madelline L. Peach", "Reina Akatsuki", "Minnie Andrus", "Nova", "Scarlet", "Huntress", "Malice", "Persephone", "Betilda Arai Buckland", "Nichte Y’ir", "Ælfflæd (now Daphne)", "Megan Minosine", "Jane “Kit” Adams", "Alicia Red Velvetine", "Julia May Caldwin", "Twins Sara & Kara", "Vesuvianelle Lalahon", "Amber Ogden “Vix”", "XIN: Dollmaker", "Ophelia Reisha", "Esther Reisha", "Custom", "Eris Julianari Stonefallen", "Xiao Liena", "\"Jin\" [Choose a Name]", "Sara Star", "Red Betty" ]


relics : List String
relics =
    [ "HexVPN", "Storm Brew", "Nightlight", "Stained Sliver", "Jade Bolt", "Golden Fish", "Necronomicon", "Alchemist Stone", "Yaga Root", "Nymph Vessel", "Longing Mirror", "Hellrider", "Archer’s Bow", "Assassin’s edge", "Warden’s Maul", "Devil’s Trident", "Guardian’s Wall", "Alchemist Stash", "Gem of Renewal", "Prosthesis", "Violet Lenses", "Mana Core", "Magic Talisman", "Treasurer’s Mint", "Companion Brick", "Heirloom", "Riftblade", "Life Record", "Servant Dolls", "Dollmaker’s Kit", "Thaumic Spikes", "Secret Elixir", "Cosmic Pearl", "Witch Deck", "Battleship", "Mythril Armor", "Ritual Inks", "Spell Bullets", "Great War Rifle", "Witch Pistol", "Jester Oni Mask", "Far Talisman", "Master Key", "Pewter Crown", "Sun Shard", "Hydron", "Collection", "Witch Kisses" ]
