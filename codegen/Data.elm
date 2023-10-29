module Data exposing (Enum, enums)


type alias Enum =
    { name : String
    , variants : List String
    , exceptions : List ( String, String )
    , toImage : Bool
    }


enums : List Enum
enums =
    [ enumWithImages "Class" classes
    , enumWithImages "Race" races
    , enumWithoutImages "Size" sizes
    , enumWithExceptions "Affinity" affinities [ ( "All", "???" ) ] True
    , enumWithoutImages "ComplicationCategory" [ "WorldShift" ]
    , enumWithImages "Complication" complications
    , enumWithImages "GameMode" gameModes
    , enumWithImages "Slot" slots
    , enumWithImages "Magic" magics
    , enumWithImages "Perk" perks
    , enumWithoutImages "Faction" factions
    , enumWithImages "Companion" companions
    ]


enumWithExceptions : String -> List String -> List ( String, String ) -> Bool -> Enum
enumWithExceptions name variants exceptions toImage =
    { name = name
    , variants = variants
    , exceptions = exceptions
    , toImage = toImage
    }


enumWithoutImages : String -> List String -> Enum
enumWithoutImages name variants =
    { name = name
    , variants = variants
    , exceptions = []
    , toImage = False
    }


enumWithImages : String -> List String -> Enum
enumWithImages name variants =
    { name = name
    , variants = variants
    , exceptions = []
    , toImage = True
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
    [ "Aethernautics", "Alchemy", "Consortation", "Curses", "Divination", "Earthmoving", "Familiarity", "Firecalling", "Hexes", "Metallurgy", "Metamorphosis", "Naturalism", "Necromancy", "Portals", "Psychotics", "Runes", "Waterworking", "Windkeeping", "Witchery", "Digicasting", "Wands", "Ministration", "Occultism", "Dominion", "Covenants", "Monstrosity", "Gadgetry", "Integration" ]


perks : List String
perks =
    [ "Oracle", "Jack-of-All", "Transformation Sequence", "Poisoner", "Witchflame", "Energized", "Conjuration", "Elephant Trunk", "Prestidigitation", "Suggestion", "Fascinate", "Pantomime", "Beauty Sleep", "Third Eye", "Soul Jellies", "Hat Trick", "Mood Weather", "Improved Familiar", "Hybridize", "Apex", "Charge Swap", "Crystallize", "Memorize", "Maid Hand", "Hot Swap", "Menagerie", "Blood Witch", "Gunwitch", "Levitation", "Isekaid", "Heritage", "Magic Friendship", "Windsong", "Broom Beast", "Isekai Worlds", "Isekai Heritage", "Summer School", "Magical Heart", "Miniaturization", "Soul Warrior", "Comfy Pocket", "Improved Rod", "Witch... hut?", "Company", "Pet Break", "Magic Shop", "Keeper", "Soul Graft" ]


factions : List String
factions =
    [ "The College of Arcadia", "Hawthorne Academia", "The Watchers", "The Hespatian Coven", "Lunabella", "Alfheimr Alliance", "The Outsiders", "The O.R.C.", "Alphazon Industries" ]


companions : List String
companions =
    [ "Rachel Pool", "Anne Laurenchi", "Canday Wesbank", "Tessa-Marie Kudashov", "Evelynn P. Willowcrane", "John Doe", "Hannah Grangely", "Elizabell Sinclaire", "Ashley Lovenko", "Sylvanne Mae Kanzaki", "Francis Isaac Giovanni", "Ifra al-Zahra", "Sariah J. Snow", "Claire Bel’montegra", "Lucille M. Bright", "King Daemian Kain", "Whisper", "Red Mother", "Diana", "Cassandra", "King Culicarius", "Einodia - Kate", "Victoria Watts", "Richard Max Johnson", "Bethadonna Rossbaum", "Miranda Quincy", "Samantha Nat Ponds", "Jennifer K. Young", "Agent 7Y", "Agent 9s", "Alex K. Halls", "Isabella Mable Oaks", "Evangelina Rosa Costaval", "Penelope", "The Caretaker", "Lost Queen", "Gift from Beyond", "Agent 9s (Original)", "Princess Dael’ezra of Charis", "Anaphalon Greenwield", "Briar Gracehollow", "Duchess Sael’astra of Odalle", "Mandy Hunts", "Eskhander Mahabadi", "Experiment 627", "August Rose o’Bare", "Saya Kurosawa", "Francesca Astrenichtys", "Elaine A. Victorica", "Maimonada Majesteim", "Azurelliea Ad’Madelline", "Melissa Vincimvitch", "Laura D. Devonshire", "Caroline", "Suzy the Miasma", "Noriko du Nichols", "Sylvarra as’Domonina", "Madelline L. Peach", "Reina Akatsuki", "Minnie Andrus", "Nova", "Scarlet", "Huntress", "Malice" ]
