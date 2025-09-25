module Data.GameMode exposing (intro, slotDescription)


slotDescription : String
slotDescription =
    """
    Slot modes ignore the requirement of 3 ➡️ 4 ➡️ 5 for acquiring magics. A slot is a slot and stands on its own, and is meant to be simplified

    Slots mode is quite simplified.

    - (folk) {folk *FOLK*} slots can be used to obtain anything in the cyoa marked with the blue slot token.
    - (noble) {noble *NOBLE*} slots are a green token.
    - (heroic) {heroic *HEROIC*} slots use a gold token.
    - (epic) {epic *EPIC*} slots use a purple token.
    - (white) {white White} tokens are for variable options granting or requiring slots of a custom value based on the Power granted or required of the option in question, can use multiple slots per individual segments of a variable option, like each 4p class of Summer School.

    *Folk* = 1-4p. *Noble* = 5-8p. *Heroic* = 9-12p. *Epic* = 13+.

    You can also use this to break down any slot gains from sources that may have given you power later on.

    _*Complications*_ are marked with a dot of the type that complication grants.

    In _Constellation_ mode, complication dots can be stacked or broken down like Skill Tree is capable of doing.

    In _Skill Tree mode_, you can reserve the slots from complications for later use.

    *Options with a cost of 0* or less that would normally be free as a result of your class, can be treated as a white token “Free” slot.
    """


intro : String
intro =
    """
    {choice You can only choose one game mode, or none to play the default way. Each supports a different type of player, and what you might want out of it.}
    """
