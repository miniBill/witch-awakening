module Data.GameMode exposing (Details, all, intro, slotDescription)

import Generated.Types exposing (GameMode(..))


type alias Details =
    { name : GameMode
    , content : String
    }


all : List Details
all =
    [ storyArc, earlyBird, skillTree, constellation ]


storyArc : Details
storyArc =
    { name = StoryArc
    , content =
        """
        Your Starting Power is reduced to 10 points, which cannot be increased with complications. Instead, your Power Cap is increased to [150], and Complications can increase your Power Cap by up to +60.

        You cannot start with a magic at more than Rank 3 or perks worth more than 6p before discounts, but they unlock at milestones.

        - *50 Power*, you can increase magics to rank 4 and gain perks up to 12p.
        - *At 100 Power*, you can increase magics to rank 5, with no perk cost cap

        This means that in your build you'd incorporate Quests and mention the passage of time with the quests to include your growth, and what you buy along the way, as you rise in power over time.
        """
    }


earlyBird : Details
earlyBird =
    { name = EarlyBird
    , content =
        """
        Incompatible with _Story Arc_. Rather than start less and end more, you get it all over with from the get go.

        Your Power Cap matches your Starting Power, but your *Starting Power is increased to [75]*. This means that what you start with is what you get, only acquiring companions and relics via quests later on. Simplifies things greatly, removing growth methods. _You can still benefit from *complications*_ increasing starting power, so up to _+30p_, but not even wishes will grant you any additional power points once you start.

        You can store power for later use if you so wish to do so, to simulate natural growth at your own rate, though you can do that after buying too.
        """
    }


skillTree : Details
skillTree =
    { name = SkillTree
    , content =
        """
        You abandon your Power all together. You instead rely on SLOTS: _*Mode-Arc*_.

        You begin with one _folk_ magic slot & 1 *folk* slot for a perk. You also gain 3 folk slots based on class. *Academics* use these for Magic. *Sorceresses* use these for Perks. *Warlocks* use these for Relics After you've completed 4 quests, your starting magic & perk slot increase to *Noble*. After 4 more, they become *Heroic*. After 3 more, they become *Epic*. Your 3 class slots lag behind by 4 tier becoming Heroic when you gain Epic for your starters. You gain an extra slot every time you complete a quest equal to that quest's tier. Skill Tree mode users can spend 3 slots of a lower tier for 1 of a higher tier, or break 1 higher tier slot into 2 lower tier slots.
        """
    }


constellation : Details
constellation =
    { name = Constellation
    , content =
        """
        You abandon your Power all together. You instead rely on SLOTS: _*Mode-Early*_.

        You start out with a static amount of slots, for one-and-done players that want the simplest experience and with an up-front build. Nothing else to consider, no mathing out Power, just a bundle of slots to use. You have:

        - 4 *Epic* slots,
        - 6 *Heroic* slots,
        - 10 *Noble* slots, &
        - 15 *Folk* slots

        You also have 3 _Heroic_ slots based on your Type.

        *Academics* use these for Magi. *Sorceresses* use these for Perks. *Warlocks* use these for Relics. It's up to you whether you have all this power now, or grow into it with in-context training or unlock them as you go at your own pace.
        """
    }


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
    # Game Mode

    {choice You can only choose one game mode, or none to play the default way. Each supports a different type of player, and what you might want out of it.}
    """
