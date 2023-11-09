module Data.Complication exposing (Content(..), Details, all, generic, intro, title, worldShifts, worldShiftsDescription)

import Generated.Types exposing (Class(..), Complication(..))


type alias Details =
    { name : Complication
    , class : Maybe Class
    , content : Content
    }


type Content
    = WithTiers String (List ( String, Int )) String
    | Single Int String
    | WithChoices String (List ( String, Int )) String
    | WithGains String (List Int)


all : List Details
all =
    worldShifts ++ generic


worldShifts : List Details
worldShifts =
    [ brutality, masquerade, trueNames, monsters, population, bonk ]


generic : List Details
generic =
    [ dysfunction, vulnerability, rejection, crutch, restriction, hunted, dislikeable, monsterBait, blackSwan, spellSink, likeADuck, likeARock, eyeCatcher, sillyGoose, hardLessons, coldHeart, hideous, witchMark, nemesis, addiction, sensoryDisability, physicalDisability, sensoryShock, adoringFan, veryDere, requirement, unveiled, nightmares, kryptonite, fitWitch, branded, noPrivacy, bloodFeud, marked, defeated, fixation, allNatural, witchknight, inadequacy, dysphoria, betrayal, compulsion ]


brutality : Details
brutality =
    { name = Brutality
    , class = Nothing
    , content =
        WithTiers
            "The world is shifted towards _brutality_ to a chosen tier:"
            [ ( "Violence is more widespread, people are quicker to react aggressively to get their way, being more forceful in pursuit of their interests and less understanding of slights", 1 )
            , ( "Killing becomes more commonplace. Enemies are considerably more likely to kill you outright before or after other interests, such as drawing it out for fun, witches value life less due to their ways to cheat death, so witches often kill other witches", 2 )
            , ( "Death is ubiquitous, ever present. Most people know several people who have been killed and it’s very common practice to confirm kills of witches, ensuring their method of cheating death is voided", 6 )
            ]
            ""
    }


masquerade : Details
masquerade =
    { name = Masquerade
    , class = Nothing
    , content =
        WithTiers
            "The _Masquerade_ is laced with Covenant and Curse-like effects."
            [ ( "Revealing magic to strangers is auto-punished via pain response. Using magic to run key parts of a business among mortals is auto-punished by erasure of products or facilities. Can apply Rank 1 curses", 1 )
            , ( "Revealing to associates is auto-punished, strangers causes extra pain. Magic in business now includes parts of the production line or marketing Can apply Rank 3 curses", 2 )
            , ( "Revealing to close relationships is now auto-punished. Associates with extra pain, strangers may leave you unconscious after intense pain. Magic in business includes basic conveniences in running your shop. Can apply Rank 5 curses", 4 )
            ]
            ""
    }


trueNames : Details
trueNames =
    { name = TrueNames
    , class = Nothing
    , content =
        WithTiers
            "_True Names_ become more important for any magical being. Your True Name is instinctually known to you once you Awaken. You can be coerced into providing your true name."
            [ ( "If someone knows another’s True Name, both feel more drawn to one another, making it easy to always notice them in a crowd, and subtly be guided to them if looking", 1 )
            , ( "Speaking someone’s True Name can affect them similar to the Suggestion perk, and they’re more susceptible to persuasion against usual interest", 3 )
            , ( "Speaking a True Name can let you affect the named target with magic regardless of distance. If warded, they feel when the ward is no longer blocking them", 8 )
            ]
            ""
    }


monsters : Details
monsters =
    { name = Monsters
    , class = Nothing
    , content =
        WithTiers
            "_Monsters_ become more common and widespread, increasing rates. This does not mean that the monsters are terrorizing the place, and many might not even kill, but they cause problems in general. The Veil still hides them from humans and the humans from most monsters, usually."
            [ ( "Almost every city has at least a handful of active monsters, towns may have 1-3. Monsters that target humans are _Uncommon_", 1 )
            , ( "Cities may have dozens of active monsters while towns deal with up to a handful Monsters targeting humans are _somewhat uncommon_", 2 )
            , ( "Cities can be infested with various monsters while towns have to deal with a few every week or 1/day. Monsters targeting humans are _common_", 4 )
            ]
            ""
    }


population : Details
population =
    { name = Population
    , class = Nothing
    , content =
        WithTiers
            "The _population_ of witches is decreased, increasing the burden on individual witches to maintain supernatural balances while increasing individual attention."
            [ ( "-50%. The size of all factions is cut in half, except the ORC and Alphazon who only have half the witch asses", 1 )
            , ( "There are only a few dozen witches with ranks 4+ in any given magic, throughout all realms of the living. Total pop of witches is less than a country", 2 )
            , ( "There is now only a single witch with rank 5 in any given magic specialization. You yourself can only choose one, the rest are capped, and if you choose a companion with r5 in your chosen special, their rank is reduced to 4. Total witch population measured in hundreds", 3 )
            ]
            ""
    }


bonk : Details
bonk =
    { name = Bonk
    , class = Nothing
    , content =
        WithTiers
            "The world is shifted towards a degree of _lewdity_, you can determine for yourself if this affects just Witchdom, or the mundane world as well."
            [ ( "Eroticism is more common. Public ads use more innuendo or nudity. PDA is a little more handsy. Hookup culture somehow even more common. Seen as a normal topic for chats", 1 )
            , ( "Ads might be straight up explicit. PDA might include foreplay. Hide-the-pickle is often a casual pastime among friends", 2 )
            , ( "Almost all ads are explicit. Hide-the-pickle is as common as a handshake or hug and an expected norm or you’re seen as a strange prude for not engaging with it. Pass once? Maybe an off day. Repeatedly? Okay something is suspicious", 3 )
            ]
            ""
    }


dysfunction : Details
dysfunction =
    { name = Dysfunction
    , class = Just Sorceress
    , content =
        Single 12 """
            "WHOA I was wrong, I’m so sorry. You ARE witch, you’ll get Witch Type as normal, you have lot of power... but... it looks like your maximum rank is 0... It’s not unheard of." You only benefit from Rank 0 effects of magic, and are otherwise incapable of putting any ranks into any magic specializations, or any perks with a cost greater than 4. (after counting affinity and type discounts). No impact on Relics, Gadgetry, Integration, Type perks, or Metamorphosis.
            """
    }


vulnerability : Details
vulnerability =
    { name = Vulnerability
    , class = Just Academic
    , content =
        WithChoices
            "Choose one serious Weakness. Weakness bypasses Affinity resistances, negating it."
            [ ( "*Pyre*: You catch fire as though made of dry straw and lint", 4 )
            , ( "*Melt*: Water melts you as though it were an incredible acid, but only when recognizable as water and not another product, such as “Beer”, or “Soda”", 4 )
            , ( "*Iron*: Iron and all its forms that would still be called iron, sear you as though white hot, and can bum through you. An iron blade passing through you with ease", 4 )
            ]
            ""
    }


rejection : Details
rejection =
    { name = Rejection
    , class = Just Warlock
    , content =
        Single 2 """
            Nature doesn’t like you. Non-feline animals will flee the area Domesticated animals like dogs get agitated and bark at you Predators may attack. Plants seem more sickly flowers close. Bugs bother or sting you more often.

            Familiars aren’t affected.
            """
    }


crutch : Details
crutch =
    { name = Crutch
    , class = Just Academic
    , content =
        Single 2 """
            You can’t use magic on your own, you have to rely on a specially prepared magical medium; Wands, Staves, Tomes, Crystal Balls, Decks of arcana cards, and so on. Without a medium, you cannot use magic.
            """
    }


restriction : Details
restriction =
    { name = Restriction
    , class = Just Sorceress
    , content =
        WithGains """
            You are incapable of learning any magic from one chosen archetype. Potions, Hexes, ect. You can take this up to 3 times. This includes a magic’s rank 0 effect normally available to all witches. This cannot restrict you from Faction magic of factions you don’t belong to.

            For example, you could restrict _Wands_ only if you chose Hawthorne as your faction.
            """
            (List.map ((*) 2) <| List.range 1 3)
    }


hunted : Details
hunted =
    { name = Hunted
    , class = Just Warlock
    , content = Single 8 """
        You’re actively being hunted by a _revenant_ witch hunter. He has somehow bonded to you as his next target. He bonds to one witch at a time and relentlessly hunts that witch until their True Death. He’s killed many before. He’ll intelligently stalk and take notes until he thinks it’s time to strike and learn what he can. He “respawns” every full moon with his soulbound diviner’s map. He may cooperate with your enemy.
        """
    }


dislikeable : Details
dislikeable =
    { name = Dislikeable
    , class = Just Warlock
    , content = Single 2 """
        Something about you rubs people the wrong way, it’s harder to find wholesome true friends or lovers on a real personal level. Companions cost +2p to buy and will take longer to form serious relations.
        """
    }


monsterBait : Details
monsterBait =
    { name = MonsterBait
    , class = Just Academic
    , content = Single 4 """
        Something about your awakened soul is like a siren’s call to monstrous entities. Different monsters may be after you for and per witch type. Monster encounters greatly increased.
        """
    }


blackSwan : Details
blackSwan =
    { name = BlackSwan
    , class = Just Sorceress
    , content = Single 4 """
        Things very often go wrong around you. It can feel like you’re very unlucky, yet also more lucky in avoiding death in the moment to moment, but will be taking that gamble a lot more often. You’re often in the wrong place at the wrong time and end up inserting yourself into situations by accident.
        """
    }


spellSink : Details
spellSink =
    { name = SpellSink
    , class = Just Warlock
    , content = Single 2 """
        You’re like a hole in the ether of magic, it just disappears within you. You’re immune to direct magics, beneficial and harmful No buffs, healing, neither mind control or paralysis. It takes more effort for you to use magic being twice as fatiguing and costly.
        """
    }


likeADuck : Details
likeADuck =
    { name = LikeADuck
    , class = Just Sorceress
    , content = Single 2 """
        You’re lightweight, weighing as supernaturally buoyant in water. Difficult to dive, and you’re easier to manhandle or get knocked around by various effects, including a windy day

        If Sylph, you’d actively float like a helium balloon.
        """
    }


likeARock : Details
likeARock =
    { name = LikeARock
    , class = Just Academic
    , content = Single 2 """
        You’re twice as heavy, and sink through water as though it were air, yet move through it twice as slow as though walking through jello. Not incompatible with duck. With both, you’ll be lightweight to enemies, heavy otherwise, and sink in water.
        """
    }


eyeCatcher : Details
eyeCatcher =
    { name = EyeCatcher
    , class = Just Sorceress
    , content = Single 2 """
        You draw more attention in anything you do. Wherever you go, you turn heads. You’re physically more attractive than normal sure, but your presence itself is more magnetic in nature, presence.

        In short, anything you do to draw attention to yourself draws significantly more attention Meaning you’re easy to track if anyone sees you, as you leave an impression and most people will notice and remember it. It’s very difficult to “blend in” to crowds, and something stands out about you even through transformations. Like, damn, that’s a cute or elegant cat, ect, and if you cover yourself too much then that itself is notable.
        """
    }


sillyGoose : Details
sillyGoose =
    { name = SillyGoose
    , class = Just Academic
    , content = Single 2 """
        Your true form is younger than normal, anywhere between 6 and 14, but on top of that it’s abnormally hard for people to take you seriously. You tend to melt hearts when they see you and it seems to dumb people down, similar to seeing a really adorable kitten and instinctively talking down, magnified a few times. They’re generally dismissive with presumption that you’re being imaginative, or mistaken about something. If it’s any consolation, they’ll likewise resist believing you did something wrong, depending, You are more excitable and do have an overactive imagination. For 1p instead of 2, you can only have the age curse. Persistent.
        """
    }


hardLessons : Details
hardLessons =
    { name = HardLessons
    , class = Just Academic
    , content = Single 8 """
        Magic just refuses to “click” fo you, struggling harder to learn or train your magic, or keep managing to do something to displease your patron, the effect is the same: You have half the rate of Power gain from your witch type, and once you cement the ability in question, you still take longer to get the hang of actually utilizing the magic properly, requiring more practical practice before at full efficiency.
        """
    }


coldHeart : Details
coldHeart =
    { name = ColdHeart
    , class = Just Warlock
    , content = Single 2 """
        Your empathy is muffled, it’s harder to see things from the perspective of others and to recognize the emotions in others that aren’t obvious, but things like mood changes and facial expressions. You’re quick to see people in black and white; Either allies, or enemies, and if people aren’t as skilled or powerful as you, you instinctively think less of them. This can manifest as a tyrant, or as mother knows best, elitist with a reflex of wanting control either way
        """
    }


hideous : Details
hideous =
    { name = Hideous
    , class = Just Warlock
    , content = Single 8 """
        Your appearance is seriously unfortunate. You look *very* old, plus choose at least 5: Hunched back. Large hooked nose. Warty. Crazy eyes. Ratty hair. Gnarled limbs. Crooked jaw. Jagged teeth. Snaggletooth. Fungal growths. Bone growths. Pustules. Bunion. Cankles. Leathery skin. Wookie hair. Yellowed woody nails. Persistent with shapechanging, you still look worn and ugly.
        """
    }


witchMark : Details
witchMark =
    { name = WitchMark
    , class = Just Sorceress
    , content = Single 2 """
        A peculiar birthmark resembling a third nipple somewhere on your body. Those in the know can identify it as a mark of a witch. Their err of course being not all witches have one.

        Yes, if you must know, they’re as sensitive as a real one, so be a little careful with it. No I’m not telling where mine is! Some witches try removing them and they just reappear. Hex witches are able to move it for you, if you wish. I knew a witch whose mark was on her forehead, poor gal, until we found her and helped out with a relocation. Persistent.
        """
    }


nemesis : Details
nemesis =
    { name = Nemesis
    , class = Just Sorceress
    , content = Single 8 """
            Oh no. It’s been a long time since I’ve seen anyone with this in their fate. Your awakening will provoke a paired awakening in another individual. It can be anyone in your life that you would hesitate to kill. If you have. nobody, or by chance, it can be an entirely new being retroactively woven into your life and you will have new memories of them that you can’t distinguish from your old life. You WILL care about them, and they will care about you to an extent, but they will resent you deep to their core. They don’t want you to die, but they will want you to fail or suffer. If you have something good, they want it for theirself. If you succeed at something, they want to be better at it than you. Expect to never have peace unless there is an equilibrium between you and them. They have any and every special ability you have, including drawbacks, perks, relics. They’ll have roughly equivalent companions. Turning their heart will take a LONG storyarc, if at all.
            """
    }


addiction : Details
addiction =
    { name = Addiction
    , class = Just Warlock
    , content = Single 2 """
            You have an addiction of some form. You cannot go more than a week without it willingly, and that’s a good week. It’s usually a daily habit and you won’t feel like yourself without having engaged with it, Whether it is a substance or stimuli. It can be a drug, or it could be coffee, or lewd actions. You have a 25% chance of being incapable of resisting your addiction whenever it crops up which increases by 5% with every repeat exposure until you give in. An addiction to lewd things for example, would count any attractive sight as an exposure. This doesn’t mean immediate gratification but when you have a reasonable moment to indulge, while going a little out of your way to have the opportunity. The addiction can’t be something generally seen as productive or healthy (Honestly). This can be taken up to 3 times.
            """
    }


sensoryDisability : Details
sensoryDisability =
    { name = SensoryDisability
    , class = Just Academic
    , content = Single 2 """
            You have a sense that doesn’t function properly. Any of the 5 senses. Compared to sight; You’d be very nearsighted and have to squint to read normal sized text from around 1ft Hearing would mean you’d not notice someone whispering 5ft away, and their casual speaking volume would be muffled. With scent, you can only faintly smell something directly under your nose. With touch, you feel only about 10% the pleasure or pain someone else would and wouldn’t notice a squirrel climbing up your leg, and you’d only faintly feel a cat digging its claws into your back. Stepping on a leggo would be tolerable. With taste, you only taste extremes, So you might have to enjoy food in other ways like with heat or texture, or very oversaturated flavors. For 5p, you’re completely missing a sense instead.

            Can take up to 3 times
            """
    }


physicalDisability : Details
physicalDisability =
    { name = PhysicalDisability
    , class = Just Academic
    , content = Single 2 """
            For some reason your true form is missing a limb as a core part of your identity, your true form is a manifestation of the reality of your being, so it’s a little more important than that.

            Replacements wont work and it persists through transformation magic. Inorganic prosthetics do work, or a symbiotic lifeform that could operate on its own

            You can take this up to 4 times.
            """
    }


sensoryShock : Details
sensoryShock =
    { name = SensoryShock
    , class = Just Sorceress
    , content = Single 2 """
            Opposite to sensory disability, one of your senses is acutely hyperaware. Can’t take with the same sense as you have a sensory disability. If vision, you see very sharply and at great distance but you get overwhelmed with complex colors and patterns and with too much movement to take in all at once, excellent low light vision but sudden bright light can blind you like daggers to the eyes. If scent, you can smell a campfire from miles away but strong unpleasant odours can completely overwhelm you, with some scents worse than others such as garlic. If touch, you might be perpetually uncomfortable by all but masterwork silk clothing and the finest cottons, and more sensitive to pain. Etc.
            """
    }


adoringFan : Details
adoringFan =
    { name = AdoringFan
    , class = Just Sorceress
    , content = Single 2 """
            You have a fan. A bit of a simpleton, he’s completely devoted to you. Or rather, the idea of you. He’s unattractive to you but really wants in your pants even if he has a girlfriend or wife already. He will consume any media that has you in it religiously, while also stalking you in his free time. Telescopic cameras from the tree or adjacent building level stalking, and will to the best of his not insignificant ability, break into properties or online accounts just to observe. maybe steal personal effects, not based on monetary value, but out of his obsession for you.

            Depending on how you handle this, he may go more insane and daring, or he might be kept in line and managed as a basic nuisance. If he dies, he’ll be replaced with another like him, on top of any normal stalkers you may attract the old fashioned way. If you were involved with their death, the next will be more daring than the last, which can stack to the point they become literal monsters hunting you.
            """
    }


veryDere : Details
veryDere =
    { name = VeryDere
    , class = Just Sorceress
    , content = Single 2 """
            However you actually feel inside it’s very hard for you to express positive emotions directed at another person and you can’t take compliments well. You’re easily flustered, embarrassed, aroused, or cheered up by others, and your natural response is the appearance of being agitated and to reject and distance yourself from the scenario. You feel like you shouldn’t be feeling these things and like it’s a failure or open weakness if you give in to them or let others notice your true feelings.

            ...In simple terms, you’ve become a tsundere

            Alternatively, you can be a kuudere, dandere, yandere, himedere, kamidere, bakadere, or shundere. You embody the dere of choice, at least externally.
            """
    }


requirement : Details
requirement =
    { name = Requirement
    , class = Just Warlock
    , content =
        WithChoices """
            Above and beyond an addiction, you have a requirement necessary for your survival the way a human requires food, water, and air.
            """
            [ ( "For 4p, you require it as often as food", 4 )
            , ( "For 8p, you require it as often as water", 8 )
            , ( "For 12p, you require it as often air", 12 )
            ]
            """
            This includes the level of discomfort with its absence. This can be any substance or stimuli that is at minimum as rare as its equivalent.

            This isn’t just a hunger equivalent, but the more you’re lacking your requirement the more you begin to passively lose a small amount of mana, and the more mana is consumed in the use of magic spells and effects.
            """
    }


unveiled : Details
unveiled =
    { name = Unveiled
    , class = Just Sorceress
    , content = Single 4 """
            The veil doesn’t hide you. Humans will see you clearly as you are and any act of magic. If your appearance is changed, your mortal ID doesn’t change and anyone that knew you noticed the differences if any.

            Normally the veil would hide unnatural features while something like a guy becoming a gal would be made normal for most humans affected by the veil though inconsistencies and glitches can happen now and then especially when pressed or given cause to remember otherwise. Likewise for using magic in front of humans. This still stresses the veil and can lead to masquerade violations eventually if abused, and you never know when a medium is present, or someone resists.

            """
    }


nightmares : Details
nightmares =
    { name = Nightmares
    , class = Just Academic
    , content = Single 6 """
            You’ll have a split soul torn between our universe, and a mundane universe where magic doesn’t exist. When you sleep, which you’ll require at regular intervals regardless of magic or perks, (At least once a week) you’ll “Wake” in the other world, an alt-Earth where Soviet Russia conquered the world as the first to develop nukes. If you aren’t useful to society, you’re considered a parasite... and are suspicion for past ramblings about the other world and magic prior to you “waking up”. Try not to end up lobotomized and turned into an automata slave, which can happen for a variety of causes, including accusations of anti-Party conspiracy or an officer’s word Whenever you die, you relive it again from the point you originally entered it to do it all over.
            """
    }


kryptonite : Details
kryptonite =
    { name = Kryptonite
    , class = Just Warlock
    , content =
        WithGains """
            Some witches express magic on a wavelength that can be disrupted by some substance. You have a kryptonite. It can be rare but it’s not rare enough that your enemies can’t make use of it. Its rarity affects the power gain:

            - Very common: +10.
            - Common: +8.
            - Uncommon +6.
            - Rare: +4.

            The proximity within which it affects you:

            - Touch: +1.
            - 3m: +2.
            - 9m: +3.
            - 15m: +4.
            - 30m: +5.

            Severity:

            - Completely cut off from active magic and ongoing effects: +4.

            OR

            - Magic effectiveness halved: +2.
            - Rapidly drains mana store: +2.
            - Open to hypnotic suggestion / issued commands: +4,
            - These suggestions persist after exposure ends: +2
            - Death while exposed prevents passive methods of death circumvention: +6.
            """
            (List.range 7 31)
    }


fitWitch : Details
fitWitch =
    { name = FitWitch
    , class = Just Sorceress
    , content = WithGains """
            Your magic ability is more closely intertwined with your physical ability. In order to grow your magical ability, you have to maintain your physique the old fashioned way: Exercise. While magic can shape your body, this has no bearing on the magic. For every day you don’t exercise for 1 hour or more, you lose 10% overall magic effectiveness. (Damage, range, area, duration ect). For every 5 hours of overall exercise of minimum 30 minute intervals to count towards the total, you raise it by 10% up to 100%. Exercise counts if it’s sufficiently strenuous and makes you sweat (If you would be capable of sweating or not). This only stacks to a reduction of -50% at the worst, but can stack to -100% for an extra +2.
            """ [ 2, 4 ]
    }


branded : Details
branded =
    { name = Branded
    , class = Just Sorceress
    , content = Single 8 """
        Sorry to burst your bubble, but you’re not a witch. I’ve removed the veil from you because I want to run a little experiment... To turn you into a witch that may or may not be more powerful than most witches, and I can only do it on a human that didn’t already have a witch spark.

        I’m not doing this for free, the process I devised incorporates a slave crest written into your very soul, You will be my... lifelong unpaid employee with benefits. I treat my things very well, and you’ll usually have autonomy, I’m hardly a task master. But that’s the deal. I’ll turn you into a witch, but you’ll be MY witch, unable to act against my wishes in any way, \\*_Penelope will not have this ritual if you did not select this complication._
        """
    }


noPrivacy : Details
noPrivacy =
    { name = NoPrivacy
    , class = Just Academic
    , content = Single 8 """
        I’ve been under a lot of scrutiny lately, there’s a good chance that some bad actors are watching me right now and by performing this ritual, it will advertise your witch awakening across open channels. All your information about you will be leaked, they’ll know everything about you on quite the intimate level. They’ll see everything that you select in this process, from my perspective, a third person perspective, seeing your face and body. Knowing your weaknesses and strengths, your magic, what quests are in your future, and what factions you’re likely to interact with later. On the plus side, your allies will be closer, but so will your enemies, and they’ll know where you are for the first week.
        """
    }


bloodFeud : Details
bloodFeud =
    { name = BloodFeud
    , class = Just Warlock
    , content = Single 4 """
        You’ll be part of a magical lineage that has major deeply rooted beef with another magical lineage. This rival lineage will despise yours on an instinctual level with the whispering echos of their ancestors reinforcing their bias to highlight any little thing about you that might annoy them in some way, which applies to you about them as well. Resisting this will be akin to resisting an actual legitimate phobia, a deep visceral emotional response.

        You’ll constantly feel the instinct to hate them, and undermine them in whatever they’re trying to achieve. You’ll often find yourself wondering what they’re currently up to, nothing good of course. Describe this other lineage. They’ll be just as strong as yours.
    """
    }


marked : Details
marked =
    { name = Marked
    , class = Just Warlock
    , content = Single 2 """
        You’ll very soon find yourself with a bounty on your head, If you’re aligned more with law, the bounty will stem from _Hespatia Alphazon, and the Outsiders_. If you’re aligned more with chaos, then the bounty will stem from the _Watchers, ORC, and the Alliance_. _Independents_ and divisive elements of _Hawthorne_ and _Arcadia_ will also be involved on either side. _Lunabella_ is unlikely to be involved in either direction unless you’ve antagonized them.

        The bounty will make your life very chaotic to slowly taper off after months or years of engaging with would-be bounty collectors, diminishing over time as you prove to be too strong a mark for most of the hunters or assassins.
        """
    }


defeated : Details
defeated =
    { name = Defeated
    , class = Just Academic
    , content = Single 6 """
        You’re fate-bound to suffer a major defeat at some point and you’ll be at the mercy of your enemy for some amount of time, until they will decide to kill you in a predestined death. What is not predestined, is whether they interrupt your method of cheating death. Whatever your method they will at least ensure that you do die, so an Immortality Curse would be overcome for example. You’ll want to make preparations and plans such that your method of cheating death can be preserved or recovered from the scene and into safe hands, such as having Companions steal your body back, or intercepting your dryad’s seed or draviri egg for example. This could occur during a Quest, without failing the quest.
        """
    }


fixation : Details
fixation =
    { name = Fixation
    , class = Just Sorceress
    , content =
        WithChoices "You will have a particular fixation that influences your thoughts frequently, shaping how you view the world and others around you, influencing your opinions of people or places, and relations"
            [ ( "*Violence*. You’re fixated on at least the thought of violence. You often think about what it would be like to harm people, destroy things", 2 )
            , ( "*Eroticism*. Fixated on lewd thoughts and behaviors, you’ll often think about what people look like under those robes & what sort of things you’d do on that broom", 2 )
            , ( "*Curiosity*. Driven crazy by not knowing something, you can’t stand secrets or things you’ve told you can’t see. Gossipy or voyeury", 2 )
            , ( "*Paranoia*. The world is so big all of a sudden, threats anywhere, who can really be trusted?", 2 )
            ]
            ""
    }


allNatural : Details
allNatural =
    { name = AllNatural
    , class = Just Sorceress
    , content = Single 8 """
        Looks like we cannot trigger your awakening... I’m.. so sorry. Your witch identity is heavily buried within you, outside of our reach. You will require a natural awakening. Somewhat like childbirth, some people have it easy, most go through a lot of trauma... and then there are those who have especially traumatic ordeals that put them at the brink of death, assuming they survive. You are the latter. At some point in time after your meeting with Penelope, which you will forget about until you awaken, you’ll experience extreme hardships that push past your limits, which may also be a good excuse as the source of other complications, resulting in you naturally awakening before (or after) you would have died.
        """
    }


witchknight : Details
witchknight =
    { name = Witchknight
    , class = Just Warlock
    , content = Single 4 """
        You’re not actually a witch, you’re a _Witchknight_. A true witch who might not have even experienced an awakening yet, bonded to you by rejecting their witchdom on a subconscious level in accordance to what would be their true wishes, or it was orchestrated by a 3rd party such as gods, or themselves in a prior life that they wiped or sealed the memories of. You have all the benefits of being witch, but your source of power is a very mortal human boy or girl, either a child you’re guardian of, or a romantic interest, some manner of close relationship of the source. If your source dies, you lose your magic until they can be resurrected somehow, if ever. Since you aren’t actually a witch, you do not have to pay for _Elephant Trunk_ if you wish for it.
        """
    }


inadequacy : Details
inadequacy =
    { name = Inadequacy
    , class = Just Academic
    , content = Single 2 """
        No matter how objectively powerful or capable you may be, you will be plagued with imposter syndrome, feeling like you don’t belong, that you’re nothing special and that others would be better suited to any given situation you find yourself. You’ll always suggest others take the role you’d be best at and try to stick to the back or cheer another on, only stepping in if others insist but you’ll still doubt yourself all along the way. Won? A fluke.

        You’ll never feel like you’re good enough, at least not until you find someone that loves you deeply and encourages you to be your best after they’ve won your trust through perseverance, and you can trust them right? If not, you may as well act like you do, what’s the alternative?
        """
    }


dysphoria : Details
dysphoria =
    { name = Dysphoria
    , class = Just Academic
    , content = Single 2 """
        The changes brought on by awakening click in your brain in the wrong way, cementing the idea in your head that this body is alien to you, that this isn’t right. Who is that in the mirror?

        You’ll be unable to assume your past form regardless of magic, including _Transformation Sequence_ which will just give you a different human body. The body may be objectively better in every way but you wont feel like you deserve it, or that it was stolen from somebody else.

        You’ll have a phantom limb-like sensation where the body doesn’t align with your psychological awareness of self. You’ll always feel like an imposter of your own body, at least until you find true love that can bridge this gap in your mind... if you let it.
        """
    }


betrayal : Details
betrayal =
    { name = Betrayal
    , class = Just Warlock
    , content = Single 4 """
        A bad omen, you have a major betrayal in your future. One figure you trust and will not expect will completely blindside you with a serious betrayal that will come at a heavy cost to you on many levels. They will be someone you cared about and trusted as well as you could trust anybody. It will be random among your closest relationships or most respected individuals. This does not necessarily mean they think ill of you, perhaps they were forced, perhaps they were talked into it with lies and framing. Perhaps they thought they were doing the right thing. Perhaps they ARE doing the right thing.

        There is room for redemption, but the setback will cost you nonetheless, whether or not you lose that person for good is another story.
        """
    }


compulsion : Details
compulsion =
    { name = Compulsion
    , class = Just Sorceress
    , content =
        WithChoices "Somewhat of an advanced case of Addiction, you will have a particular compulsion that you can only overcome with great need only once in a while. The more infrequent the more overwhelming the compulsion is. This is measured by frequency:"
            [ ( "*Rare*: 1p encountered an average of once each year", 1 )
            , ( "*Very uncommon*: 2p encountered an average of once a month", 2 )
            , ( "*Uncommon*: 4p encountered an average of once a day", 4 )
            , ( "*Common*: 8p you’re likely to run into your compulsion multiple times a day", 8 )
            ]
            """
            - *Finite*: -50% Your compulsion is a finite exposure of things you only do once. Example: To eat every pie you see vs Eat every NEW type of pie you see.
            """
    }


title : String
title =
    "# Complications"


intro : String
intro =
    """
    "Now that you have a taste of what you can be, I should mention the system we have here. There are greater powers out here that are invested in the sustainability of mortal humans, powerful entities from demon lords to celestial monarchs and mysterious deities. They don’t like magic intervention upsetting the balance they have achieved and can have messed with fate itself to impose limitations and consequences for subverting “the natural order”. That aside, it’s nonetheless in our best interest to keep a distance. Some humans are in on it, some humans strain against it. The Treaties of the Masquerade, or simply The Masquerade, is a near universally agreed upon principle that all supernatural entities are beholden to. When they were established, humanity was infected with a contagious curseplague that rooted itself in the mind to amplify the effects of cognitive bias and expectations. On average, it would require extraordinary circumstance for a human to perceive the supernatural. A dragon flying over their head could be perceived as a plane spewing napalm, if they see anything at all, and it gets reported as a gas line explosion or terrorist bombing. Sometimes things bleed through and you get things like bigfoot, and sometimes spiritual being aren’t as affected, already being in the spirit world, so some people might see ghosts on occasion, or they mistake a vampire or elf as a ghost because their brain is trying to delete the information as it comes in and it misses some spots. Some humans are less affected than others, and human agencies exist that are aware of the Masquerade and contribute to upholding on their end, recognizing the need to maintain this balance and prevent the world from sliding into chaos. Witches are one thing, but if destabilized too far it could kick off the War in the Heavens all over again as demons and celestials fight for primacy. Last time that happened, the Dinosaurs didn’t make the cut. The humans have proven themselves a capable threat lately, feels like just yesterday the upstarts nuked the Library of New Alexandria..."

    "So. Basic principles of upholding the Masquerade:"

    - The Veil on human minds can only be stretched so far and you never know when someone resistant to it is watching, so when among mortals avoid obvious magic. It can also attract attention from supernatural entities, or government and private agencies that might have something to say about it in one way or another.
    - Using magic to help out an individual human in need can be fine, but don’t push it. Doing too much to upset the way of things strains the masquerade, whether or not it’s obviously magic at face value. You aren’t a special saint who’s the first person to think about ending world hunger. You’ll have to run a charity case like everyone else.
    - You can sell magic items, particularly consumables, to humans so long as you keep it to niche markets and market it as some natural remedy “They” don’t want you to know about so long as the effects are excusable by good luck, placebo, or modern medicine, unless the individual is in on the Masquerade and invested in keeping the secret.
    - If you need to relax or want to stretch some magical muscles, I recommend joining a Faction, I can hook you up later, most have their methods of avoiding the Masquerade and allies can be very helpful. Monsters exist, some are human, or other witches with skewed moral framework... Others are very, very literal.
    - Or don’t bother with the human world at all! Who says you need to even stay on Earth? Party with Lunabella on the moon, or fly yourself to Pluto and establish an interplanetary portal network, explore new dimensions and maybe even some divine realms!

    "Now.... Let’s see if we can spot any {choice _*complications*_} with your true form." {choice Complications raise your POWER CAP to a max of +30}, OR {choice grant additional Starting Points} _within your Power Cap_ separately.

    Complications make your life more difficult. {choice *Every Complication taken grants POWER shown in the corner.*}
    """


worldShiftsDescription : String
worldShiftsDescription =
    """
    When taking world shifts, you’re altering the nature of the particular version of Witch Awakening’s reality that you enter into. The others may exist independently, but this one will be your home dimension.

    World shifts of course won’t be seen in-universe as complications shown by Penelope, rather, they will be points of fact that Penelope points out similar to how she pointed out the information about the masquerade and other setting details.

    You can always choose if a World Shift affects the mundane and magical world alike, or only affects the magical world. (Only affecting the mundane world would be too inconsequential.)
    """
