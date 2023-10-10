module View.Complications exposing (viewComplications)

import Element exposing (Element, alignBottom, alignRight, alignTop, centerX, el, fill, height, moveDown, moveLeft, moveRight, moveUp, padding, px, spacing, width)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Generated.Types as Types exposing (Class(..), ComplicationCategory, ComplicationName(..), Slot(..))
import Gradients
import List.Extra
import String.Multiline
import Theme exposing (gradientText)
import Types exposing (Choice(..), Complication, ComplicationKind(..))


viewComplications : List Complication -> Element Choice
viewComplications complications =
    Theme.column
        [ width fill
        , spacing <| Theme.rythm * 2
        ]
        [ Theme.blocks [] <| String.Multiline.here """
            # Complications

            "Now that you have a taste of what you can be, I should mention the system we have here. There are greater powers out here that are invested in the sustainability of mortal humans, powerful entities from demon lords to celestial monarchs and mysterious deities. They don't like magic intervention upsetting the balance they have achieved and can have messed with fate itself to impose limitations and consequences for subverting “the natural order”. That aside, it's nonetheless in our best interest to keep a distance. Some humans are in on it, some humans strain against it. The Treaties of the Masquerade, or simply The Masquerade, is a near universally agreed upon principle that all supernatural entities are beholden to. When they were established humanity was infected with a contagious curseplague that rooted itself in the mind to amplify the effects of cognitive bias and expectations. On average, it would require extraordinary circumstance for a human to perceive the supernatural. A dragon flying over their head could be perceived as a plane spewing napalm, if they see anything at all, and it gets reported as a gas line explosion or terrorist bombing. Sometimes things bleed through and you get things like bigfoot, and sometimes spiritual being aren't as affected, already being in the spirit world, so some people might see ghosts on occasion, or they mistake a vampire or elf as a ghost because their brain is trying to delete the information as it comes in and it misses some spots. Some humans are less affected than others, and human agencies exist that are aware of the Masquerade and contribute to upholding on their end, recognizing the need to maintain this balance and prevent the world from sliding into chaos. Witches are one thing, but if destabilized too far it could kick off the War in the Heavens all over again as demons and celestials fight for primacy. Last time that happened, the Dinosaurs didn't make the cut. The humans have proven themselves a capable threat lately, feels like just yesterday the upstarts nuked the Library of New Alexandria..."

            "So. Basic principles of upholding the Masquerade:"

            - The Veil on human minds can only be stretched so far and you never know when someone resistant to it is watching, so when among mortals avoid obvious magic. It can also attract attention from supernatural entities, or government and private agencies that might have something to say about it in one way or another.
            - Using magic to help out an individual human in need can be fine, but don't push it. Doing too much to upset the way of things strains the masquerade, whether or not it's obviously magic at face value. You aren't a special saint who's the first person to think about ending world hunger. You'll have to run a charity case like everyone else.
            - You can sell magic items, particularly consumables, to humans so long as you keep it to niche markets and market it as some natural remedy “They” don't want you to know about so long as the effects are excusable by good luck, placebo, or modern medicine, unless the individual is in on the Masquerade and invested in keeping the secret.
            - If you need to relax or want to stretch some magical muscles, I recommend joining a Faction, I can hook you up later, most have their methods of avoiding the Masquerade and allies can be very helpful. Monsters exist, some are human, or other witches with skewed moral framework... Others are very, very literal.
            - Or don't bother with the human world at all! Who says you need to even stay on Earth? Party with Lunabella on the moon, or fly yourself to Pluto and establish an interplanetary portal network, explore new dimensions and maybe even some divine realms!

            "Now.... Let's see if we can spot any {choice _*complications*_} with your true form." {choice Complications raise your POWER CAP to a max of +30}, OR {choice grant additional Starting Points} _within your Power Cap_ separately.

            Complications make your life more difficult. {choice *Every Complication taken grants POWER shown in the corner.*}
            """
        , Theme.blocks [] <| String.Multiline.here """
            # Game Mode

            {choice You can only choose one game mode, or none to play the default way. Each supports a different type of player, and what you might want out of it.}
            """
        , [ complicationBox complications storyArc
          , complicationBox complications earlyBird
          , complicationBox complications skillTree
          , complicationBox complications constellation
          , Theme.blocks
                [ width <| Element.maximum 400 fill
                , alignTop
                , Border.width 1
                , Theme.padding
                , Theme.borderColor Theme.colors.gameMode
                ]
                (String.Multiline.here """
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
                    """)
          ]
            |> Theme.wrappedRow
                [ centerX
                , spacing <| Theme.rythm * 3
                ]
            |> Element.map (\( complication, selected ) -> ChoiceComplication complication selected)
        , Theme.blocks [] "# World Shifts"
        , [ complicationBox complications brutality
          , complicationBox complications masquerade
          , complicationBox complications trueNames
          , complicationBox complications monsters
          , complicationBox complications population
          , complicationBox complications bonk
          , Theme.blocks
                [ width <| Element.maximum 400 fill
                , alignTop
                , Border.width 1
                , Theme.padding
                , Theme.borderColor Theme.colors.worldShift
                ]
                (String.Multiline.here """
                    When taking world shifts, you're altering the nature of the particular version of Witch Awakening's reality that you enter into. The others may exist independently, but this one will be your home dimension.

                    World shifts of course won't be seen in-universe as complications shown by Penelope, rather, they will be points of fact that Penelope points out similar to how she pointed out the information about the masquerade and other setting details.

                    You can always choose if a World Shift affects the mundane and magical world alike, or only affects the magical world. (Only affecting the mundane world would be too inconsequential.)
                    """)
          ]
            |> Theme.wrappedRow
                [ centerX
                , spacing <| Theme.rythm * 3
                ]
            |> Element.map (\( complication, selected ) -> ChoiceComplication complication selected)
        , Theme.blocks [] "# More Complications"
        , [ dysfunction
          , vulnerability
          , rejection
          , crutch
          , restriction
          ]
            |> List.map (complicationBox complications)
            |> Theme.wrappedRow
                [ centerX
                , spacing <| Theme.rythm * 3
                ]
            |> Element.map (\( complication, selected ) -> ChoiceComplication complication selected)
        ]


type alias ComplicationDetails =
    { name : ComplicationName
    , class : Maybe Class
    , content : Content
    }


type Content
    = WithTiers String (List ( String, Int )) String
    | Single Int String
    | WithChoices String (List ( String, Int )) String


brutality : ComplicationDetails
brutality =
    { name = Brutality
    , class = Nothing
    , content =
        WithTiers
            "The world is shifted towards _brutality_ to a chosen tier:"
            [ ( "Violence is more widespread, people are quicker to react aggressively to get their way, being more forceful in pursuit of their interests and less understanding of slights", 1 )
            , ( "Killing becomes more commonplace. Enemies are considerably more likely to kill you outright before or after other interests, such as drawing it out for fun, witches value life less due to their ways to cheat death, so witches often kill other witches", 2 )
            , ( "Death is ubiquitous, ever present. Most people know several people who have been killed and it's very common practice to confirm kills of witches, ensuring their method of cheating death is voided", 6 )
            ]
            ""
    }


masquerade : ComplicationDetails
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


trueNames : ComplicationDetails
trueNames =
    { name = TrueNames
    , class = Nothing
    , content =
        WithTiers
            "_True Names_ become more important for any magical being. Your True Name is instinctually known to you once you Awaken. You can be coerced into providing your true name."
            [ ( "If someone knows another's True Name, both feel more drawn to one another, making it easy to always notice them in a crowd, and subtly be guided to them if looking", 1 )
            , ( "Speaking someone's True Name can affect them similar to the Suggestion perk, and they're more susceptible to persuasion against usual interest", 3 )
            , ( "Speaking a True Name can let you affect the named target with magic regardless of distance. If warded, they feel when the ward is no longer blocking them", 8 )
            ]
            ""
    }


monsters : ComplicationDetails
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


population : ComplicationDetails
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


bonk : ComplicationDetails
bonk =
    { name = Bonk
    , class = Nothing
    , content =
        WithTiers
            "The world is shifted towards a degree of _lewdity_, you can determine for yourself if this affects just Witchdom, or the mundane world as well."
            [ ( "Eroticism is more common. Public ads use more innuendo or nudity. PDA is a little more handsy. Hookup culture somehow even more common. Seen as a normal topic for chats", 1 )
            , ( "Ads might be straight up explicit. PDA might include foreplay. Hide-the-pickle is often a casual pastime among friends", 2 )
            , ( "Almost all ads are explicit. Hide-the-pickle is as common as a handshake or hug and an expected norm or you're seen as a strange prude for not engaging with it. Pass once? Maybe an off day. Repeatedly? Okay something is suspicious", 3 )
            ]
            ""
    }


storyArc : ComplicationDetails
storyArc =
    { name = StoryArc
    , class = Nothing
    , content =
        Single 0 """
            Your Starting Power is reduced to 10 points, which cannot be increased with complications. Instead, your Power Cap is increased to [150], and Complications can increase your Power Cap by up to +60.

            You cannot start with a magic at more than Rank 3 or perks worth more than 6p before discounts, but they unlock at milestones.

            - *50 Power*, you can increase magics to rank 4 and gain perks up to 12p.
            - *At 100 Power*, you can increase magics to rank 5, with no perk cost cap

            This means that in your build you'd incorporate Quests and mention the passage of time with the quests to include your growth, and what you buy along the way, as you rise in power over time.
            """
    }


earlyBird : ComplicationDetails
earlyBird =
    { name = EarlyBird
    , class = Nothing
    , content =
        Single 0 """
            Incompatible with _Story Arc_. Rather than start less and end more, you get it all over with from the get go.

            Your Power Cap matches your Starting Power, but your *Starting Power is increased to [75]*. This means that what you start with is what you get, only acquiring companions and relics via quests later on. Simplifies things greatly, removing growth methods. _You can still benefit from *complications*_ increasing starting power, so up to _+30p_, but not even wishes will grant you any additional power points once you start.

            You can store power for later use if you so wish to do so, to simulate natural growth at your own rate, though you can do that after buying too.
            """
    }


skillTree : ComplicationDetails
skillTree =
    { name = SkillTree
    , class = Nothing
    , content =
        Single 0 """
            You abandon your Power all together. You instead rely on SLOTS: _*Mode-Arc*_.

            You begin with one _folk_ magic slot & 1 *folk* slot for a perk. You also gain 3 folk slots based on class. *Academics* use these for Magic. *Sorceresses* use these for Perks. *Warlocks* use these for Relics After you've completed 4 quests, your starting magic & perk slot increase to *Noble*. After 4 more, they become *Heroic*. After 3 more, they become *Epic*. Your 3 class slots lag behind by 4 tier becoming Heroic when you gain Epic for your starters. You gain an extra slot every time you complete a quest equal to that quest's tier. Skill Tree mode users can spend 3 slots of a lower tier for 1 of a higher tier, or break 1 higher tier slot into 2 lower tier slots.
            """
    }


constellation : ComplicationDetails
constellation =
    { name = Constellation
    , class = Nothing
    , content =
        Single 0 """
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


dysfunction : ComplicationDetails
dysfunction =
    { name = Dysfunction
    , class = Just Sorceress
    , content =
        Single 12 """
            "WHOA I was wrong, I'm so sorry. You ARE witch, you'll get Witch Type as normal, you have lot of power... but... it looks like your maximum rank is 0... It's not unheard of." You only benefit from Rank 0 effects of magic, and are otherwise incapable of putting any ranks into any magic specializations, or any perks with a cost greater than 4. (after counting affinity and type discounts). No impact on Relics, Gadgetry, Integration, Type perks, or Metamorphosis.
            """
    }


vulnerability : ComplicationDetails
vulnerability =
    { name = Vulnerability
    , class = Just Academic
    , content =
        WithChoices
            "Choose one serious Weakness. Weakness bypasses Affinity resistances, negating it."
            [ ( "*Pyre*: You catch fire as though made of dry straw and lint.", 4 )
            , ( "*Melt*: Water melts you as though it were an incredible acid, but only when recognizable as water and not another product, such as “Beer”, or “Soda”", 4 )
            , ( "*Iron*: Iron and all its forms that would still be called iron, sear you as though white hot, and can bum through you. An iron blade passing through you with ease.", 4 )
            ]
            ""
    }


rejection : ComplicationDetails
rejection =
    { name = Rejection
    , class = Just Warlock
    , content =
        Single 2 """
            Nature doesn't like you. Non-feline animals will flee the area Domesticated animals like dogs get agitated and bark at you Predators may attack. Plants seem more sickly flowers close. Bugs bother or sting you more often.

            Familiars aren't affected.
            """
    }


crutch : ComplicationDetails
crutch =
    { name = Crutch
    , class = Just Academic
    , content =
        Single 2 """
            You can't use magic on your own, you have to rely on a specially prepared magical medium; Wands, Staves, Tomes, Crystal Balls, Decks of arcana cards, and so on. Without a medium, you cannot use magic.
            """
    }


restriction : ComplicationDetails
restriction =
    { name = Restriction
    , class = Just Sorceress
    , content =
        Single 2 """
            You are incapable of learning any magic from one chosen archetype. Potions, Hexes, ect. You can take this up to 3 times. This includes a magic's rank 0 effect normally available to all witches. This cannot restrict you from Faction magic of factions you don't belong to.

            For example, you could restrict _Wands_ only if you chose Hawthome as your faction.
            """
    }


complicationBox :
    List Complication
    -> ComplicationDetails
    -> Element ( Complication, Bool )
complicationBox selected { name, class, content } =
    let
        isSelected : Maybe Complication
        isSelected =
            List.Extra.find (\sel -> sel.name == name) selected

        category : Maybe ComplicationCategory
        category =
            Types.complicationNameToCategory name

        glow : Maybe Int
        glow =
            if isSelected == Nothing then
                Nothing

            else
                Just color

        msg : Maybe ( Complication, Bool )
        msg =
            case ( content, isSelected ) of
                ( _, Just complication ) ->
                    Just ( complication, False )

                ( Single _ _, Nothing ) ->
                    Just ( { name = name, kind = Nontiered }, True )

                ( WithTiers _ _ _, Nothing ) ->
                    Nothing

                ( WithChoices _ _ _, Nothing ) ->
                    Nothing

        gradient : List ( Int, Int, Int )
        gradient =
            category
                |> Maybe.map Theme.complicationCategoryToGradient
                |> Maybe.withDefault Gradients.blueGradient

        color : Int
        color =
            category
                |> Maybe.map Theme.complicationCategoryToColor
                |> Maybe.withDefault Theme.colors.folk

        gains : List Int
        gains =
            (case content of
                WithTiers _ tiers _ ->
                    List.map Tuple.second tiers

                WithChoices _ choices _ ->
                    List.map Tuple.second choices

                Single gain _ ->
                    [ gain ]
            )
                |> List.filter ((/=) 0)
                |> List.Extra.unique

        gainGradient : Element msg
        gainGradient =
            gains
                |> List.map (\gain -> "+" ++ String.fromInt gain)
                |> String.join "/"
                |> gradientText 4 Gradients.yellowGradient

        viewSlot : Slot -> Element msg
        viewSlot slot =
            el [ alignRight, moveLeft 4, moveDown 4 ] <|
                Theme.image [ width <| px 32 ] <|
                    Types.slotToImage slot
    in
    Theme.card
        { glow = glow
        , imageAttrs =
            [ Border.width 4
            , Theme.borderColor color
            ]
        , imageHeight = 400
        , image = Types.complicationNameToImage name
        , inFront =
            [ case class of
                Nothing ->
                    Element.none

                Just c ->
                    el [ alignBottom ] <|
                        Theme.image [ width <| px 40 ] <|
                            Theme.classToBadge c
            , case gains of
                [] ->
                    Element.none

                [ g ] ->
                    viewSlot (gainToSlot g)

                _ ->
                    viewSlot White
            , case category of
                Just c ->
                    Element.column
                        [ alignTop
                        , Font.size 28
                        , centerX
                        , moveDown 8
                        ]
                        [ el [ centerX, Theme.captureIt ] <|
                            gradientText 4 gradient <|
                                Types.complicationCategoryToString c
                        , el [ centerX, Theme.captureIt ] gainGradient
                        ]

                Nothing ->
                    el
                        [ moveDown 16
                        , moveRight 16
                        , Font.size 28
                        , Theme.captureIt
                        ]
                        gainGradient
            , el
                [ alignBottom
                , Theme.celticHand
                , Font.size 32
                , centerX
                , moveUp 4
                ]
                (gradientText 4 gradient <|
                    Types.complicationNameToString name
                )
            ]
        , content =
            case content of
                Single _ block ->
                    [ Theme.blocks
                        [ height fill
                        , Theme.padding
                        ]
                        (String.Multiline.here block)
                    ]

                WithTiers before tiers after ->
                    [ Theme.column [ height fill, Theme.padding ] <|
                        Theme.blocks [] before
                            :: List.indexedMap
                                (\tier ( label, _ ) ->
                                    let
                                        complication : Complication
                                        complication =
                                            { name = name
                                            , kind = Tiered (tier + 1)
                                            }

                                        isTierSelected : Bool
                                        isTierSelected =
                                            List.member complication selected
                                    in
                                    Input.button
                                        [ if isTierSelected then
                                            Theme.backgroundColor color

                                          else
                                            Border.width 1
                                        , Border.width 1
                                        , Border.rounded 4
                                        , padding 4
                                        ]
                                        { label =
                                            Theme.blocks []
                                                ("- *Tier "
                                                    ++ String.fromInt (tier + 1)
                                                    ++ "*: "
                                                    ++ label
                                                    ++ "."
                                                )
                                        , onPress = Just ( complication, not isTierSelected )
                                        }
                                )
                                tiers
                            ++ [ Theme.blocks [] after ]
                    ]

                WithChoices before choices after ->
                    [ Theme.column [ height fill, Theme.padding ] <|
                        Theme.blocks [] before
                            :: List.indexedMap
                                (\choice ( label, _ ) ->
                                    let
                                        complication : Complication
                                        complication =
                                            { name = name
                                            , kind = Tiered (choice + 1)
                                            }

                                        isChoiceSelected : Bool
                                        isChoiceSelected =
                                            List.member complication selected
                                    in
                                    Input.button
                                        (if isChoiceSelected then
                                            [ Theme.backgroundColor color, Border.rounded 4, padding 4 ]

                                         else
                                            [ Border.rounded 4, padding 4 ]
                                        )
                                        { label =
                                            Theme.blocks []
                                                ("- "
                                                    ++ label
                                                    ++ "."
                                                )
                                        , onPress = Just ( complication, not isChoiceSelected )
                                        }
                                )
                                choices
                            ++ [ Theme.blocks [] after ]
                    ]
        , onPress = msg
        }


gainToSlot : Int -> Slot
gainToSlot gain =
    if gain <= 4 then
        Folk

    else if gain <= 8 then
        Noble

    else if gain <= 12 then
        Heroic

    else
        Epic
