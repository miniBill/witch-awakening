module Data.Complication exposing (Content(..), Details, intro, title, worldShiftsDescription)

import Generated.Types exposing (Class, Complication, ComplicationCategory)


type alias Details =
    { name : Complication
    , class : Maybe Class
    , content : Content
    , category : Maybe ComplicationCategory
    , dlc : Maybe String
    }


type Content
    = WithTiers String (List ( String, Int )) String
    | Single Int String
    | WithChoices String (List ( String, Int )) String
    | WithGains (List Int) String


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
