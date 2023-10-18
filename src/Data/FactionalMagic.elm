module Data.FactionalMagic exposing (Details, all, intro)

import Data.Magic exposing (Affinities(..))
import Generated.Types exposing (Affinity(..), Class(..), Faction(..), Magic(..))


type alias Details =
    { name : Magic
    , star : Bool
    , class : Maybe Class
    , faction : Faction
    , affinities : Affinities
    , description : String
    , ranks : List String
    }


all : List Details
all =
    [ digicasting ]


digicasting : Details
digicasting =
    { name = Digicasting
    , star = False
    , class = Just Academic
    , faction = Arcadia
    , affinities = Regular [ Soul, Mind, Life ]
    , description =
        """Ideas have power behind them. Every new thought is an echo of creation deep below the sub layers of the material reality, swimming on the edge of oblivion and nothingness where what's real or not becomes a blurred line. With more attention, emotion, and repeat application of will, some ideas get rooted and cemented in that abyss, adding to the infinite expanse of the Something, the opposing sphere of influence encompassing all that is. To cut the preamble short: Digicasting is the access to worlds of imagination created over time from dedicated dreamers. The dream of an author giving birth to a new world line by line over years of dwelling on it. The shared imagination of countless developers, as well as players, that share in the experience of a digitally realized reality. Some forms of this magic have existed in isolated cases focused on dreams, Digicasting is a new specialization formalized in Arcadia and taught alongside planar theory."""
    , ranks =
        [ """Rank 1 grants a foundational element to digicasting, a minor ritual you use prior to sleeping using a medium containing a synthetic world. A book, game cartridge, CD, or thumb drive, or a controller or mouse extended from your PC or console. When you dream, instead of tapping your own dreamspace, you instead host the world of the chosen medium in full lucid detail. Unlike a lucid dream, the dream is not in your control but is running on automatic from the collective intentions of its contributors behind that world's creation. Including secrets not public knowledge that were side thoughts or wishful thinking that didn't make it into the final product such as king so and so having an affair with that maid. You can reset or return to where you left off with repeat uses. Time passes in real time, though you can skip by sleeping within the dream. World logic persists as normal, including leveling and class systems of games, alternate physics or magic systems of books, ect."""
        , """Rank 2 allows you to bring others into a shared dream space, with or without hosting a medium. While waking, you can use a screen to pull digital constructs out from its world into your own. Such objects are either made of what appears to be pixels, or ink. They are cosmetic, without special abilities or explosive properties, but the strange matter is as light as aerogel and strong as steel. They can appear 2D, and slowly disappear with ink dripping away or mixels sparking off, over about 10 minutes."""
        , """Rank 3 allows you to access an imaginary world through a medium without having to dream first. You can travel through a screen or page directly, digitizing yourself to enter it instead of the reverse. Your body disappears and the object gains an inky mist or pixelated fog. Like with r2, you can bring others with you. You can also enter the internet itself at large, and reappear at any connected device you can find. It's like that mortal movie... wreck it ralph. You can observe IPs and search, or seek out a known IP. Computers connected to Hex VPN stand out to you with strange identifiers that are unique but unreadable by devices, visible only to witches in digicast. Entering worlds in this manner connects you to the “true” world of that given work, and is shared by other digicasters who have visited that same world. It has persistence, and you could even leave things from the real world in that digi world, and retrieve it later."""
        , """At rank 4, you don't have to have a medium to pull digi constructs from imaginary worlds you've been to, but can instead directly manifest them where you can see within 60ft. About as tiring as doing 2 jumping jacks, per 1 cubic foot of construct. Such objects can also be spatially locked in place to remain set in the air or relative to you. Additionally, you can pull creatures from the medium that behave as that creature would in said medium. They appear as they would be viewed in that medium. ie; 2D, stylized, or “real”, as the creator imagined. Again, they don't have any special abilities that aren't comparable to a mundane adult human."""
        , """At rank 5, you can digitize yourself in the real world at-will, adopting a form of a given style. You resist harm as though made of steel and emit a candle-strength glow if composed of pixels as opposed to ink, with no biological requirements for life, including aging. You're solid all the way through with desired exceptions. The form can be 2D or 3D. In this form, you can enter and exit screens or illustrations freely. When you exit you can assume normal size, or exit at the scale of the screen. A phone screen could see you at a few inches tall. A large theater screen could easily octuple your height, Or large wall art on the size of a building. If 2D, your edges are blade-like and you can slip through cracks wide and uniform enough to slip through, allowing for only slight bends as though made of stiff rubber, but you can also reduce the profile of your 2D form by crouching, for example. Winds could be a problem for you, catching your side like a sail."""
        ]
    }


intro : String
intro =
    """
    Factional Magic are the various magic specializations that are associated with a specific faction where that specialization originates or is otherwise more common with them while being less common beyond it. This can be different per specialization. For example, Wands is well known by the very nature of Hawthorne where Hawthome students usually move on throughout Witchdom and most people are welcome to study at Hawthorne and learn. On the other hand, you have Occultism, the magic of Hespatia, that is far less common as people don't usually live to betray Hespatia and they aren't exactly welcoming to sit-ins.

    If a Factional magic has a Rank 0 effect, then it is universally available to all witches all the same, but the evolution on that rank 0 effect requires the specialized knowledge known to the given faction.

    The two faction “magics” of Gadgetry and Integration are NOT magic at all, but nonetheless require Power to invest in as a measure of your fate and opportunities.
    """
