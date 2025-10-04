module View.Faction exposing (viewFaction)

import Color exposing (Color)
import Data.Faction as Faction
import Element exposing (Element, alignBottom, alignTop, centerX, column, el, fill, fillPortion, height, inFront, moveDown, paddingXY, rgb, rgba, shrink, spacing, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Generated.Faction as Faction
import Generated.Gradient as Gradient
import Generated.Image as Image exposing (Image)
import Generated.Magic as Magic
import Generated.Types exposing (Faction)
import List.Extra
import Set exposing (Set)
import Theme
import Types exposing (Choice(..), Display(..), IdKind(..))
import View


viewFaction : Set String -> Display -> Maybe ( Faction, Bool ) -> Element Choice
viewFaction hideDLC display faction =
    let
        filtered : List Faction.Details
        filtered =
            Faction.all
                |> View.filterDLC hideDLC
    in
    if List.isEmpty filtered then
        Element.none

    else
        View.collapsible (Theme.topBackground Image.factionIntro)
            display
            DisplayFaction
            ChoiceFaction
            IdKindFaction
            "# Factions"
            [ Theme.column
                [ Theme.padding
                , spacing 32
                , width fill
                ]
                [ Theme.blocks
                    [ Background.color <| rgba 0 0 0 0.75
                    , Theme.rounded
                    , Theme.padding
                    , width <| Element.maximum 800 fill
                    , centerX
                    ]
                    IdKindFaction
                    (intro ++ String.repeat 4 "\n" ++ summaries)
                ]
            , filtered
                |> List.Extra.removeWhen .isHuman
                |> List.filterMap (factionBox display faction)
                |> Theme.column
                    [ width fill
                    , spacing <| Theme.rhythm * 3
                    ]
            , Element.row
                [ width fill
                , Background.image Image.factionHumansIntro1.src
                , width fill
                ]
                [ el [ width fill, Element.paddingEach { left = 32, bottom = 64, top = 200, right = 32 } ] <|
                    Theme.blocks
                        [ width fill
                        , Background.color <| rgba 1 1 1 0.75
                        , Font.color <| rgb 0 0 0
                        , Font.center
                        , Theme.padding
                        , Theme.rounded
                        ]
                        IdKindFaction
                        humansIntro
                , Theme.image
                    [ width fill
                    , alignBottom
                    ]
                    Image.factionHumansIntro2
                ]
            , filtered
                |> List.filter .isHuman
                |> List.filterMap (factionBox display faction)
                |> Theme.column
                    [ width fill
                    , spacing <| Theme.rhythm * 3
                    ]
            ]
            [ filtered
                |> List.filterMap (factionBox display faction)
                |> Theme.column
                    [ width fill
                    , spacing <| Theme.rhythm * 3
                    ]
            ]


factionBox :
    Display
    -> Maybe ( Faction, Bool )
    -> Faction.Details
    -> Maybe (Element (Maybe ( Faction, Bool )))
factionBox display selected details =
    if display /= DisplayFull && Maybe.map Tuple.first selected /= Just details.name then
        Nothing

    else
        Theme.column [ width fill, Theme.id IdKindFaction (Faction.toString details.name) ]
            [ introRow display details
            , Theme.wrappedRow [ width fill ]
                [ content selected details
                , viewPerk display selected details
                ]
            ]
            |> Just


content :
    Maybe ( Faction, Bool )
    -> Faction.Details
    -> Element (Maybe ( Faction, Bool ))
content selected { name, description, location, relations } =
    let
        isFactionSelected : Bool
        isFactionSelected =
            case selected of
                Nothing ->
                    False

                Just ( selectedFaction, _ ) ->
                    selectedFaction == name

        factionGlow : Maybe Color
        factionGlow =
            if isFactionSelected then
                Just (Color.rgb255 0xF3 0xEA 0x6F)

            else
                Nothing

        factionMsg : Maybe ( Faction, Bool )
        factionMsg =
            if isFactionSelected then
                Nothing

            else
                Just ( name, False )
    in
    Theme.maybeButton
        [ width <| fillPortion 5
        , height fill
        , Font.color <| rgb 0 0 0
        , case factionGlow of
            Just color ->
                Theme.backgroundColor <| Theme.colorToBackground color

            Nothing ->
                Theme.backgroundColor (Color.rgb255 0xC1 0xC1 0xC1)
        , case factionGlow of
            Just color ->
                Theme.borderGlow color

            Nothing ->
                Border.width 0
        ]
        { label =
            Theme.column [ alignTop ]
                [ Theme.blocks
                    [ height fill
                    , Theme.padding
                    ]
                    IdKindFaction
                    ("[DESCRIPTION:] " ++ description)
                , Theme.blocks
                    [ height fill
                    , Theme.padding
                    ]
                    IdKindFaction
                    ("[LOCATION:] " ++ location)
                , Theme.blocks
                    [ height fill
                    , Theme.padding
                    ]
                    IdKindFaction
                    ("[RELATIONS:] " ++ relations)
                ]
        , onPress = Just factionMsg
        }


viewPerk :
    Display
    -> Maybe ( Faction, Bool )
    -> Faction.Details
    -> Element (Maybe ( Faction, Bool ))
viewPerk display selected { name, perk, perkContent, images } =
    let
        isPerkSelected : Bool
        isPerkSelected =
            selected == Just ( name, True )
    in
    if display /= DisplayFull && not isPerkSelected then
        Element.none

    else
        let
            glowColor : Color
            glowColor =
                Color.rgb255 0xF3 0xEA 0x6F

            perkMsg : Maybe ( Faction, Bool )
            perkMsg =
                Just ( name, not isPerkSelected )
        in
        Theme.card
            [ if isPerkSelected then
                Theme.backgroundColor <| Theme.colorToBackground glowColor

              else
                Theme.backgroundColor (Color.rgb255 0xC1 0xC1 0xC1)
            , width <| Element.minimum 150 fill
            , height shrink
            , alignTop
            ]
            { display = DisplayFull
            , forceShow = False
            , glow = glowColor
            , isSelected = isPerkSelected
            , imageAttrs = []
            , imageHeight = 240
            , image = images.image5
            , inFront =
                [ Theme.gradientTextWrapped
                    [ alignBottom
                    , Theme.celticHand
                    , Font.size 24
                    , centerX
                    , paddingXY 8 0
                    ]
                    3
                    Gradient.blueGradient
                    perk
                ]
            , content =
                case List.Extra.find (\magic -> magic.faction == Just name) Magic.all of
                    Nothing ->
                        [ Theme.blocks [] IdKindFaction perkContent ]

                    Just magic ->
                        [ Theme.blocks []
                            IdKindFaction
                            (perkContent ++ "\n\n_*[" ++ Magic.toString magic.name ++ "]*_ is half price for you, stacks with affinity.")
                        ]
            , onPress = Just perkMsg
            }
            |> Maybe.withDefault Element.none


introRow :
    Display
    ->
        { a
            | name : Faction
            , dlc : Maybe String
            , motto : String
            , images : { image1 : Image, image2 : Image, image3 : Image, image4 : Image, image5 : Image }
        }
    -> Element msg
introRow display { name, dlc, motto, images } =
    let
        img : Image -> Element msg
        img { src } =
            el
                [ width <| Element.minimum 100 fill
                , height <| Element.minimum 300 fill
                , Background.image src
                ]
                Element.none
    in
    if display == DisplayFull then
        Theme.wrappedRow
            [ width fill
            , inFront
                (case dlc of
                    Nothing ->
                        Element.none

                    Just dlcName ->
                        el
                            [ centerX
                            , Theme.captureIt
                            , Font.size 24
                            , moveDown 4
                            ]
                            (Theme.gradientText 4 Gradient.purpleGradient dlcName)
                )
            ]
            [ img images.image1
            , Theme.column [ width <| fillPortion 4 ]
                [ img images.image2
                , img images.image3
                , column [ width fill ]
                    [ Theme.gradientTextWrapped
                        [ width fill
                        , Font.size 40
                        , Theme.celticHand
                        ]
                        2
                        Gradient.blueGradient
                        (Faction.toString name)
                    , Theme.gradientTextWrapped
                        [ width fill
                        , Font.size 24
                        , Theme.morpheus
                        ]
                        2
                        Gradient.yellowGradient
                        motto
                    ]
                ]
            , img images.image4
            ]

    else
        Theme.gradientTextWrapped
            [ width fill
            , Font.size 40
            , Theme.celticHand
            ]
            2
            Gradient.blueGradient
            (Faction.toString name)


intro : String
intro =
    """
    "I mentioned I can hook you up to other witches if you want. It helps to be less of a fish out of water, and help you practice your magics. Especially for academics or hard learners, though everyone still needs practice and hands on experience before you get the hang of it. Different factions employ different methods of staying on the down low or avoiding humans entirely. As witches, we aren’t limited to this world. Did you get a little overwhelmed by the idea of having to avoid earth governments? Don’t worry about it. The factions have you covered and have hammered out ways to avoid dealing with mortals. Most witches have abandoned Earth after they hecking _*nuked our ancient library*_. Calm. I’m calm. But yeah, most witches just don’t see a whole lot of reason to stick to earth these days. There are a lot of extraplanar interests in mortals, and primordial hierarchies, so even if we wanted to go to all out war against humans it’d just draw out larger entities and... Suffice it to say, we’ve run the numbers and it’s just better for everyone if we go our own ways. We get to explore different dimensions and planets, humans are stuck on this rock. Let them have it. So play by their rules when on their turf and you shouldn’t have any issue. If you want to go crazy with magic experiments, then knock your socks off anywhere else, it’s not hard" {choice You can pick a primary faction.} This does not prevent you from associating with the others, unless hostile.

    "Quick run down on the factions I think are more relevant" Beyond aesthetics, {choice your choice of primary faction grants a shown perk or relic}. You can forgo this perk to instead gain 2 Power, or you can skip choosing a Faction to be Independent for 4 Power. Your Faction’s magic is half cost to you, further reduced by half if you have a matching Affinity.

    {choice You can still take the Specialization of a faction you don’t belong to, but its power cost is doubled.}
    """


summaries : String
summaries =
    """
    {choice _Summaries:_}

    - {choice __*The College of Arcadia*__}. American upstarts running their own pocket realm. Rural american town vibes melded with a mild city experience, lots of personal liberties but they maintain a degree of country justice to keep the order. Heavily integrates modem lifestyle and conveniences you might expect while having quick access to expansive wildlife and wilderness to explore with paradisiacal climate. {choice _Modernity. Homey town surrounding the college. Endless expanse surrounding it full of shifting changing features._}

    - {choice __*Hawthorne Academia*__}. A very old prestigious institution that has continued to grow over time. Labyrinthian gothic architecture that heavily employs space folding and relativity, no up or down. It’s located somewhere under the Greenland ice sheet but parts of it sprawl out from connected portals that span the globe and Lunabella. They’re strict and disciplinarian with uniforms and not limited to mortal ethics. {choice _Extremely safe, discounting any punishments. Effective education, magic and mundane. Extraordinary architecture._}

    - {choice __*The Watchers*__}. Also known as the Followers of the Apocalypse, but it’s a mouthful. A bit more of a traditional coven, the Watchers date back to a few hundred years BC, and officially founded around 30AD. They integrate Abrahamic cosmology and teachings gleaned from all following churches into one overarching narrative from a third party perspective. Watchers form an interfaith network throughout, and take closer interest in mortal affairs and politics, with influence in most governments to bury things in red tape when needed. {choice _Wide connections within the mortal world. Access to wealth and influence. Help in most churches if you say the right things._}

    - {choice __*The Hespatian Coven*__}. Also operates under many alternative names with the way it subdivides into families that have overarching connections that cooperate in shared interests while retaining independent oversight for the most part. Members of a “family” may not know about the broader scope while just leadership roles are in the know and in communication with other families. They are the classic secret society, to outright secret cults, and they incorporate mortals into their structure as a way to gain leverages, blackmail, and other resources. {choice _Assassins and thieves of the highest caliber. Many conspiracies may relate to Hespatian families. Hell access._}

    - {choice __*Lunabella*__}. It may surprise you to know that this city on the moon was started in the 6th century by a lich with broomstick and a dream. It’s a glistening city that since grew to accommodate life and terraform into a garden region contained by crystal spires that maintain atmosphere- And hide casual observation from mortals. They’ve produced outposts on other planetary bodies. {choice _Killer view. Olympian luxury. Arcane transhumanism. Particular advances in transmutation magic._}
    """


humansIntro : String
humansIntro =
    """
    {center, mono} "Hey there, champ, miss, whatever. This is a pre-recorded message. If you’re seeing this, congratulations, you’ve just awakened as a Witch and we estimate now is about when the guidance ritual triggering this response has just finished going over some “Factions” of witches."

    {center} Allow me to introduce ourselves; Humans. Or “Mortals” as some witches say. As you may have noticed, we, and I, have some tricks of our own. Contrary to what most Initiator or “Guidance” witches say, such as the witch that guided your awakening just now or you wouldn’t be seeing this message, I’m sure you’ve heard some spiel about surveillance and “They nuked our library” or some such, fact is humans and witches have gotten along fine for over a century now.

    {center} Sure the nuke thing was more recent, but it’s what firmly shook some sense into the broader witch community. That library held some of the most insidious tomes of magic ever written that could- and did -make demons weep. Facts are, we’re not some weak incapable sheep to be herded anymore. Not all of us. We’re aware of the Treaties of the Masquerade, and we at the ORC... and those suits at Alphazon, act within the bounds of the masquerade to represent human interests. There are some others, but let’s keep it simple for now, and we aren’t just humans, we just don’t isolate ourselves and pretend that we’re not. I’m a witch the same as you, but we’re still human too, our gifts don’t have to change who we are.

    {center} Help us, and we help you. We don’t relish having to make enemies, but some witches let power get to their head, and they start viewing ungifted humans as pawns or resources to be used or spent.

    {center} There are two major “factions” of us, so we’ll present it like those you’ve seen so far.

    {center, mono} "Rolling film, let’s take a look."
    """
