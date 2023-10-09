module View.Complications exposing (viewComplications)

import Element exposing (Element, alignBottom, alignTop, centerX, el, fill, height, moveDown, moveUp, padding, spacing, width)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Generated.Types as Types exposing (ComplicationCategory, ComplicationName(..))
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
        , [ brutality, masquerade ]
            |> List.map (complicationBox complications)
            |> Theme.wrappedRow
                [ centerX
                , spacing <| Theme.rythm * 3
                ]
            |> Element.map (\( complication, selected ) -> ChoiceComplication complication selected)
        , Theme.blocks [] <| String.Multiline.here """

            When taking world shifts, you're altering the nature of the particular version of Witch Awakening's reality that you enter into. The others may exist independently, but this one will be your home dimension.

            World shifts of course won't be seen in-universe as complications shown by Penelope, rather, they will be points of fact that Penelope points out similar to how she pointed out the information about the masquerade and other setting details.

            You can always choose if a World Shift affects the mundane and magical world alike, or only affects the magical world. (Only affecting the mundane world would be too inconsequential.)
            """
        ]


type alias ComplicationDetails =
    { name : ComplicationName
    , content : Content
    }


type Content
    = WithTiers String (List ( String, Int )) String
    | Single String Int


brutality : ComplicationDetails
brutality =
    { name = Brutality
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
    , content =
        WithTiers
            "The _Masquerade_ is laced with Covenant and Curse-like effects."
            [ ( "Revealing magic to strangers is auto-punished via pain response. Using magic to run key parts of a business among mortals is auto-punished by rasure of products or facilities. Can apply Rank 1 curses", 1 )
            , ( "Revealing to associates is auto-punished, strangers causes extra pain. Magic in business now includes parts of the production line or marketing Can apply Rank 3 curses", 2 )
            , ( "Revealing to close relationships is now auto-punished. Associates with extra pain, strangers may leave you unconscious after intense pain. Magic in business includes basic conveniences in running your shop. Can apply Rank 5 curses", 4 )
            ]
            ""
    }


complicationBox :
    List Complication
    -> ComplicationDetails
    -> Element ( Complication, Bool )
complicationBox selected { name, content } =
    let
        isSelected : Maybe Complication
        isSelected =
            List.Extra.find (\sel -> sel.name == name) selected

        category : ComplicationCategory
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

        gradient : List ( Int, Int, Int )
        gradient =
            Theme.complicationCategoryToGradient category

        color : Int
        color =
            Theme.complicationCategoryToColor category
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
            [ Element.column
                [ alignTop
                , Font.size 28
                , centerX
                , moveDown 8
                ]
                [ el [ centerX, Theme.captureIt ] <|
                    gradientText 4 gradient <|
                        Types.complicationCategoryToString category
                , el [ centerX ] <|
                    gradientText 4 Gradients.yellowGradient <|
                        String.join "/" <|
                            List.map
                                (\gain -> "+" ++ String.fromInt gain)
                            <|
                                case content of
                                    WithTiers _ tiers _ ->
                                        List.map Tuple.second tiers

                                    Single _ gain ->
                                        [ gain ]
                ]
            , el
                [ alignBottom
                , Theme.celticHand
                , Font.size 32
                , centerX
                , moveUp 8
                ]
                (gradientText 4 gradient <|
                    Types.complicationNameToString name
                )
            ]
        , content =
            case content of
                Single block _ ->
                    [ Theme.blocks
                        [ height fill
                        , Theme.padding
                        ]
                        block
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
                                        (if isTierSelected then
                                            [ Theme.backgroundColor color, Border.rounded 4, padding 4 ]

                                         else
                                            [ Border.rounded 4, padding 4 ]
                                        )
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
        , onPress = msg
        }
