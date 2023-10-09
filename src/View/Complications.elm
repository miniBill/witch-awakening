module View.Complications exposing (viewComplications)

import Element exposing (Element, alignBottom, centerX, el, fill, height, moveUp, spacing, text, width)
import Element.Border as Border
import Element.Font as Font
import Generated.Types as Types exposing (ComplicationCategory(..), ComplicationName(..))
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

            - The Veil on human minds can only be stretched so far and you never know when someone resistant to it is watching, so when among mortals avoid obvious magic. It can also attract attention from supernatural entities, or government and private agencies that might have something to say about it in one way or another.

            - Using magic to help out an individual human in need can be fine, but don't push it. Doing too much to upset the way of things strains the masquerade, whether or not it's obviously magic at face value. You aren't a special saint who's the first person to think about ending world hunger. You'll have to run a charity case like everyone else.

            - You can sell magic items, particularly consumables, to humans so long as you keep it to niche markets and market it as some natural remedy "They" don't want you to know about so long as the effects are excusable by good luck, placebo, or modern medicine, unless the individual is in on the Masquerade and invested in keeping the secret.

            - If you need to relax or want to stretch some magical muscles, I recommend joining a Faction, I can hook you up later, most have their methods of avoiding the Masquerade and allies can be very helpful. Monsters exist, some are human, or other witches with skewed moral framework... Others are very, very literal.

            - Or don't bother with the human world at all! Who says you need to even stay on Earth? Party with Lunabella on the moon, or fly yourself to Pluto and establish an interplanetary portal network, explore new dimensions and maybe even some divine realms!

            Now.... Let's see if we can spot any complications with your true form." Complications raise your POWER CAP to a max of +30, OR grant additional Starting Points within your Power Cap separately.

            Complications make your life more difficult. Every Complication taken grants POWER shown in the corner.

            """
        , [ brutality, masquerade ]
            |> List.map (complicationBox complications)
            |> Theme.wrappedRow
                [ centerX
                , spacing <| Theme.rythm * 3
                ]
            |> Element.map (\( complication, selected ) -> ChoiceComplication complication selected)
        ]


type alias ComplicationDetails =
    { name : ComplicationName
    , content : Content
    }


type Content
    = WithTiers String (List String) String
    | Single String


brutality : ComplicationDetails
brutality =
    { name = Brutality
    , content =
        WithTiers
            "The world is shifted towards _brutality_ to a chosen tier:"
            [ "AAA" ]
            ""
    }


masquerade : ComplicationDetails
masquerade =
    { name = Masquerade
    , content =
        WithTiers
            "The world is shifted towards _brutality_ to a chosen tier:"
            [ "AAA" ]
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
                category
                    |> Theme.complicationCategoryToColor
                    |> Just

        msg : Maybe ( Complication, Bool )
        msg =
            case ( content, isSelected ) of
                ( _, Just complication ) ->
                    Just ( complication, False )

                ( Single _, Nothing ) ->
                    Just ( { name = name, kind = Nontiered }, True )

                ( WithTiers _ _ _, Nothing ) ->
                    Just ( { name = name, kind = Tiered 1 }, True )
    in
    Theme.card
        { glow = glow
        , imageAttrs =
            [ Border.width 4
            , Theme.borderColor <| Theme.complicationCategoryToColor category
            ]
        , imageHeight = 400
        , image = Types.complicationNameToImage name
        , inFront =
            [ el
                [ alignBottom
                , Theme.celticHand
                , Font.size 56
                , centerX
                , moveUp 8
                ]
                (gradientText 4 (Theme.complicationCategoryToGradient category) <|
                    Types.complicationNameToString name
                )
            ]
        , content =
            case content of
                Single block ->
                    [ Theme.blocks
                        [ height fill
                        , Theme.padding
                        ]
                        block
                    ]

                _ ->
                    [ text "TODO" ]
        , onPress = msg
        }
