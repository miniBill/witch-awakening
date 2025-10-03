module View.Race exposing (raceToShortString, viewRace)

import Color
import Data.Affinity as Affinity
import Data.Race as Race
import Element exposing (Attribute, Element, alignTop, centerX, el, fill, moveDown, moveRight, moveUp, rgb, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Generated.Race
import Generated.Types as Types exposing (Affinity(..), Race(..), Size)
import Gradients
import Images exposing (Image)
import List.Extra
import Set exposing (Set)
import Theme
import Types exposing (Choice(..), Display, IdKind(..))
import View
import View.Affinity as Affinity


viewRace : Set String -> Display -> List Race -> Element Choice
viewRace hideDLC display races =
    let
        filtered : List Race.Details
        filtered =
            Generated.Race.all races
                |> View.filterDLC hideDLC
    in
    if List.isEmpty filtered then
        Element.none

    else
        let
            raceBoxes : Element ( Race, Bool )
            raceBoxes =
                filtered
                    |> List.filterMap (raceBox display races)
                    |> Theme.wrappedRow
                        [ width fill
                        , spacing <| Theme.rhythm * 3
                        , Theme.centerWrap
                        ]
        in
        View.collapsible []
            display
            DisplayRace
            ChoiceRace
            IdKindRace
            "# True Form - Race"
            [ Theme.blocks [] IdKindRace intro
            , raceBoxes
            ]
            [ raceBoxes
            ]


raceBox :
    Display
    -> List Race
    -> Race.Details
    -> Maybe (Element ( Race, Bool ))
raceBox display selected { name, tank, affinities, charge, content, dlc } =
    let
        isSelected : Bool
        isSelected =
            List.member name selected

        shortName : String
        shortName =
            raceToShortString name
    in
    Theme.card [ Theme.id IdKindRace shortName ]
        { display = display
        , forceShow = List.isEmpty selected
        , glow = Color.rgb255 0xF3 0xEA 0x6F
        , isSelected = isSelected
        , imageAttrs = []
        , imageHeight = 600
        , image = Types.raceToImage name
        , inFront =
            [ Theme.gradientTextWrapped
                [ alignTop
                , Theme.captureIt
                , if String.length shortName > 10 then
                    Font.size 46

                  else if String.length shortName > 8 then
                    Font.size 52

                  else
                    Font.size 56
                , centerX
                ]
                6
                Gradients.yellowGradient
                shortName
            , case dlc of
                Nothing ->
                    Element.none

                Just dlcName ->
                    el
                        [ centerX
                        , Theme.captureIt
                        , Font.size 24
                        , moveDown 60
                        ]
                        (Theme.gradientText 4 Gradients.purpleGradient dlcName)
            ]
        , content =
            Theme.row [ centerX ]
                [ viewTank tank
                , viewAffinities affinities
                , viewCharge charge
                ]
                :: Theme.blocks [] IdKindRace content
                :: affinityPicker selected name
        , onPress =
            case ( name, isSelected ) of
                ( RaceDravir _, False ) ->
                    Nothing

                ( RaceGenie _, False ) ->
                    Nothing

                ( RaceGemini _, False ) ->
                    Nothing

                _ ->
                    Just ( name, not isSelected )
        }


raceToShortString : Race -> String
raceToShortString name =
    Types.raceToString name
        |> String.split "-"
        |> List.take 1
        |> String.concat


affinityPicker : List Race -> Race -> List (Element ( Race, Bool ))
affinityPicker selected race =
    let
        innerPicker : (Affinity -> Race) -> Affinity -> List Affinity -> Element ( Race, Bool )
        innerPicker ctor currentAffinity list =
            list
                |> List.map
                    (\affinity ->
                        let
                            isSelected : Bool
                            isSelected =
                                affinity == currentAffinity

                            msg : ( Race, Bool )
                            msg =
                                ( ctor affinity, not isSelected )
                        in
                        Affinity.button isSelected msg affinity
                    )
                |> Theme.wrappedRow []

        picker : (Affinity -> Race) -> Affinity -> List Affinity -> List (Element ( Race, Bool ))
        picker ctor currentAffinity list =
            [ el [ Font.bold ] <| text "Pick an affinity:"
            , innerPicker ctor currentAffinity list
            ]
    in
    case race of
        RaceAmalgam currentAffinity1 currentAffinity2 ->
            let
                otherRaces : List Race
                otherRaces =
                    selected
                        |> List.Extra.removeWhen
                            (\r ->
                                case r of
                                    RaceAmalgam _ _ ->
                                        True

                                    _ ->
                                        False
                            )

                firstRaceAffinities : List Affinity
                firstRaceAffinities =
                    List.head otherRaces
                        |> Maybe.map Affinity.affinitiesForRace
                        |> Maybe.withDefault Affinity.defaultList
                        |> Affinity.toList

                secondRaceAffinities : List Affinity
                secondRaceAffinities =
                    List.head (List.drop 1 otherRaces)
                        |> Maybe.map Affinity.affinitiesForRace
                        |> Maybe.withDefault Affinity.defaultList
                        |> Affinity.toList
            in
            [ el [ Font.bold ] <| text "Pick an affinity from the first race:"
            , innerPicker (\newAffinity -> RaceAmalgam newAffinity currentAffinity2) currentAffinity1 firstRaceAffinities
            , el [ Font.bold ] <| text "Pick an affinity from the second race:"
            , innerPicker (\newAffinity -> RaceAmalgam currentAffinity1 newAffinity) currentAffinity2 secondRaceAffinities
            ]

        RaceDravir currentAffinity ->
            picker RaceDravir
                currentAffinity
                [ AffinityFire
                , AffinityWind
                , AffinityWater
                , AffinityEarth
                , AffinityMetal
                , AffinityNature
                ]

        RaceGenie currentAffinity ->
            Affinity.selectable
                |> List.map .name
                |> picker RaceGenie currentAffinity

        RaceGemini currentAffinity ->
            Affinity.selectable
                |> List.map .name
                |> List.Extra.remove AffinityEarth
                |> picker RaceGemini currentAffinity

        _ ->
            []


viewAffinities : List Affinity -> Element ( Race, Bool )
viewAffinities affinities =
    Theme.row
        [ moveDown 2
        , Border.width 4
        , Border.color <| rgb 0 0 0
        , Border.rounded 999
        , Background.color <| rgb 0 0 0
        ]
        (List.map Theme.viewAffinity affinities)


viewTank : Size -> Element msg
viewTank size =
    viewSize []
        Images.tank
        (List.map
            (\( r, g, b ) -> ( r // 2, g * 3 // 5, b ))
            Gradients.blueGradient
        )
        size


viewCharge : Size -> Element msg
viewCharge size =
    viewSize [ moveRight 30 ]
        Images.charge
        Gradients.yellowGradient
        size


viewSize :
    List (Attribute msg)
    -> Image
    -> List ( Int, Int, Int )
    -> Size
    -> Element msg
viewSize attrs image gradient size =
    Theme.viewSize gradient size
        |> el
            ([ Font.size 20
             , Element.onLeft <| Theme.image [ moveUp 10 ] image
             ]
                ++ attrs
            )


intro : String
intro =
    """
    "This is my favorite part, so don’t zone out on me: Even if most people prefer to be a neutral it’s fun to see what other option or two a person might have. Let’s see... oh, oh. ooh" The witch shudders with quite the enthusiasm, and continues "_This is ten times_ the most options I’ve seen in another witch. _*You have so many possibilities*_! Let’s explore them... I haven’t even seen some of these before, in person _or_ in an awakening ritual. I’m a fan of #7 in particular, they’re so cute." This is the physical expression of your True Form, what you would revert to if any effect were to reveal the true nature of a shapechanger for example. Your old body was just a temporary vessel for this true form. In your change, {choice you can totally remake your body within human limits}. However, witches are female by default, but you can take the perk _Elephant Trunk_ to be male if you wish, or the _Transformation Sequence_ perk to transform into your past human vessel and back. Note with the options presented you can become another non-human species as a Witch of that species, ie; A witch can be a naiad but not all naiads are witches. Every different race of witch has a different source of Mana as well as some unique benefits and base abilities. All witches except Neutral and partially Daeva share some degree of _inherent_ life extension, though there are many other methods any witch can learn or obtain later on regardless of their race. {choice *The blue potion icon represents Mana, the yellow bolt icon represents Charge rate. _This uses a Relative system and not absolute value, don’t overthink it_*}, it’s a representation of what you can expect in comparing your witch race to an average of other witches _of your same general power level_. High mana represents high storage, & charge represents the time it takes your method to fill your mana stores though it can often scale with overexposure, and stack.

    This is very simply just to give you a suggestion for perspective on how much magic you could expect to use. High Mana means a bigger tank, but charge is how fast it is filled. So high tank might be good for going all in while a low tank but high charge might have the endurance to go for longer. This is roughly balanced by how easy the charge source is to come by and how well it can stack up on itself.
    """
