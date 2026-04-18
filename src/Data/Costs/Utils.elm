module Data.Costs.Utils exposing (Requisite(..), Requisites(..), affinityDiscountIf, applyClassBonusIf, capWithWarning, checkRequisites, find, hasMagicAtRank, requisitesParser, slotUnsupported)

import Data.Affinity exposing (InAffinity(..))
import Data.Costs.Monad as Monad exposing (Monad)
import Data.Costs.Points as Points exposing (Points)
import Generated.Class as Class
import Generated.Magic as Magic
import Generated.Perk as Perk
import Generated.Quest as Quest
import Generated.Types as Types exposing (Class, Magic, Perk, Quest)
import List.Extra
import Parser exposing ((|.), (|=), Parser)
import Types exposing (RankedMagic, RankedPerk)


applyClassBonusIf : Bool -> Int -> Int
applyClassBonusIf isClass cost =
    if isClass then
        cost + 2

    else
        cost


find : String -> (item -> key) -> key -> List item -> (key -> String) -> Monad item
find label toKey value list toString =
    case List.Extra.find (\candidate -> toKey candidate == value) list of
        Nothing ->
            Monad.error <| label ++ " " ++ toString value ++ " not found"

        Just v ->
            Monad.succeed v


slotUnsupported : Monad value
slotUnsupported =
    Monad.error "Slot modes not supported yet"


{-| Cap a value to a maximum. Emit a warning if the maximum is exceeded by the input.
-}
capWithWarning : Int -> String -> Int -> Monad Points
capWithWarning cap warning value =
    if value > cap then
        Points.fromPower cap
            |> Monad.succeed
            |> Monad.withWarning warning

    else
        Points.fromPower value
            |> Monad.succeed


affinityDiscountIf : InAffinity -> Int -> Int
affinityDiscountIf inAffinity value =
    if value >= 0 then
        value

    else
        case inAffinity of
            DoubleAffinity ->
                -- We're rounding down the _second_ halving but not the first,
                -- hence why the -1
                (value - 1) // 4

            InAffinity ->
                (value - 1) // 2

            OffAffinity ->
                value


requisiteParser : Parser Requisite
requisiteParser =
    let
        options :
            (name -> res)
            -> (name -> String)
            -> List { kind | name : name }
            -> Parser res
        options variant toString list =
            list
                |> List.map
                    (\m ->
                        let
                            nameString : String
                            nameString =
                                toString m.name
                        in
                        Parser.succeed (variant m.name)
                            |. Parser.keyword
                                (if String.endsWith "-" nameString then
                                    String.slice 0 -1 nameString

                                 else
                                    nameString
                                )
                    )
                |> Parser.oneOf

        magicParser : Parser Magic
        magicParser =
            options identity Magic.toString Magic.all

        classParser : Parser Requisite
        classParser =
            options RequiresClass Class.toString Class.all

        questParser : Parser Requisite
        questParser =
            options RequiresQuest Quest.toString Quest.all

        perkParser : Parser Requisite
        perkParser =
            options RequiresPerk Perk.toString (Perk.all [])
    in
    Parser.oneOf
        [ Parser.succeed RequiresMagic
            |= magicParser
            |. Parser.spaces
            |= Parser.oneOf
                [ Parser.succeed identity
                    |= Parser.int
                    |. Parser.oneOf
                        [ Parser.succeed () |. Parser.symbol "+"
                        , Parser.succeed ()
                        ]
                , Parser.succeed 1
                ]
        , classParser
        , questParser
        , perkParser
        ]


checkRequisites :
    { a | requires : Maybe String }
    -> String
    ->
        { model
            | class : Maybe Class
            , magic : List RankedMagic
            , quests : List Quest
            , perks : List RankedPerk
        }
    -> c
    -> Monad c
checkRequisites details nameString model res =
    case details.requires of
        Nothing ->
            Monad.succeed res

        Just req ->
            case Parser.run requisitesParser req of
                Err _ ->
                    Monad.succeed res
                        |> Monad.withWarning ("Failed to parse requisites for " ++ nameString ++ ": " ++ req)

                Ok requisites ->
                    let
                        missingRequisites : Requisites -> List String
                        missingRequisites rqs =
                            case rqs of
                                RequiresAllOf ands ->
                                    List.concatMap missingRequisites ands

                                RequiresAnyOf [] ->
                                    []

                                RequiresAnyOf (h :: t) ->
                                    List.foldl
                                        (\e a ->
                                            if List.isEmpty a then
                                                a

                                            else
                                                missingRequisites e
                                        )
                                        (missingRequisites h)
                                        t

                                Requires requisite ->
                                    let
                                        check : Result String ()
                                        check =
                                            checkRequisite model requisite
                                    in
                                    case check of
                                        Ok _ ->
                                            []

                                        Err e ->
                                            [ e ]
                    in
                    case missingRequisites requisites of
                        [] ->
                            Monad.succeed res

                        [ missing ] ->
                            Monad.succeed res
                                |> Monad.withWarning
                                    ("Missing requisite for "
                                        ++ nameString
                                        ++ ": "
                                        ++ missing
                                    )

                        missing ->
                            Monad.succeed res
                                |> Monad.withWarning
                                    ("Missing requisites for "
                                        ++ nameString
                                        ++ ": "
                                        ++ String.join ", " missing
                                    )


checkRequisite :
    { model
        | class : Maybe Class
        , magic : List RankedMagic
        , quests : List Quest
        , perks : List RankedPerk
    }
    -> Requisite
    -> Result String ()
checkRequisite model requisite =
    case requisite of
        RequiresMagic requiredName requiredRank ->
            if hasMagicAtRank model requiredName requiredRank then
                Ok ()

            else
                Err (Magic.toString requiredName ++ " " ++ String.fromInt requiredRank)

        RequiresClass class ->
            if model.class == Just class then
                Ok ()

            else
                Err (Class.toString class)

        RequiresQuest quest ->
            if List.member quest model.quests then
                Ok ()

            else
                Err (Quest.toString quest)

        RequiresPerk perk ->
            if List.any (\p -> Types.isSamePerk p.name perk) model.perks then
                Ok ()

            else
                Err (Perk.toString perk)


type Requisites
    = RequiresAllOf (List Requisites)
    | RequiresAnyOf (List Requisites)
    | Requires Requisite


requisitesParser : Parser Requisites
requisitesParser =
    (Parser.sequence
        { start = ""
        , end = ""
        , trailing = Parser.Forbidden
        , spaces = Parser.spaces
        , item = requisiteDisjunctionParser
        , separator = ","
        }
        |> Parser.map
            (\r ->
                case r of
                    [ s ] ->
                        s

                    _ ->
                        RequiresAllOf r
            )
    )
        |. Parser.end


requisiteDisjunctionParser : Parser Requisites
requisiteDisjunctionParser =
    Parser.sequence
        { start = ""
        , end = ""
        , trailing = Parser.Forbidden
        , spaces = Parser.spaces
        , item = requisiteParser |> Parser.map Requires
        , separator = "or"
        }
        |> Parser.map
            (\r ->
                case r of
                    [ s ] ->
                        s

                    _ ->
                        RequiresAnyOf r
            )


hasMagicAtRank : { a | magic : List RankedMagic } -> Magic -> Int -> Bool
hasMagicAtRank model requiredName requiredRank =
    List.any
        (\rankedMagic ->
            rankedMagic.name == requiredName && rankedMagic.rank >= requiredRank
        )
        model.magic


type Requisite
    = RequiresMagic Magic Int
    | RequiresClass Class
    | RequiresPerk Perk
    | RequiresQuest Types.Quest
