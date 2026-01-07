module Generate.Magic exposing (MagicModule, file)

import Dict
import Elm
import Elm.Annotation
import Elm.Declare
import Elm.Declare.Extra
import Gen.Data.Magic
import Generate.Enum as Enum exposing (Enum)
import Generate.Types exposing (TypesModule)
import Generate.Utils exposing (yassify)
import Parsers
import String.Extra


type alias MagicModule =
    { all : Elm.Expression
    , toString : Elm.Expression -> Elm.Expression
    }


file : TypesModule -> Enum -> List ( Maybe String, Parsers.Magic ) -> Elm.Declare.Module MagicModule
file types enum dlcMagics =
    Elm.Declare.module_ [ "Generated", "Magic" ] MagicModule
        |> Elm.Declare.with (all dlcMagics)
        |> Elm.Declare.with (Enum.toString enum)
        |> Elm.Declare.Extra.withDeclarations (dlcToMagics types dlcMagics)


all : List ( Maybe String, Parsers.Magic ) -> Elm.Declare.Value
all dlcMagics =
    dlcMagics
        |> List.sortBy (\( dlc, _ ) -> Maybe.withDefault "" dlc)
        |> List.map
            (\( _, magic ) ->
                yassify magic.name
                    |> String.Extra.decapitalize
                    |> Elm.val
            )
        |> Elm.list
        |> Elm.withType (Elm.Annotation.list Gen.Data.Magic.annotation_.details)
        |> Elm.Declare.value "all"


dlcToMagics : TypesModule -> List ( Maybe String, Parsers.Magic ) -> List Elm.Declaration
dlcToMagics types magics =
    List.map
        (\( dlcName, magic ) ->
            let
                maxRank : Int
                maxRank =
                    magic.ranks
                        |> Dict.keys
                        |> List.maximum
                        |> Maybe.withDefault 5

                class : Elm.Expression
                class =
                    case magic.class of
                        Just "Special" ->
                            Gen.Data.Magic.make_.classSpecial

                        Just c ->
                            Gen.Data.Magic.make_.classOne (types.class.value c)

                        Nothing ->
                            Gen.Data.Magic.make_.classNone
            in
            Gen.Data.Magic.make_.details
                { name = types.magic.value magic.name
                , class = class
                , faction = Elm.maybe (Maybe.map types.faction.value magic.faction)
                , hasRankZero = Elm.bool magic.hasRankZero
                , isElementalism = Elm.bool magic.isElementalism
                , affinities = affinitiesToExpression types magic.elements
                , description = Elm.string magic.description
                , dlc = Elm.maybe (Maybe.map Elm.string dlcName)
                , ranks =
                    List.range 1 maxRank
                        |> List.map
                            (\rank ->
                                Dict.get rank magic.ranks
                                    |> Maybe.withDefault ""
                                    |> Elm.string
                            )
                        |> Elm.list
                , requires = Elm.maybe (Maybe.map Elm.string magic.requires)
                }
                |> Elm.declaration (yassify magic.name)
                |> Elm.expose
        )
        magics


affinitiesToExpression : TypesModule -> Parsers.MagicAffinity -> Elm.Expression
affinitiesToExpression types affinity =
    case affinity of
        Parsers.Regular alternatives ->
            alternatives
                |> List.map types.affinity.value
                |> Elm.list
                |> Gen.Data.Magic.make_.regular

        Parsers.Alternative alternatives ->
            alternatives
                |> List.map
                    (\alternative ->
                        alternative
                            |> List.map types.affinity.value
                            |> Elm.list
                    )
                |> Elm.list
                |> Gen.Data.Magic.make_.alternative
