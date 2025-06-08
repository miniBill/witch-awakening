module Generate.Magics exposing (MagicModule, file)

import Dict
import Elm
import Elm.Annotation
import Elm.Declare
import Elm.Declare.Extra
import Gen.Data.Magic
import Generate.Types
import Generate.Utils exposing (yassify)
import Parsers
import String.Extra


type alias MagicModule =
    { all : Elm.Expression
    }


file : List ( Maybe String, Parsers.Magic ) -> Elm.Declare.Module MagicModule
file dlcMagics =
    Elm.Declare.module_ [ "Generated", "Magic" ] MagicModule
        |> Elm.Declare.with (all dlcMagics)
        |> Elm.Declare.Extra.withDeclarations (dlcToMagics dlcMagics)


all : List ( Maybe String, Parsers.Magic ) -> Elm.Declare.Value
all dlcMagics =
    dlcMagics
        |> List.map (\( _, magic ) -> Elm.val (String.Extra.decapitalize (yassify magic.name)))
        |> Elm.list
        |> Elm.withType (Elm.Annotation.list Gen.Data.Magic.annotation_.details)
        |> Elm.Declare.value "all"


dlcToMagics : List ( Maybe String, Parsers.Magic ) -> List Elm.Declaration
dlcToMagics magics =
    List.map
        (\( dlcName, magic ) ->
            let
                maxRank : Int
                maxRank =
                    magic.ranks
                        |> Dict.keys
                        |> List.maximum
                        |> Maybe.withDefault 5
            in
            Gen.Data.Magic.make_.details
                { name = Generate.Types.value magic.name
                , class = Elm.maybe (Maybe.map Generate.Types.value magic.class)
                , faction = Elm.maybe (Maybe.map Generate.Types.value magic.faction)
                , hasRankZero = Elm.bool magic.hasRankZero
                , isElementalism = Elm.bool magic.isElementalism
                , affinities = affinitiesToExpression magic.elements
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
                }
                |> Elm.declaration (yassify magic.name)
                |> Elm.expose
        )
        magics


affinitiesToExpression : Parsers.MagicAffinity -> Elm.Expression
affinitiesToExpression affinity =
    case affinity of
        Parsers.Regular alternatives ->
            alternatives
                |> List.map Generate.Types.value
                |> Elm.list
                |> Gen.Data.Magic.make_.regular

        Parsers.Alternative alternatives ->
            alternatives
                |> List.map
                    (\alternative ->
                        alternative
                            |> List.map Generate.Types.value
                            |> Elm.list
                    )
                |> Elm.list
                |> Gen.Data.Magic.make_.alternative
