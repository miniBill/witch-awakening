module Generate.TypePerk exposing (TypePerksModule, file)

import Elm
import Elm.Annotation
import Elm.Arg
import Elm.Declare
import Elm.Declare.Extra
import Gen.CodeGen.Generate as Generate
import Gen.List
import Gen.Maybe
import Gen.Types
import Generate.Types exposing (TypesModule)
import Generate.Utils exposing (yassify)
import Maybe.Extra
import Parsers
import ResultME exposing (ResultME)
import String.Extra


type alias TypePerksModule =
    { all : Elm.Expression
    , details : Elm.Annotation.Annotation
    }


file : TypesModule -> List ( Maybe String, Parsers.Race ) -> ResultME Generate.Error (Elm.Declare.Module TypePerksModule)
file types dlcRaces =
    dlcToTypePerks types dlcRaces
        |> Result.map
            (\declarations ->
                Elm.Declare.module_ [ "Generated", "TypePerk" ] TypePerksModule
                    |> Elm.Declare.with (all types dlcRaces)
                    |> Elm.Declare.with (details types)
                    |> Elm.Declare.Extra.withDeclarations declarations
            )


details :
    TypesModule
    ->
        { annotation : Elm.Annotation.Annotation
        , declaration : Elm.Declaration
        , internal : Elm.Declare.Internal Elm.Annotation.Annotation
        , make :
            { content : Elm.Expression
            , cost : Elm.Expression
            , dlc : Elm.Expression
            , gain : Elm.Expression
            , race : Elm.Expression
            , name : Elm.Expression
            }
            -> Elm.Expression
        }
details types =
    Elm.Declare.Extra.customRecord "Details"
        |> Elm.Declare.Extra.withField "race" .race types.race.annotation
        |> Elm.Declare.Extra.withField "name" .name (Elm.Annotation.maybe Elm.Annotation.string)
        |> Elm.Declare.Extra.withField "gain" .gain (Elm.Annotation.list Gen.Types.annotation_.rankedMagic)
        |> Elm.Declare.Extra.withField "cost" .cost Elm.Annotation.int
        |> Elm.Declare.Extra.withField "content" .content Elm.Annotation.string
        |> Elm.Declare.Extra.withField "dlc" .dlc (Elm.Annotation.maybe Elm.Annotation.string)
        |> Elm.Declare.Extra.buildCustomRecord


all : TypesModule -> List ( Maybe String, Parsers.Race ) -> Elm.Declare.Value
all types dlcRaces =
    dlcRaces
        |> List.sortBy (\( dlc, _ ) -> Maybe.withDefault "" dlc)
        |> List.filterMap
            (\( _, race ) ->
                if race.perk == Nothing then
                    Nothing

                else
                    yassify race.name
                        |> String.Extra.decapitalize
                        |> Elm.val
                        |> Just
            )
        |> Elm.list
        |> Gen.List.call_.sortBy
            (Elm.fn
                (Elm.Arg.record identity
                    |> Elm.Arg.field "dlc"
                )
                (\dlc -> Gen.Maybe.withDefault (Elm.string "") dlc)
            )
        |> Elm.withType (Elm.Annotation.list (details types).annotation)
        |> Elm.Declare.value "all"


dlcToTypePerks : TypesModule -> List ( Maybe String, Parsers.Race ) -> ResultME Generate.Error (List Elm.Declaration)
dlcToTypePerks types races =
    ResultME.combineMap
        (\( dlcName, race ) ->
            case race.perk of
                Nothing ->
                    Ok Nothing

                Just perk ->
                    perkToDeclaration types dlcName race perk
                        |> Result.map Just
        )
        races
        |> Result.map Maybe.Extra.values


perkToDeclaration :
    TypesModule
    -> Maybe String
    -> Parsers.Race
    ->
        { name : Maybe String
        , gain : Maybe String
        , description : String
        , cost : Int
        }
    -> ResultME Generate.Error Elm.Declaration
perkToDeclaration types dlcName race perk =
    let
        gainResult =
            case perk.gain of
                Nothing ->
                    Ok (Elm.list [])

                Just gain ->
                    gain
                        |> String.split ","
                        |> ResultME.combineMap
                            (\rawPiece ->
                                let
                                    parsed =
                                        case rawPiece |> String.trim |> String.split " " of
                                            [ pieceNameString, pieceValueString ] ->
                                                String.toInt pieceValueString
                                                    |> Maybe.map
                                                        (\pieceValue ->
                                                            Gen.Types.make_.rankedMagic
                                                                { name = types.magic.value pieceNameString
                                                                , rank = Elm.int pieceValue
                                                                }
                                                        )

                                            _ ->
                                                Nothing
                                in
                                case parsed of
                                    Just p ->
                                        Ok p

                                    Nothing ->
                                        ResultME.error
                                            { title = "Could not parse type perk"
                                            , description = "Could not parse type perk gain: " ++ rawPiece
                                            }
                            )
                        |> Result.map Elm.list
    in
    gainResult
        |> Result.map
            (\gain ->
                (details types).make
                    { race =
                        case race.elements of
                            [] ->
                                Elm.apply (types.race.value race.name) [ types.affinity.value "All", types.affinity.value "All" ]

                            [ _ ] ->
                                Elm.apply (types.race.value race.name) [ types.affinity.value "All" ]

                            _ ->
                                types.race.value race.name
                    , name = Elm.maybe (Maybe.map Elm.string perk.name)
                    , gain = gain
                    , content = Elm.string perk.description
                    , cost = Elm.int perk.cost
                    , dlc = Elm.maybe (Maybe.map Elm.string dlcName)
                    }
                    |> Elm.declaration (yassify race.name)
                    |> Elm.expose
            )
