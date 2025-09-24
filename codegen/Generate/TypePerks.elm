module Generate.TypePerks exposing (TypePerksModule, file)

import Elm
import Elm.Annotation
import Elm.Arg
import Elm.Declare
import Elm.Declare.Extra
import Gen.CodeGen.Generate as Generate
import Gen.Data.TypePerk
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
    }


file : TypesModule -> List ( Maybe String, Parsers.Race ) -> ResultME Generate.Error (Elm.Declare.Module TypePerksModule)
file types dlcRaces =
    dlcToTypePerks types dlcRaces
        |> Result.map
            (\declarations ->
                Elm.Declare.module_ [ "Generated", "TypePerk" ] TypePerksModule
                    |> Elm.Declare.with (all dlcRaces)
                    |> Elm.Declare.Extra.withDeclarations declarations
            )


all : List ( Maybe String, Parsers.Race ) -> Elm.Declare.Value
all dlcRaces =
    dlcRaces
        |> List.sortBy (\( dlc, _ ) -> Maybe.withDefault "" dlc)
        |> List.filterMap
            (\( _, race ) ->
                if race.perk == Nothing then
                    Nothing

                else
                    Just (Elm.val (String.Extra.decapitalize (yassify race.name)))
            )
        |> Elm.list
        |> Gen.List.call_.sortBy
            (Elm.fn
                (Elm.Arg.record identity
                    |> Elm.Arg.field "dlc"
                )
                (\dlc -> Gen.Maybe.withDefault (Elm.string "") dlc)
            )
        |> Elm.withType (Elm.Annotation.list Gen.Data.TypePerk.annotation_.details)
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
                Gen.Data.TypePerk.make_.details
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
