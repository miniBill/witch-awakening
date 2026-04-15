module Generate.Faction exposing (FactionsModule, file)

import Elm
import Elm.Annotation
import Elm.Arg
import Elm.Case
import Elm.Declare
import Elm.Declare.Extra
import Gen.List
import Gen.Maybe
import Generate.Enum as Enum exposing (Enum)
import Generate.Image exposing (ImageModule)
import Generate.Types exposing (TypesModule)
import Generate.Utils exposing (yassify)
import Parsers
import String.Extra


type alias FactionsModule =
    { all : Elm.Expression
    , toString : Elm.Expression -> Elm.Expression
    , toShortString : Elm.Expression -> Elm.Expression
    , toCollectiveName : Elm.Expression -> Elm.Expression
    , details : Elm.Annotation.Annotation
    , images : Elm.Annotation.Annotation
    }


file : TypesModule -> ImageModule -> Enum -> List ( Maybe String, Parsers.Faction ) -> Elm.Declare.Module FactionsModule
file types image enum dlcFactions =
    Elm.Declare.module_ [ "Generated", "Faction" ] FactionsModule
        |> Elm.Declare.with (all types dlcFactions)
        |> Elm.Declare.with (Enum.toString enum)
        |> Elm.Declare.with (toShortString types dlcFactions)
        |> Elm.Declare.with (toCollectiveName types dlcFactions)
        |> Elm.Declare.with (details types)
        |> Elm.Declare.with images
        |> Elm.Declare.withDeclarations (dlcToFactions types image dlcFactions)


details :
    TypesModule
    ->
        Elm.Declare.Extra.Record
            { name : Elm.Expression
            , motto : Elm.Expression
            , isHuman : Elm.Expression
            , description : Elm.Expression
            , location : Elm.Expression
            , relations : Elm.Expression
            , perk : Elm.Expression
            , perkContent : Elm.Expression
            , dlc : Elm.Expression
            , images : Elm.Expression
            }
details types =
    Elm.Declare.record "Details"
        |> Elm.Declare.withField "name" .name types.faction.annotation
        |> Elm.Declare.withField "motto" .motto Elm.Annotation.string
        |> Elm.Declare.withField "isHuman" .isHuman Elm.Annotation.bool
        |> Elm.Declare.withField "description" .description Elm.Annotation.string
        |> Elm.Declare.withField "location" .location Elm.Annotation.string
        |> Elm.Declare.withField "relations" .relations Elm.Annotation.string
        |> Elm.Declare.withField "perk" .perk Elm.Annotation.string
        |> Elm.Declare.withField "perkContent" .perkContent Elm.Annotation.string
        |> Elm.Declare.withField "dlc" .dlc (Elm.Annotation.maybe Elm.Annotation.string)
        |> Elm.Declare.withField "images" .images images.annotation
        |> Elm.Declare.buildRecord


images : Elm.Declare.Annotation
images =
    Elm.Declare.alias "Images"
        (List.range 1 5
            |> List.map (\i -> ( "image" ++ String.fromInt i, Elm.Annotation.namedWith [ "Generated", "Image" ] "Image" [] ))
            |> Elm.Annotation.record
        )


all : TypesModule -> List ( Maybe String, Parsers.Faction ) -> Elm.Declare.Value
all types dlcFactions =
    dlcFactions
        |> List.sortBy (\( dlc, _ ) -> Maybe.withDefault "" dlc)
        |> List.map
            (\( _, faction ) ->
                yassify faction.name
                    |> String.Extra.decapitalize
                    |> Elm.val
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


toShortString : TypesModule -> List ( Maybe String, Parsers.Faction ) -> Elm.Declare.Function (Elm.Expression -> Elm.Expression)
toShortString types dlcFactions =
    Elm.Declare.fn "toShortString" (Elm.Arg.var "faction") <|
        \factionArg ->
            dlcFactions
                |> List.sortBy (\( dlc, _ ) -> Maybe.withDefault "" dlc)
                |> List.map
                    (\( _, faction ) ->
                        Elm.Case.branch (types.faction.argWith faction.name []) (\_ -> Elm.string faction.shortName)
                    )
                |> (\l ->
                        l
                            ++ [ Elm.Case.branch (types.faction.argWith "Independents" []) (\_ -> Elm.string "Independents") ]
                   )
                |> Elm.Case.custom factionArg types.faction.annotation


toCollectiveName : TypesModule -> List ( Maybe String, Parsers.Faction ) -> Elm.Declare.Function (Elm.Expression -> Elm.Expression)
toCollectiveName types dlcFactions =
    Elm.Declare.fn "toCollectiveName" (Elm.Arg.var "faction") <|
        \factionArg ->
            dlcFactions
                |> List.sortBy (\( dlc, _ ) -> Maybe.withDefault "" dlc)
                |> List.map
                    (\( _, faction ) ->
                        Elm.Case.branch
                            (Elm.Arg.customType "Just" identity
                                |> Elm.Arg.item (types.faction.argWith faction.name [])
                            )
                        <|
                            \_ -> Elm.string faction.collectiveName
                    )
                |> (\l ->
                        l
                            ++ [ Elm.Case.branch
                                    (Elm.Arg.customType "Just" identity
                                        |> Elm.Arg.item (types.faction.argWith "Independents" [])
                                    )
                                 <|
                                    \_ -> Elm.string "Independents / Other"
                               , Elm.Case.branch (Elm.Arg.customType "Nothing" ()) <|
                                    \_ -> Elm.string "Independents / Other"
                               ]
                   )
                |> Elm.Case.custom factionArg (Elm.Annotation.maybe types.faction.annotation)


dlcToFactions : TypesModule -> ImageModule -> List ( Maybe String, Parsers.Faction ) -> List Elm.Declaration
dlcToFactions types image factions =
    List.map
        (\( dlcName, faction ) ->
            (details types).make
                { name = types.faction.value faction.name
                , motto = Elm.string faction.motto
                , description = Elm.string faction.description
                , location = Elm.string faction.location
                , relations = Elm.string faction.relations
                , perk = Elm.string faction.perk
                , perkContent = Elm.string faction.perkContent
                , isHuman = Elm.bool faction.isHuman
                , dlc = Elm.maybe (Maybe.map Elm.string dlcName)
                , images = image.valueFrom ("faction" ++ yassify faction.name)
                }
                |> Elm.declaration (yassify faction.name)
                |> Elm.expose
        )
        factions
