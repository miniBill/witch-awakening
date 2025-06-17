module Generate.Perks exposing (PerkModule, file)

import Elm
import Elm.Annotation
import Elm.Arg
import Elm.Declare
import Elm.Declare.Extra
import Elm.Op
import Gen.Data.Perk
import Gen.Types
import Generate.Types exposing (TypesModule)
import Generate.Utils exposing (yassify)
import Parsers exposing (Content(..))
import String.Extra


type alias PerkModule =
    { all : Elm.Expression -> Elm.Expression
    }


file : TypesModule -> List ( Maybe String, Parsers.Perk ) -> Elm.Declare.Module PerkModule
file types dlcPerks =
    Elm.Declare.module_ [ "Generated", "Perk" ] PerkModule
        |> Elm.Declare.with (all dlcPerks)
        |> Elm.Declare.Extra.withDeclarations (dlcToPerks types dlcPerks)


all : List ( Maybe String, Parsers.Perk ) -> Elm.Declare.Function (Elm.Expression -> Elm.Expression)
all dlcPerks =
    Elm.Declare.fn "all"
        (Elm.Arg.varWith "perks"
            (Elm.Annotation.list Gen.Types.annotation_.rankedPerk)
        )
    <|
        \perks ->
            Elm.Op.append
                (Gen.Data.Perk.call_.all perks)
                (dlcPerks
                    |> List.sortBy (\( dlc, _ ) -> Maybe.withDefault "" dlc)
                    |> List.map (\( _, perk ) -> Elm.val (String.Extra.decapitalize (yassify perk.name)))
                    |> Elm.list
                )
                |> Elm.withType (Elm.Annotation.list Gen.Data.Perk.annotation_.details)


dlcToPerks : TypesModule -> List ( Maybe String, Parsers.Perk ) -> List Elm.Declaration
dlcToPerks types perks =
    List.map
        (\( dlcName, perk ) ->
            Gen.Data.Perk.make_.details
                { name = types.valueFrom perk.name
                , class = types.valueFrom perk.class
                , affinity = types.valueFrom perk.element
                , isMeta = Elm.bool perk.isMeta
                , content =
                    case perk.content of
                        Parsers.Single cost description ->
                            Gen.Data.Perk.make_.single (Elm.int cost) (Elm.string description)

                        Parsers.WithCosts costs description ->
                            Gen.Data.Perk.make_.withCosts (Elm.list (List.map Elm.int costs)) (Elm.string description)

                        Parsers.WithChoices () before choices after ->
                            Gen.Data.Perk.make_.withChoices
                                (Elm.string before)
                                (choices
                                    |> List.map
                                        (\( choice, cost ) -> Elm.tuple (Elm.string choice) (Elm.int cost))
                                    |> Elm.list
                                )
                                (Elm.string after)
                , dlc = Elm.maybe (Maybe.map Elm.string dlcName)
                }
                |> Elm.declaration (yassify perk.name)
                |> Elm.expose
        )
        perks
