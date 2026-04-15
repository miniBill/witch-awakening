module Generate.Enum exposing (Argument(..), Enum, Variant, toAnnotation, toString)

import Elm
import Elm.Annotation
import Elm.Arg
import Elm.Case
import Elm.Declare
import Elm.Op.Extra
import Gen.List
import Gen.String
import Generate.Utils as Utils
import String.Extra


type alias Enum =
    { name : String
    , variants : List Variant
    , toImage : Bool
    , isSame : Bool
    }


type alias Variant =
    { name : String
    , arguments : List Argument
    , dlc : Maybe String
    , toStringException : Maybe String
    }


type Argument
    = ValueArgument String
    | ListArgument String


toString : Enum -> Elm.Declare.Function (Elm.Expression -> Elm.Expression)
toString enum =
    let
        lowerName : String
        lowerName =
            String.Extra.decapitalize enum.name

        type_ : Elm.Annotation.Annotation
        type_ =
            Elm.Annotation.named [ "Generated", "Types" ] enum.name
    in
    (\value ->
        let
            variantToBranch : Variant -> Elm.Case.Branch
            variantToBranch variant =
                let
                    variantString : Elm.Expression
                    variantString =
                        variant.toStringException
                            |> Maybe.withDefault variant.name
                            |> Elm.string
                in
                Elm.Case.branch
                    (Elm.Arg.customType (enum.name ++ Utils.yassify variant.name) identity
                        |> Elm.Arg.items
                            (List.map
                                (\t ->
                                    case t of
                                        ValueArgument v ->
                                            Elm.Arg.var v

                                        ListArgument v ->
                                            Elm.Arg.var v
                                )
                                variant.arguments
                            )
                    )
                <|
                    \vals ->
                        (variantString
                            :: List.map2
                                (\arg val ->
                                    case arg of
                                        ValueArgument i ->
                                            Elm.apply
                                                (Elm.value
                                                    { importFrom = [ "Generated", i ]
                                                    , name = "toString"
                                                    , annotation = Nothing
                                                    }
                                                )
                                                [ val ]

                                        ListArgument i ->
                                            val
                                                |> Gen.List.call_.map
                                                    (Elm.value
                                                        { importFrom = [ "Generated", i ]
                                                        , name = "toString"
                                                        , annotation = Nothing
                                                        }
                                                    )
                                                |> Gen.String.call_.join (Elm.string ",")
                                )
                                variant.arguments
                                vals
                        )
                            |> List.intersperse (Elm.string "-")
                            |> Elm.Op.Extra.appendsStrings
        in
        enum.variants
            |> List.map variantToBranch
            |> Elm.Case.custom value type_
    )
        |> Elm.Declare.fn "toString" (Elm.Arg.varWith lowerName type_)


toAnnotation : Enum -> Elm.Declare.Annotation
toAnnotation enum =
    enum.variants
        |> List.map
            (\variant ->
                variant.arguments
                    |> List.map
                        (\t ->
                            case t of
                                ListArgument i ->
                                    Elm.Annotation.list (Elm.Annotation.named [] i)

                                ValueArgument i ->
                                    Elm.Annotation.named [] i
                        )
                    |> Elm.variantWith (enum.name ++ Utils.yassify variant.name)
            )
        |> Elm.Declare.customType enum.name
        |> Elm.Declare.exposeConstructor
