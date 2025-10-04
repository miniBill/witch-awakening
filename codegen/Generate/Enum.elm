module Generate.Enum exposing (Enum, Variant, toAnnotation, toString)

import Elm
import Elm.Annotation
import Elm.Arg
import Elm.Case
import Elm.Declare
import Elm.Declare.Extra
import Elm.Op.Extra
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
    , arguments : List String
    , dlc : Maybe String
    , toStringException : Maybe String
    }


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
                        |> Elm.Arg.items (List.map Elm.Arg.var variant.arguments)
                    )
                <|
                    \vals ->
                        (variantString
                            :: List.map2
                                (\arg val ->
                                    Elm.apply
                                        (Elm.value
                                            { importFrom = [ "Generated", arg ]
                                            , name = "toString"
                                            , annotation = Nothing
                                            }
                                        )
                                        [ val ]
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
                    |> List.map (Elm.Annotation.named [])
                    |> Elm.variantWith (enum.name ++ Utils.yassify variant.name)
            )
        |> Elm.Declare.customType enum.name
        |> Elm.Declare.Extra.exposeConstructor
