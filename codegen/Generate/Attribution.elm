module Generate.Attribution exposing (AttributionModule, DLCAttribution, file)

import Elm
import Elm.Annotation as Annotation
import Elm.Declare


type alias DLCAttribution =
    { name : String
    , author : String
    , link : Maybe String
    }


type alias AttributionModule =
    { all : Elm.Expression }


file : List DLCAttribution -> Elm.Declare.Module AttributionModule
file dlcAttributions =
    Elm.Declare.module_ [ "Generated", "Attribution" ] AttributionModule
        |> Elm.Declare.with (all dlcAttributions)


all : List DLCAttribution -> Elm.Declare.Value
all dlcAttributions =
    dlcAttributions
        |> List.map
            (\{ name, author, link } ->
                Elm.record
                    [ ( "name", Elm.string name )
                    , ( "author", Elm.string author )
                    , ( "link"
                      , Elm.maybe (Maybe.map Elm.string link)
                            |> Elm.withType (Annotation.maybe Annotation.string)
                      )
                    ]
            )
        |> Elm.list
        |> Elm.Declare.value "all"
