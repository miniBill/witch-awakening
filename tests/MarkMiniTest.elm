module MarkMiniTest exposing (suite)

import Expect
import MarkMini
import Parser exposing ((|.))
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Mark mini"
        [ test "_[30]_" <|
            \_ ->
                "_[30]_"
                    |> Parser.run (MarkMini.blockParser |. Parser.end)
                    |> Expect.equal
                        (MarkMini.Paragraph
                            { pieces =
                                [ MarkMini.Italic
                                    [ MarkMini.Power "30" ]
                                ]
                            , center = False
                            , mono = False
                            }
                            |> Ok
                        )
        , test "~~Mother~~" <|
            \_ ->
                "~~Mother~~"
                    |> Parser.run (MarkMini.blockParser |. Parser.end)
                    |> Expect.equal
                        (MarkMini.Paragraph
                            { pieces = [ MarkMini.Strikethrough [ MarkMini.Text "Mother" ] ]
                            , center = False
                            , mono = False
                            }
                            |> Ok
                        )
        , test "+/- Will mould you into a competent hunter through whatever means she deems necessary, won't hesitate to use tough love. ~~Mother~~ teacher knows best." <|
            \_ ->
                "+/- Will mould you into a competent hunter through whatever means she deems necessary, won't hesitate to use tough love. ~~Mother~~ teacher knows best."
                    |> Parser.run (MarkMini.blockParser |. Parser.end)
                    |> Expect.equal
                        (MarkMini.Paragraph
                            { pieces =
                                [ MarkMini.Text "+/- Will mould you into a competent hunter through whatever means she deems necessary, won't hesitate to use tough love. "
                                , MarkMini.Strikethrough [ MarkMini.Text "Mother" ]
                                , MarkMini.Text " teacher knows best."
                                ]
                            , center = False
                            , mono = False
                            }
                            |> Ok
                        )
        ]
