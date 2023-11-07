module MarkMiniTest exposing (suite)

import Expect
import MarkMini
import Parser
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Mark mini"
        [ test "_[30]_" <|
            \_ ->
                "_[30]_"
                    |> Parser.run MarkMini.blockParser
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
        ]
