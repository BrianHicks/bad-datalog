module Datalog.ParserTest exposing (..)

import Datalog
import Datalog.Parser exposing (..)
import Expect
import Test exposing (..)


fromStringTest : Test
fromStringTest =
    describe "fromString"
        [ test "accepts a head-only rule" <|
            \_ ->
                fromString "parent_child(\"Nate\", \"Brian\")."
                    |> Expect.equal
                        (Ok
                            [ Datalog.Rule
                                (Datalog.Atom
                                    "parent_child"
                                    [ Datalog.Sym "Nate"
                                    , Datalog.Sym "Brian"
                                    ]
                                )
                                []
                            ]
                        )
        ]
