module RelationalTests exposing (..)

import Array
import Expect
import Relational exposing (..)
import Test exposing (..)


insertTests : Test
insertTests =
    describe "insert"
        [ test "you can insert a row" <|
            \_ ->
                empty
                    |> insert "human" (Array.fromList [ String "Socrates" ])
                    |> Expect.ok
        ]
