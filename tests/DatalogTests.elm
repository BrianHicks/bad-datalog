module DatalogTests exposing (..)

import Array
import Database
import Datalog exposing (..)
import Expect
import Test exposing (..)


datalogTests : Test
datalogTests =
    describe "Datalog"
        [ describe "ruleToPlan"
            [ test "a simple read turns into a Read -> Project" <|
                \_ ->
                    rule (atom "mortal" [ var "who" ]) [ atom "greek" [ var "who" ] ]
                        |> ruleToPlan
                        |> Expect.equal (Ok (Database.Project [ 0 ] (Database.Read "greek")))
            ]
        ]
