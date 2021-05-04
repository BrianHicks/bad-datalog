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
            , test "a filtered read turns into a Select" <|
                \_ ->
                    rule (atom "mortal" [ var "first name" ]) [ atom "greek" [ var "first name", string "of Athens" ] ]
                        |> ruleToPlan
                        |> Expect.equal
                            (Database.Read "greek"
                                |> Database.Select (Database.Predicate 1 Database.Eq (Database.Constant (Database.String "of Athens")))
                                |> Database.Project [ 0 ]
                                |> Ok
                            )
            , test "sharing a variable between two atoms results in a join" <|
                \_ ->
                    rule
                        (atom "reachable" [ var "a", var "c" ])
                        [ atom "link" [ var "a", var "b" ]
                        , atom "reachable" [ var "b", var "c" ]
                        ]
                        |> ruleToPlan
                        |> Expect.equal
                            (Database.Join
                                { left = Database.Read "reachable"
                                , right = Database.Read "link"
                                , fields = [ ( 0, 1 ) ]
                                }
                                |> Database.Project [ 2, 1 ]
                                |> Ok
                            )
            ]
        ]
