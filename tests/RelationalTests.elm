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
                    |> insert "human" [ String "Socrates" ]
                    |> Expect.ok
        , test "you can't insert a row with a different number of columns" <|
            \_ ->
                empty
                    |> insert "human" [ String "Socrates" ]
                    |> Result.andThen (insert "human" [ String "Socrates", String "Greek" ])
                    |> Expect.err
        , test "you can't insert a row with a different field type" <|
            \_ ->
                empty
                    |> insert "human" [ String "Socrates" ]
                    |> Result.andThen (insert "human" [ Int 0 ])
                    |> Expect.err
        ]


runPlanTests : Test
runPlanTests =
    describe "runPlan"
        [ describe "read"
            [ test "can read a relation" <|
                \_ ->
                    empty
                        |> insert "human" [ String "Socrates" ]
                        |> Result.andThen (runPlan (Read "human"))
                        |> Result.map .rows
                        |> Expect.equal (Ok [ Array.fromList [ String "Socrates" ] ])
            , test "cannot read a relation that doesn't exist" <|
                \_ ->
                    empty
                        |> runPlan (Read "human")
                        |> Expect.equal (Err (RelationNotFound "human"))
            ]
        ]
