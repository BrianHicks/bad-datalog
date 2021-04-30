module RelationalTests exposing (..)

import Array exposing (Array)
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
                    |> Expect.equal
                        (Err
                            (SchemaMismatch
                                { wanted = Array.fromList [ StringField ]
                                , got = Array.fromList [ StringField, StringField ]
                                }
                            )
                        )
        , test "you can't insert a row with a different field type" <|
            \_ ->
                empty
                    |> insert "human" [ String "Socrates" ]
                    |> Result.andThen (insert "human" [ Int 0 ])
                    |> Expect.equal
                        (Err
                            (SchemaMismatch
                                { wanted = Array.fromList [ StringField ]
                                , got = Array.fromList [ IntField ]
                                }
                            )
                        )
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
        , describe "select"
            [ test "can select on string equality" <|
                \_ ->
                    toysDb
                        |> Result.andThen
                            (runPlan (Select (Predicate 1 Eq (Constant (String "Bear"))) (Read "toys")))
                        |> Result.map .rows
                        |> Expect.equal (Ok [ cloudBear ])
            , test "can select on integer equality" <|
                \_ ->
                    toysDb
                        |> Result.andThen
                            (runPlan (Select (Predicate 3 Eq (Constant (Int 16))) (Read "toys")))
                        |> Result.map .rows
                        |> Expect.equal (Ok [ axel ])
            , test "can select on string greater-than" <|
                \_ ->
                    toysDb
                        |> Result.andThen
                            (runPlan (Select (Predicate 0 Gt (Constant (String "Cloud Bear"))) (Read "toys")))
                        |> Result.map .rows
                        |> Expect.equal (Ok [ hampton, humphrey ])
            , test "can select on integer greater-than" <|
                \_ ->
                    toysDb
                        |> Result.andThen
                            (runPlan (Select (Predicate 3 Gt (Constant (Int 20))) (Read "toys")))
                        |> Result.map .rows
                        |> Expect.equal (Ok [ humphrey, cloudBear ])
            , test "can select on string less-than" <|
                \_ ->
                    toysDb
                        |> Result.andThen
                            (runPlan (Select (Predicate 0 Lt (Constant (String "Cloud Bear"))) (Read "toys")))
                        |> Result.map .rows
                        |> Expect.equal (Ok [ axel ])
            , test "can select on integer less-than" <|
                \_ ->
                    toysDb
                        |> Result.andThen
                            (runPlan (Select (Predicate 3 Lt (Constant (Int 20))) (Read "toys")))
                        |> Result.map .rows
                        |> Expect.equal (Ok [ hampton, axel ])
            , test "can not-ify a selection" <|
                \_ ->
                    toysDb
                        |> Result.andThen
                            (runPlan (Select (Not (Predicate 0 Eq (Constant (String "Humphrey")))) (Read "toys")))
                        |> Result.map .rows
                        |> Expect.equal (Ok [ hampton, cloudBear, axel ])
            , test "can and-ify two selections" <|
                \_ ->
                    toysDb
                        |> Result.andThen
                            (runPlan
                                (Select
                                    (And
                                        (Predicate 2 Eq (Constant (String "USA")))
                                        (Predicate 3 Gt (Constant (Int 30)))
                                    )
                                    (Read "toys")
                                )
                            )
                        |> Result.map .rows
                        |> Expect.equal (Ok [ humphrey ])
            , test "can or-ify two selections" <|
                \_ ->
                    toysDb
                        |> Result.andThen
                            (runPlan
                                (Select
                                    (Or
                                        (Predicate 3 Lt (Constant (Int 16)))
                                        (Predicate 3 Eq (Constant (Int 16)))
                                    )
                                    (Read "toys")
                                )
                            )
                        |> Result.map .rows
                        |> Expect.equal (Ok [ hampton, axel ])
            ]
        ]


toysDb : Result Problem Database
toysDb =
    Ok empty
        |> Result.andThen (insert "toys" (Array.toList axel))
        |> Result.andThen (insert "toys" (Array.toList cloudBear))
        |> Result.andThen (insert "toys" (Array.toList humphrey))
        |> Result.andThen (insert "toys" (Array.toList hampton))


axel : Array Constant
axel =
    Array.fromList [ String "Axel", String "Puffin", String "Iceland", Int 16 ]


cloudBear : Array Constant
cloudBear =
    Array.fromList [ String "Cloud Bear", String "Bear", String "USA", Int 24 ]


humphrey : Array Constant
humphrey =
    Array.fromList [ String "Humphrey", String "Elephant", String "USA", Int 32 ]


hampton : Array Constant
hampton =
    Array.fromList [ String "Hampton", String "Rabbit", String "USA", Int 8 ]
