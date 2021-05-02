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
                                { wanted = Array.fromList [ StringType ]
                                , got = Array.fromList [ StringType, StringType ]
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
                                { wanted = Array.fromList [ StringType ]
                                , got = Array.fromList [ IntType ]
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
            [ test "doesn't let you select a field that doesn't exist on the left-hand side" <|
                \_ ->
                    toysDb
                        |> Result.andThen
                            (runPlan (Select (Predicate 5 Eq (Constant (Int 0))) (Read "toys")))
                        |> Expect.equal (Err (UnknownFields [ 5 ]))
            , test "doesn't let you select a field that doesn't exist on the right-hand side" <|
                \_ ->
                    toysDb
                        |> Result.andThen
                            (runPlan (Select (Predicate 0 Eq (Field 5)) (Read "toys")))
                        |> Expect.equal (Err (UnknownFields [ 5 ]))
            , test "doesn't let you make comparisons against unlike types" <|
                \_ ->
                    toysDb
                        |> Result.andThen
                            (runPlan (Select (Predicate 0 Eq (Constant (Int 0))) (Read "toys")))
                        |> Expect.equal (Err (IncompatibleComparison StringType IntType))
            , test "can select on string equality" <|
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
        , describe "project"
            [ test "does not allow selecting non-existent fields" <|
                \_ ->
                    toysDb
                        |> Result.andThen
                            (runPlan (Project [ 5 ] (Read "toys")))
                        |> Expect.equal (Err (UnknownFields [ 5 ]))
            , test "can project a set of fields" <|
                \_ ->
                    toysDb
                        |> Result.andThen
                            (runPlan (Project [ 0, 2 ] (Read "toys")))
                        |> Expect.equal
                            (Ok
                                { schema = Array.fromList [ StringType, StringType ]
                                , rows =
                                    [ Array.fromList [ String "Hampton", String "USA" ]
                                    , Array.fromList [ String "Humphrey", String "USA" ]
                                    , Array.fromList [ String "Cloud Bear", String "USA" ]
                                    , Array.fromList [ String "Axel", String "Iceland" ]
                                    ]
                                }
                            )
            ]
        , describe "cross-product"
            [ test "combines all the rows on the left and right" <|
                \_ ->
                    mascotsDb
                        |> Result.andThen
                            (runPlan (CrossProduct (Read "mascots") (Read "teams")))
                        |> Expect.equal
                            (Ok
                                { schema = Array.fromList [ StringType, StringType, StringType, StringType, StringType ]
                                , rows =
                                    [ Array.append gritty flyers
                                    , Array.append gritty blues
                                    , Array.append gritty cardinals
                                    , Array.append louie flyers
                                    , Array.append louie blues
                                    , Array.append louie cardinals
                                    , Array.append fredbird flyers
                                    , Array.append fredbird blues
                                    , Array.append fredbird cardinals
                                    ]
                                }
                            )
            ]
        , describe "join"
            [ test "joins on attributes in order" <|
                \_ ->
                    mascotsDb
                        |> Result.andThen
                            (runPlan
                                (Join
                                    { left = Read "mascots"
                                    , leftFields = [ 1 ]
                                    , right = Read "teams"
                                    , rightFields = [ 0 ]
                                    }
                                )
                            )
                        |> Expect.equal
                            (Ok
                                { schema = Array.fromList [ StringType, StringType, StringType, StringType, StringType ]
                                , rows =
                                    [ Array.append gritty flyers
                                    , Array.append louie blues
                                    , Array.append fredbird cardinals
                                    ]
                                }
                            )
            ]
        ]



-- Example: toys
-- Used for simple selection


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



-- Example: Mascots
-- used for joining


mascotsDb : Result Problem Database
mascotsDb =
    Ok empty
        |> Result.andThen (insert "mascots" (Array.toList fredbird))
        |> Result.andThen (insert "mascots" (Array.toList louie))
        |> Result.andThen (insert "mascots" (Array.toList gritty))
        |> Result.andThen (insert "teams" (Array.toList cardinals))
        |> Result.andThen (insert "teams" (Array.toList blues))
        |> Result.andThen (insert "teams" (Array.toList flyers))


fredbird : Array Constant
fredbird =
    Array.fromList [ String "Fredbird", String "Cardinals" ]


louie : Array Constant
louie =
    Array.fromList [ String "Louie", String "Blues" ]


gritty : Array Constant
gritty =
    Array.fromList [ String "Gritty", String "Flyers" ]


cardinals : Array Constant
cardinals =
    Array.fromList [ String "Cardinals", String "MLB", String "St. Louis, MO" ]


blues : Array Constant
blues =
    Array.fromList [ String "Blues", String "NHL", String "St. Louis, MO" ]


flyers : Array Constant
flyers =
    Array.fromList [ String "Flyers", String "NHL", String "Philadelphia, PA" ]
