module Database.DatalogTests exposing (..)

import Array exposing (Array)
import Database
import Database.Datalog exposing (..)
import Expect
import Test exposing (..)


datalogTests : Test
datalogTests =
    describe "Datalog"
        [ describe "planRule"
            [ test "a simple read turns into a Read -> Project" <|
                \_ ->
                    rule "mortal" [ "who" ]
                        |> with "greek" [ var "who" ]
                        |> planRule
                        |> Expect.equal (Ok (Database.Project [ 0 ] (Database.Read "greek")))
            , test "a filtered read turns into a Select" <|
                \_ ->
                    rule "mortal" [ "first name" ]
                        |> with "greek" [ var "first name", string "of Athens" ]
                        |> planRule
                        |> Expect.equal
                            (Database.Read "greek"
                                |> Database.Select (Database.Predicate 1 Database.Eq (Database.Constant (Database.String "of Athens")))
                                |> Database.Project [ 0 ]
                                |> Ok
                            )
            , test "sharing a variable between two atoms results in a join" <|
                \_ ->
                    rule "reachable" [ "a", "c" ]
                        |> with "link" [ var "a", var "b" ]
                        |> with "reachable" [ var "b", var "c" ]
                        |> planRule
                        |> Expect.equal
                            (Database.JoinOn
                                { left = Database.Read "reachable"
                                , right = Database.Read "link"
                                , fields = [ ( 0, 1 ) ]
                                }
                                |> Database.Project [ 2, 1 ]
                                |> Ok
                            )
            , test "filtering adds a predicate" <|
                \_ ->
                    rule "adult" [ "name" ]
                        |> with "person" [ var "name", var "age" ]
                        |> filter (gt "age" (int 20))
                        |> planRule
                        |> Expect.equal
                            (Database.Read "person"
                                |> Database.Select (Database.Predicate 1 Database.Gt (Database.Constant (Database.Int 20)))
                                |> Database.Project [ 0 ]
                                |> Ok
                            )
            , test "filtering using negation adds a predicate" <|
                \_ ->
                    rule "sibling" [ "a", "b" ]
                        |> with "parent" [ var "parent", var "a" ]
                        |> with "parent" [ var "parent", var "b" ]
                        |> filter (not_ (eq "a" (var "b")))
                        |> planRule
                        |> Expect.equal
                            (Database.JoinOn
                                { left = Database.Read "parent"
                                , right = Database.Read "parent"
                                , fields = [ ( 0, 0 ) ]
                                }
                                |> Database.Select (Database.Not (Database.Predicate 3 Database.Eq (Database.Field 1)))
                                |> Database.Project [ 3, 1 ]
                                |> Ok
                            )
            , test "filtering using or adds a predicate" <|
                \_ ->
                    rule "teen" [ "name" ]
                        |> with "person" [ var "name", var "age" ]
                        |> filter
                            (or
                                (gt "age" (int 12))
                                (lt "age" (int 20))
                            )
                        |> planRule
                        |> Expect.equal
                            (Database.Read "person"
                                |> Database.Select
                                    (Database.Or
                                        (Database.Predicate 1 Database.Gt (Database.Constant (Database.Int 12)))
                                        (Database.Predicate 1 Database.Lt (Database.Constant (Database.Int 20)))
                                    )
                                |> Database.Project [ 0 ]
                                |> Ok
                            )
            , test "filtering using separate filters adds two filters" <|
                \_ ->
                    rule "oldHockeyTeam" [ "name" ]
                        |> with "team" [ var "name", var "league", var "age" ]
                        |> filter (eq "league" (string "NHL"))
                        |> filter (gt "age" (int 50))
                        |> planRule
                        |> Expect.equal
                            (Database.Read "team"
                                |> Database.Select (Database.Predicate 2 Database.Gt (Database.Constant (Database.Int 50)))
                                |> Database.Select (Database.Predicate 1 Database.Eq (Database.Constant (Database.String "NHL")))
                                |> Database.Project [ 0 ]
                                |> Ok
                            )
            , test "negation adds an outer join" <|
                \_ ->
                    rule "unreachable" [ "a", "b" ]
                        |> with "node" [ var "a" ]
                        |> with "node" [ var "b" ]
                        |> without "reachable" [ var "a", var "b" ]
                        |> planRule
                        |> Expect.equal
                            (Database.OuterJoinOn
                                { keep =
                                    Database.JoinOn
                                        { fields = []
                                        , left = Database.Read "node"
                                        , right = Database.Read "node"
                                        }
                                , drop = Database.Read "reachable"
                                , fields = [ ( 1, 0 ), ( 0, 1 ) ]
                                }
                                |> Database.Project [ 1, 0 ]
                                |> Ok
                            )
            , describe "safety"
                [ test "rules are required to have bodies" <|
                    \_ ->
                        rule "noBody" [ "a" ]
                            |> planRule
                            |> Expect.equal (Err NeedAtLeastOnePositiveAtom)
                , test "all terms in the head must appear in the body" <|
                    \_ ->
                        rule "bad" [ "a", "b" ]
                            |> with "fine" [ var "a" ]
                            |> planRule
                            |> Expect.equal (Err (VariableDoesNotAppearInBody "b"))
                , test "you can't have just filters" <|
                    \_ ->
                        rule "bad" [ "a" ]
                            |> filter (eq "a" (string "no"))
                            |> planRule
                            |> Expect.equal (Err NeedAtLeastOnePositiveAtom)
                , test "you can't filter on an unbound name" <|
                    \_ ->
                        rule "bad" [ "a" ]
                            |> with "fine" [ var "a" ]
                            |> filter (eq "b" (string "no"))
                            |> planRule
                            |> Expect.equal (Err (VariableDoesNotAppearInBody "b"))
                , test "every name appearing in a negative atom must also appear in a positive atom" <|
                    \_ ->
                        rule "bad" [ "a" ]
                            |> with "an atom to avoid the must-have-one-positive-atom rule" [ var "b" ]
                            |> without "here's the problem" [ var "a" ]
                            |> planRule
                            |> Expect.equal (Err (VariableMustAppearInPositiveAtom "a"))
                ]
            ]
        , describe "running a program"
            [ test "I can insert data" <|
                \_ ->
                    empty
                        |> insert "greek" [ string "Socrates" ]
                        |> Expect.ok
            , test "I can't insert variables" <|
                \_ ->
                    empty
                        |> insert "greek" [ var "who?" ]
                        |> Expect.equal (Err (CannotInsertVariable "who?"))
            , test "I can read the data I wrote back" <|
                \_ ->
                    empty
                        |> insert "greek" [ string "Socrates" ]
                        |> Result.andThen
                            (query
                                [ rule "query" [ "who" ]
                                    |> with "greek" [ var "who" ]
                                ]
                            )
                        |> Result.andThen (readError "query")
                        |> Expect.equal
                            (Ok [ Array.fromList [ Database.String "Socrates" ] ])
            , test "I can make a recursive query" <|
                \_ ->
                    empty
                        |> insert "link" [ int 1, int 2 ]
                        |> Result.andThen (insert "link" [ int 2, int 3 ])
                        |> Result.andThen (insert "link" [ int 3, int 3 ])
                        |> Result.andThen (insert "link" [ int 3, int 4 ])
                        |> Result.andThen
                            (query
                                [ rule "reachable" [ "a", "b" ]
                                    |> with "link" [ var "a", var "b" ]
                                , rule "reachable" [ "a", "c" ]
                                    |> with "link" [ var "a", var "b" ]
                                    |> with "reachable" [ var "b", var "c" ]
                                ]
                            )
                        |> Result.andThen (readError "reachable")
                        |> Expect.equal
                            (let
                                link : Int -> Int -> Array Database.Constant
                                link a b =
                                    Array.fromList [ Database.Int a, Database.Int b ]
                             in
                             Ok
                                [ link 1 2
                                , link 1 3
                                , link 1 4
                                , link 2 3
                                , link 2 4
                                , link 3 3
                                , link 3 4
                                ]
                            )
            , test "I can make a joining query" <|
                \_ ->
                    empty
                        |> insert "mascot" [ string "Fredbird", string "Cardinals" ]
                        |> Result.andThen (insert "mascot" [ string "Louie", string "Blues" ])
                        |> Result.andThen (insert "mascot" [ string "Gritty", string "Flyers" ])
                        |> Result.andThen (insert "team" [ string "Cardinals", string "St. Louis" ])
                        |> Result.andThen (insert "team" [ string "Blues", string "St. Louis" ])
                        |> Result.andThen (insert "team" [ string "Flyers", string "Philadelphia" ])
                        |> Result.andThen
                            (query
                                [ rule "hometown" [ "name", "hometown" ]
                                    |> with "mascot" [ var "name", var "team" ]
                                    |> with "team" [ var "team", var "hometown" ]
                                ]
                            )
                        |> Result.andThen (readError "hometown")
                        |> Expect.equal
                            (Ok
                                [ Array.fromList [ Database.String "Fredbird", Database.String "St. Louis" ]
                                , Array.fromList [ Database.String "Gritty", Database.String "Philadelphia" ]
                                , Array.fromList [ Database.String "Louie", Database.String "St. Louis" ]
                                ]
                            )
            , test "I can make a query with a negation" <|
                \_ ->
                    empty
                        |> insert "link" [ int 1, int 2 ]
                        |> Result.andThen (insert "link" [ int 2, int 3 ])
                        |> Result.andThen (insert "link" [ int 3, int 3 ])
                        |> Result.andThen (insert "link" [ int 3, int 4 ])
                        |> Result.andThen
                            (query
                                [ rule "reachable" [ "a", "b" ]
                                    |> with "link" [ var "a", var "b" ]
                                , rule "reachable" [ "a", "c" ]
                                    |> with "link" [ var "a", var "b" ]
                                    |> with "reachable" [ var "b", var "c" ]
                                , rule "node" [ "a" ]
                                    |> with "link" [ var "a", var "b" ]
                                , rule "node" [ "b" ]
                                    |> with "link" [ var "a", var "b" ]
                                , rule "unreachable" [ "a", "b" ]
                                    |> with "node" [ var "a" ]
                                    |> with "node" [ var "b" ]
                                    |> without "reachable" [ var "a", var "b" ]
                                ]
                            )
                        |> Result.andThen (readError "unreachable")
                        |> Expect.equal
                            (let
                                link : Int -> Int -> Array Database.Constant
                                link a b =
                                    Array.fromList [ Database.Int a, Database.Int b ]
                             in
                             Ok
                                [ link 1 1
                                , link 2 1
                                , link 2 2
                                , link 3 1
                                , link 3 2
                                , link 4 1
                                , link 4 2
                                , link 4 3
                                , link 4 4
                                ]
                            )
            , test "I cannot make a query with negative recursion" <|
                \_ ->
                    empty
                        |> insert "x" [ string "this doesn't matter, we just need it to trigger the rule under test" ]
                        |> Result.andThen
                            (query
                                [ rule "p" [ "x" ]
                                    |> with "x" [ var "x" ]
                                    |> without "q" [ var "x" ]
                                , rule "q" [ "x" ]
                                    |> with "x" [ var "x" ]
                                    |> without "p" [ var "x" ]
                                ]
                            )
                        |> Expect.equal (Err CannotHaveNegationInRecursiveQuery)
            ]
        ]


readError : String -> Database.Database -> Result Problem (List (Array Database.Constant))
readError name db =
    Database.read name db
        |> Result.map Database.rows
        |> Result.mapError DatabaseProblem
