module DatalogTests exposing (..)

import Array exposing (Array)
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
                    rule (headAtom "mortal" [ "who" ]) [ atom "greek" [ var "who" ] ]
                        |> ruleToPlan
                        |> Expect.equal (Ok (Database.Project [ 0 ] (Database.Read "greek")))
            , test "a filtered read turns into a Select" <|
                \_ ->
                    rule (headAtom "mortal" [ "first name" ]) [ atom "greek" [ var "first name", string "of Athens" ] ]
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
                        (headAtom "reachable" [ "a", "c" ])
                        [ atom "link" [ var "a", var "b" ]
                        , atom "reachable" [ var "b", var "c" ]
                        ]
                        |> ruleToPlan
                        |> Expect.equal
                            (Database.Join
                                { left = Database.Read "link"
                                , right = Database.Read "reachable"
                                , fields = [ ( 1, 0 ) ]
                                }
                                |> Database.Project [ 0, 3 ]
                                |> Ok
                            )
            , test "filtering adds a predicate" <|
                \_ ->
                    rule
                        (headAtom "adult" [ "name" ])
                        [ atom "person" [ var "name", var "age" ]
                        , filter (gt "age" (int 20))
                        ]
                        |> ruleToPlan
                        |> Expect.equal
                            (Database.Read "person"
                                |> Database.Select (Database.Predicate 1 Database.Gt (Database.Constant (Database.Int 20)))
                                |> Database.Project [ 0 ]
                                |> Ok
                            )
            , test "filtering using negation adds a predicate" <|
                \_ ->
                    rule
                        (headAtom "sibling" [ "a", "b" ])
                        [ atom "parent" [ var "parent", var "a" ]
                        , atom "parent" [ var "parent", var "b" ]
                        , filter (not_ (eq "a" (var "b")))
                        ]
                        |> ruleToPlan
                        |> Expect.equal
                            (Database.Join
                                { left = Database.Read "parent"
                                , right = Database.Read "parent"
                                , fields = [ ( 0, 0 ) ]
                                }
                                |> Database.Select (Database.Not (Database.Predicate 1 Database.Eq (Database.Field 3)))
                                |> Database.Project [ 1, 3 ]
                                |> Ok
                            )
            , test "filtering using or adds a predicate" <|
                \_ ->
                    rule
                        (headAtom "teen" [ "name" ])
                        [ atom "person" [ var "name", var "age" ]
                        , filter
                            (or
                                (gt "age" (int 12))
                                (lt "age" (int 20))
                            )
                        ]
                        |> ruleToPlan
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
                    rule
                        (headAtom "oldHockeyTeam" [ "name" ])
                        [ atom "team" [ var "name", var "league", var "age" ]
                        , filter (eq "league" (string "NHL"))
                        , filter (gt "age" (int 50))
                        ]
                        |> ruleToPlan
                        |> Expect.equal
                            (Database.Read "team"
                                |> Database.Select (Database.Predicate 1 Database.Eq (Database.Constant (Database.String "NHL")))
                                |> Database.Select (Database.Predicate 2 Database.Gt (Database.Constant (Database.Int 50)))
                                |> Database.Project [ 0 ]
                                |> Ok
                            )
            , test "negation adds an outer join" <|
                \_ ->
                    rule
                        (headAtom "unreachable" [ "a", "b" ])
                        [ atom "node" [ var "a" ]
                        , atom "node" [ var "b" ]
                        , notAtom "reachable" [ var "a", var "b" ]
                        ]
                        |> ruleToPlan
                        |> Expect.equal
                            (Database.OuterJoin
                                { keep =
                                    Database.Join
                                        { fields = []
                                        , left = Database.Read "node"
                                        , right = Database.Read "node"
                                        }
                                , drop = Database.Read "reachable"
                                }
                                |> Database.Project [ 0, 1 ]
                                |> Ok
                            )
            , describe "safety"
                [ test "rules are required to have bodies" <|
                    \_ ->
                        rule
                            (headAtom "noBody" [ "a" ])
                            []
                            |> ruleToPlan
                            |> Expect.equal (Err NeedAtLeastOneAtom)
                , test "all terms in the head must appear in the body" <|
                    \_ ->
                        rule
                            (headAtom "bad" [ "a", "b" ])
                            [ atom "fine" [ var "a" ] ]
                            |> ruleToPlan
                            |> Expect.equal (Err (VariableDoesNotAppearInBody "b"))
                , test "you can't have just filters" <|
                    \_ ->
                        rule
                            (headAtom "bad" [ "a" ])
                            [ filter (eq "a" (string "no")) ]
                            |> ruleToPlan
                            |> Expect.equal (Err NeedAtLeastOneAtom)
                , test "you can't filter on an unbound name" <|
                    \_ ->
                        rule
                            (headAtom "bad" [ "a" ])
                            [ atom "fine" [ var "a" ]
                            , filter (eq "b" (string "no"))
                            ]
                            |> ruleToPlan
                            |> Expect.equal (Err (VariableDoesNotAppearInBody "b"))
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
                                [ rule (headAtom "query" [ "who" ])
                                    [ atom "greek" [ var "who" ] ]
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
                                [ rule
                                    (headAtom "reachable" [ "a", "b" ])
                                    [ atom "link" [ var "a", var "b" ] ]
                                , rule
                                    (headAtom "reachable" [ "a", "c" ])
                                    [ atom "link" [ var "a", var "b" ]
                                    , atom "reachable" [ var "b", var "c" ]
                                    ]
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
                                [ rule (headAtom "hometown" [ "name", "hometown" ])
                                    [ atom "mascot" [ var "name", var "team" ]
                                    , atom "team" [ var "team", var "hometown" ]
                                    ]
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
                                [ rule
                                    (headAtom "reachable" [ "a", "b" ])
                                    [ atom "link" [ var "a", var "b" ] ]
                                , rule
                                    (headAtom "reachable" [ "a", "c" ])
                                    [ atom "link" [ var "a", var "b" ]
                                    , atom "reachable" [ var "b", var "c" ]
                                    ]
                                , rule (headAtom "node" [ "a" ]) [ atom "link" [ var "a", var "b" ] ]
                                , rule (headAtom "node" [ "b" ]) [ atom "link" [ var "a", var "b" ] ]
                                , rule
                                    (headAtom "unreachable" [ "a", "b" ])
                                    [ atom "node" [ var "a" ]
                                    , atom "node" [ var "b" ]
                                    , notAtom "reachable" [ var "a", var "b" ]
                                    ]
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
            ]
        ]


readError : String -> Database.Database -> Result Problem (List (Array Database.Constant))
readError name db =
    Database.read name db
        |> Result.map Database.rows
        |> Result.mapError DatabaseProblem
