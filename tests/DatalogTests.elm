module DatalogTests exposing (..)

import Datalog exposing (..)
import Datalog.Database as Database
import Expect exposing (Expectation)
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
                , test "rules are required to have at least one name" <|
                    \_ ->
                        rule "noNames" []
                            |> with "what" [ var "a" ]
                            |> planRule
                            |> Expect.equal (Err NeedAtLeastOneName)
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
        , describe "working with the database"
            [ describe "insertion"
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
                ]
            , describe "reading"
                [ test "I can read back a string I wrote" <|
                    \_ ->
                        empty
                            |> insert "greek" [ string "Socrates" ]
                            |> Result.andThen (into identity |> stringField 0 |> read "greek")
                            |> Expect.equal (Ok [ "Socrates" ])
                , test "I can read back a number I wrote" <|
                    \_ ->
                        empty
                            |> insert "numbers" [ int 1 ]
                            |> Result.andThen (into identity |> intField 0 |> read "numbers")
                            |> Expect.equal (Ok [ 1 ])
                , test "I get an error if I try to read the wrong type" <|
                    \_ ->
                        empty
                            |> insert "numbers" [ int 1 ]
                            |> Result.andThen (into identity |> stringField 0 |> read "numbers")
                            |> Expect.equal (Err (DecodingProblem (UnexpectedFieldType Database.IntType)))
                , test "I get an error if I try to read an index that doesn't exist" <|
                    \_ ->
                        empty
                            |> insert "numbers" [ int 1 ]
                            |> Result.andThen (into identity |> stringField 1 |> read "numbers")
                            |> Expect.equal (Err (DecodingProblem (FieldNotFound 1)))
                , test "I get an error if I try and read a table that doesn't exist" <|
                    \_ ->
                        empty
                            |> (into identity |> stringField 1 |> read "person")
                            |> Expect.equal (Err (DatabaseProblem (Database.RelationNotFound "person")))
                , test "I get an empty list if I try and read a table which I pre-registered" <|
                    \_ ->
                        empty
                            |> register "person" [ Database.StringType ]
                            |> Result.andThen (into identity |> stringField 1 |> read "person")
                            |> Expect.equal (Ok [])
                , test "I can read exactly one row" <|
                    \_ ->
                        empty
                            |> insert "person" [ string "Fred Rogers" ]
                            |> Result.andThen (into identity |> stringField 0 |> readOne "person")
                            |> Expect.equal (Ok "Fred Rogers")
                , test "if I expect one row and there are 0, it's an error" <|
                    \_ ->
                        empty
                            |> register "person" [ Database.StringType ]
                            |> Result.andThen (into identity |> stringField 0 |> readOne "person")
                            |> Expect.equal (Err (ExpectedExactlyOneRow 0))
                , test "if I expect one row and there si more than one, it's an error" <|
                    \_ ->
                        empty
                            |> insert "person" [ string "Fred Rogers" ]
                            |> Result.andThen (insert "person" [ string "Frida Kahlo" ])
                            |> Result.andThen (into identity |> stringField 0 |> readOne "person")
                            |> Expect.equal (Err (ExpectedExactlyOneRow 2))
                ]
            , describe "deriving new tables"
                [ test "I can derive a new table by renaming an existing table" <|
                    \_ ->
                        empty
                            |> insert "greek" [ string "Socrates" ]
                            |> Result.andThen
                                (derive
                                    [ rule "mortal" [ "name" ]
                                        |> with "greek" [ var "name" ]
                                    ]
                                )
                            |> Result.andThen
                                (into identity
                                    |> stringField 0
                                    |> read "mortal"
                                )
                            |> Expect.equal (Ok [ "Socrates" ])
                , test "I can derive a new table by combining tables" <|
                    \_ ->
                        empty
                            |> insert "greek" [ string "Socrates" ]
                            |> Result.andThen (insert "french" [ string "Simone de Beauvoir" ])
                            |> Result.andThen
                                (derive
                                    [ rule "mortal" [ "name" ]
                                        |> with "greek" [ var "name" ]
                                    , rule "mortal" [ "name" ]
                                        |> with "french" [ var "name" ]
                                    ]
                                )
                            |> Result.andThen
                                (into identity
                                    |> stringField 0
                                    |> read "mortal"
                                )
                            |> Expect.equal (Ok [ "Simone de Beauvoir", "Socrates" ])
                , test "I can filter tables by matching inside a `with` clause" <|
                    \_ ->
                        empty
                            |> insert "person" [ string "Simone de Beauvoir", string "philosopher" ]
                            |> Result.andThen (insert "person" [ string "Barbara Liskov", string "computer scientist" ])
                            |> Result.andThen
                                (derive
                                    [ rule "philosopher" [ "name" ]
                                        |> with "person" [ var "name", string "philosopher" ]
                                    ]
                                )
                            |> Result.andThen
                                (into identity
                                    |> stringField 0
                                    |> read "philosopher"
                                )
                            |> Expect.equal (Ok [ "Simone de Beauvoir" ])
                , test "I can derive with recursion" <|
                    \_ ->
                        empty
                            |> insert "link" [ int 1, int 2 ]
                            |> Result.andThen (insert "link" [ int 2, int 3 ])
                            |> Result.andThen (insert "link" [ int 3, int 3 ])
                            |> Result.andThen (insert "link" [ int 3, int 4 ])
                            |> Result.andThen
                                (derive
                                    [ rule "reachable" [ "a", "b" ]
                                        |> with "link" [ var "a", var "b" ]
                                    , rule "reachable" [ "a", "c" ]
                                        |> with "link" [ var "a", var "b" ]
                                        |> with "reachable" [ var "b", var "c" ]
                                    ]
                                )
                            |> Result.andThen
                                (into Tuple.pair
                                    |> intField 0
                                    |> intField 1
                                    |> read "reachable"
                                )
                            |> Expect.equal
                                (Ok
                                    [ ( 1, 2 )
                                    , ( 1, 3 )
                                    , ( 1, 4 )
                                    , ( 2, 3 )
                                    , ( 2, 4 )
                                    , ( 3, 3 )
                                    , ( 3, 4 )
                                    ]
                                )
                , test "I can derive a new table via join" <|
                    \_ ->
                        empty
                            |> insert "mascot" [ string "Fredbird", string "Cardinals" ]
                            |> Result.andThen (insert "mascot" [ string "Louie", string "Blues" ])
                            |> Result.andThen (insert "mascot" [ string "Gritty", string "Flyers" ])
                            |> Result.andThen (insert "team" [ string "Cardinals", string "St. Louis" ])
                            |> Result.andThen (insert "team" [ string "Blues", string "St. Louis" ])
                            |> Result.andThen (insert "team" [ string "Flyers", string "Philadelphia" ])
                            |> Result.andThen
                                (derive
                                    [ rule "hometown" [ "name", "hometown" ]
                                        |> with "mascot" [ var "name", var "team" ]
                                        |> with "team" [ var "team", var "hometown" ]
                                    ]
                                )
                            |> Result.andThen
                                (into Tuple.pair
                                    |> stringField 0
                                    |> stringField 1
                                    |> read "hometown"
                                )
                            |> Expect.equal
                                (Ok
                                    [ ( "Fredbird", "St. Louis" )
                                    , ( "Gritty", "Philadelphia" )
                                    , ( "Louie", "St. Louis" )
                                    ]
                                )
                , test "I can derive a table using negation" <|
                    \_ ->
                        empty
                            |> insert "link" [ int 1, int 2 ]
                            |> Result.andThen (insert "link" [ int 2, int 3 ])
                            |> Result.andThen (insert "link" [ int 3, int 3 ])
                            |> Result.andThen (insert "link" [ int 3, int 4 ])
                            |> Result.andThen
                                (derive
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
                            |> Result.andThen
                                (into Tuple.pair
                                    |> intField 0
                                    |> intField 1
                                    |> read "unreachable"
                                )
                            |> Expect.equal
                                (Ok
                                    [ ( 1, 1 )
                                    , ( 2, 1 )
                                    , ( 2, 2 )
                                    , ( 3, 1 )
                                    , ( 3, 2 )
                                    , ( 4, 1 )
                                    , ( 4, 2 )
                                    , ( 4, 3 )
                                    , ( 4, 4 )
                                    ]
                                )
                , test "I cannot derive a program with negative recursion" <|
                    \_ ->
                        empty
                            |> insert "x" [ string "this doesn't matter, we just need it to trigger the rule under test" ]
                            |> Result.andThen
                                (derive
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
        , describe "parsing rules from a string"
            [ describe "valid programs"
                [ test "a non-recursive rule" <|
                    \_ ->
                        expectParses
                            "mortal(thing) :- greek(thing)."
                            [ rule "mortal" [ "thing" ]
                                |> with "greek" [ var "thing" ]
                            ]
                , test "recursive rule" <|
                    \_ ->
                        expectParses
                            """
                            reachable(a, b) :- link(a, b).
                            reachable(a, c) :- link(a, b), reachable(b, c).
                            """
                            [ rule "reachable" [ "a", "b" ]
                                |> with "link" [ var "a", var "b" ]
                            , rule "reachable" [ "a", "c" ]
                                |> with "link" [ var "a", var "b" ]
                                |> with "reachable" [ var "b", var "c" ]
                            ]
                , test "an atom with an string" <|
                    \_ ->
                        expectParses
                            """baseballTeam(name) :- team(name, "MLB")."""
                            [ rule "baseballTeam" [ "name" ]
                                |> with "team" [ var "name", string "MLB" ]
                            ]
                , test "an atom with an integer" <|
                    \_ ->
                        expectParses
                            """
                            luckyBuilding(name) :- building(name, 7).
                            """
                            [ rule "luckyBuilding" [ "name" ]
                                |> with "building" [ var "name", int 7 ]
                            ]
                , describe "filters"
                    [ test "less than" <|
                        \_ ->
                            expectParses
                                """
                                child(name) :-
                                  person(name, age),
                                  age < 18.
                                """
                                [ rule "child" [ "name" ]
                                    |> with "person" [ var "name", var "age" ]
                                    |> filter (lt "age" (int 18))
                                ]
                    , test "greater than" <|
                        \_ ->
                            expectParses
                                """
                                adult(name) :-
                                  person(name, age),
                                  age > 17.
                                """
                                [ rule "adult" [ "name" ]
                                    |> with "person" [ var "name", var "age" ]
                                    |> filter (gt "age" (int 17))
                                ]
                    , test "equal to" <|
                        \_ ->
                            expectParses
                                """
                                spider(name) :-
                                  thing(name, legs),
                                  legs = 8.
                                """
                                [ rule "spider" [ "name" ]
                                    |> with "thing" [ var "name", var "legs" ]
                                    |> filter (eq "legs" (int 8))
                                ]
                    , test "or" <|
                        \_ ->
                            expectParses
                                """
                                adult(name) :-
                                  person(name, age),
                                  age = 18 || age > 18.
                                """
                                [ rule "adult" [ "name" ]
                                    |> with "person" [ var "name", var "age" ]
                                    |> filter
                                        (or
                                            (eq "age" (int 18))
                                            (gt "age" (int 18))
                                        )
                                ]
                    ]
                , todo "rule with negation"
                ]
            , describe "errors"
                [ todo "errors!" ]
            ]
        ]


expectParses : String -> List Rule -> Expectation
expectParses input expectedOutput =
    case Datalog.parse input of
        Ok actualOutput ->
            Expect.equal expectedOutput actualOutput

        Err err ->
            case err of
                ParsingProblem deadEnds ->
                    -- this should eventually just show the nicest version possible
                    -- of the error message. Probably using whatever `errorToString`
                    -- thing we end up with.
                    let
                        rows : List String
                        rows =
                            String.lines input

                        sourceRow : Int -> Maybe String
                        sourceRow i =
                            rows
                                |> List.drop (i - 1)
                                |> List.head

                        pointer : Int -> String
                        pointer i =
                            String.repeat (i - 1) " " ++ "^"
                    in
                    deadEnds
                        |> List.map
                            (\{ row, col, contextStack, problem } ->
                                String.join "\n"
                                    [ "There's a problem at row " ++ String.fromInt row ++ ", column " ++ String.fromInt col ++ ":"
                                    , ""
                                    , Maybe.withDefault "???" (sourceRow row)
                                    , pointer col
                                    , ""
                                    , "With this problem:"
                                    , Debug.toString problem
                                    , ""
                                    , "With this context:"
                                    , contextStack
                                        |> List.map (\context -> " - " ++ Debug.toString context)
                                        |> String.join "\n"
                                    ]
                            )
                        |> String.join "\n\n==========================================================\n\n"
                        |> (++) "I ran into a problem parsing. I got these dead ends:\n\n"
                        |> Expect.fail

                _ ->
                    -- fall back on a reasonable implementation
                    Expect.equal (Ok expectedOutput) (Err err)
