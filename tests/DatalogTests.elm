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
                                { left = Database.Read "reachable"
                                , right = Database.Read "link"
                                , fields = [ ( 0, 1 ) ]
                                }
                                |> Database.Project [ 2, 1 ]
                                |> Ok
                            )
            , describe "safety"
                [ test "rules are required to have bodies" <|
                    \_ ->
                        rule
                            (headAtom "noBody" [ "a" ])
                            []
                            |> ruleToPlan
                            |> Expect.equal
                                (Err CannotPlanFact)
                , test "all terms in the head must appear in the body" <|
                    \_ ->
                        rule
                            (headAtom "bad" [ "a", "b" ])
                            [ atom "fine" [ var "a" ] ]
                            |> ruleToPlan
                            |> Expect.equal
                                (Err (VariableDoesNotAppearInBody "b"))
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
                                    [ atom "reachable" [ var "a", var "b" ]
                                    , atom "reachable" [ var "b", var "c" ]
                                    ]
                                ]
                            )
                        |> Result.andThen (readError "reachable")
                        |> Result.map List.reverse
                        |> Expect.equal
                            (let
                                link : Int -> Int -> Array Database.Constant
                                link a b =
                                    Array.fromList [ Database.Int a, Database.Int b ]
                             in
                             Ok
                                [ link 1 2
                                , link 2 3
                                , link 3 3
                                , link 3 4
                                , link 1 3
                                , link 2 4
                                , link 1 4
                                ]
                            )
            ]
        ]


readError : String -> Database.Database -> Result Problem (List (Array Database.Constant))
readError name db =
    Database.read name db
        |> Result.map Database.rows
        |> Result.mapError DatabaseProblem
