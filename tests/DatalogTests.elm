module DatalogTests exposing (..)

import Datalog exposing (..)
import Datalog.Atom as Atom exposing (Atom, atom)
import Datalog.Negatable as Negatable exposing (Negatable, negative, positive)
import Datalog.Rule as Rule exposing (Rule)
import Datalog.Term as Term exposing (anonymous, string, variable)
import Dict exposing (Dict)
import Expect
import Test exposing (..)


solveTest : Test
solveTest =
    describe "solve"
        [ test "ground rules are solved" <|
            \_ ->
                unsafeProgram
                    [ Rule.fact (atom "greek" [ string "Socrates" ]) ]
                    |> solve
                    |> get ( "greek", 1 )
                    |> Expect.equal [ atom "greek" [ string "Socrates" ] ]
        , test "non-ground rules are solved" <|
            \_ ->
                unsafeProgram
                    [ Rule.fact (atom "greek" [ string "Socrates" ])
                    , Rule.rule
                        (atom "mortal" [ variable "Whom" ])
                        [ positive (atom "greek" [ variable "Whom" ]) ]
                    ]
                    |> solve
                    |> get ( "mortal", 1 )
                    |> Expect.equal [ atom "mortal" [ string "Socrates" ] ]
        , test "recursive rules are solved" <|
            \_ ->
                unsafeProgram
                    [ Rule.fact (atom "link" [ string "a", string "b" ])
                    , Rule.fact (atom "link" [ string "b", string "c" ])

                    -- the rule
                    , Rule.rule
                        (atom "reachable" [ variable "X", variable "Y" ])
                        [ positive (atom "link" [ variable "X", variable "Y" ]) ]
                    , Rule.rule
                        (atom "reachable" [ variable "X", variable "Z" ])
                        [ positive (atom "link" [ variable "X", variable "Y" ])
                        , positive (atom "reachable" [ variable "Y", variable "Z" ])
                        ]
                    ]
                    |> solve
                    |> get ( "reachable", 2 )
                    |> Expect.equal
                        [ atom "reachable" [ string "a", string "c" ]
                        , atom "reachable" [ string "b", string "c" ]
                        , atom "reachable" [ string "a", string "b" ]
                        ]
        , test "can solve all-pairs reachability" <|
            \_ ->
                solve allPairsReachability
                    |> get ( "query", 1 )
                    |> Expect.equal
                        [ atom "query" [ string "d" ]
                        , atom "query" [ string "c" ]
                        , atom "query" [ string "b" ]
                        ]
        , describe "negation"
            [ test "simple (semipositive) negation" <|
                \_ ->
                    unsafeProgram
                        [ Rule.fact (atom "link" [ string "a", string "b" ])
                        , Rule.fact (atom "link" [ string "b", string "c" ])
                        , Rule.fact (atom "link" [ string "c", string "c" ])

                        -- node
                        , Rule.rule (atom "node" [ variable "name" ])
                            [ positive (atom "link" [ variable "name", anonymous ]) ]
                        , Rule.rule (atom "node" [ variable "name" ])
                            [ positive (atom "link" [ anonymous, variable "name" ]) ]

                        -- the thing with negation
                        , Rule.rule (atom "disconnected" [ variable "x", variable "y" ])
                            [ positive (atom "node" [ variable "x" ])
                            , positive (atom "node" [ variable "y" ])
                            , negative (atom "link" [ variable "x", variable "y" ])
                            ]
                        ]
                        |> solve
                        |> get ( "disconnected", 2 )
                        |> Expect.equal
                            [ atom "disconnected" [ string "c", string "b" ]
                            , atom "disconnected" [ string "c", string "a" ]
                            , atom "disconnected" [ string "b", string "b" ]
                            , atom "disconnected" [ string "b", string "a" ]
                            , atom "disconnected" [ string "a", string "c" ]
                            , atom "disconnected" [ string "a", string "a" ]
                            ]
            , test "siblings example" <|
                \_ ->
                    unsafeProgram
                        [ Rule.fact (atom "parent" [ string "Child A", string "Parent" ])
                        , Rule.fact (atom "parent" [ string "Child B", string "Parent" ])

                        -- helper for negation
                        , Rule.rule
                            (atom "samePerson" [ variable "name", variable "name" ])
                            [ positive (atom "parent" [ variable "name", anonymous ]) ]
                        , Rule.rule
                            (atom "samePerson" [ variable "name", variable "name" ])
                            [ positive (atom "parent" [ anonymous, variable "name" ]) ]

                        -- now the actual rule
                        , Rule.rule
                            (atom "siblings" [ variable "person", variable "sibling" ])
                            [ positive (atom "parent" [ variable "person", variable "parent" ])
                            , positive (atom "parent" [ variable "sibling", variable "parent" ])
                            , negative (atom "samePerson" [ variable "person", variable "sibling" ])
                            ]
                        ]
                        |> solve
                        |> get ( "siblings", 2 )
                        |> Expect.equal
                            [ atom "siblings" [ string "Child B", string "Child A" ]
                            , atom "siblings" [ string "Child A", string "Child B" ]
                            ]
            ]
        ]


{-| All-pairs reachability example from Datalog and Recursive Query
Programming.
-}
allPairsReachability : Datalog.Program
allPairsReachability =
    unsafeProgram
        [ -- base data
          Rule.fact (atom "link" [ string "a", string "b" ])
        , Rule.fact (atom "link" [ string "b", string "c" ])
        , Rule.fact (atom "link" [ string "c", string "c" ])
        , Rule.fact (atom "link" [ string "c", string "d" ])

        -- recursive rule
        , Rule.rule
            (atom "reachable" [ variable "X", variable "Y" ])
            [ positive (atom "link" [ variable "X", variable "Y" ]) ]
        , Rule.rule
            (atom "reachable" [ variable "X", variable "Y" ])
            [ positive (atom "link" [ variable "X", variable "Z" ])
            , positive (atom "reachable" [ variable "Z", variable "Y" ])
            ]

        -- query
        , Rule.rule
            (atom "query" [ variable "X" ])
            [ positive (atom "reachable" [ variable "a", variable "X" ]) ]
        ]


get : ( String, Int ) -> Dict ( String, Int ) ( a, List a ) -> List a
get key dict =
    Dict.get key dict
        |> Maybe.map (\( first, rest ) -> first :: rest)
        |> Maybe.withDefault []


unsafeProgram : List (Result x Rule) -> Datalog.Program
unsafeProgram =
    List.filterMap
        (\res ->
            case res of
                Ok cool ->
                    Just cool

                Err err ->
                    Debug.todo (Debug.toString err)
        )
        >> program
