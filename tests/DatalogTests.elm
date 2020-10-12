module DatalogTests exposing (..)

import Datalog exposing (..)
import Datalog.Atom as Atom exposing (Atom(..))
import Datalog.Term exposing (Term(..))
import Dict
import Expect
import Test exposing (..)


solveTest : Test
solveTest =
    describe "solve"
        [ test "ground rules are solved" <|
            \_ ->
                Program
                    [ Rule (Atom "greek" [ Constant "Socrates" ]) [] ]
                    |> solve
                    |> Dict.get "greek"
                    |> Expect.equal (Just [ Atom "greek" [ Constant "Socrates" ] ])
        , test "non-ground rules are solved" <|
            \_ ->
                Program
                    [ Rule (Atom "greek" [ Constant "Socrates" ]) []
                    , Rule (Atom "mortal" [ Variable "Whom" ]) [ Atom "greek" [ Variable "Whom" ] ]
                    ]
                    |> solve
                    |> Dict.get "mortal"
                    |> Expect.equal (Just [ Atom "mortal" [ Constant "Socrates" ] ])
        , test "recursive rules are solved" <|
            \_ ->
                Program
                    [ Rule (Atom "link" [ Constant "a", Constant "b" ]) []
                    , Rule (Atom "link" [ Constant "b", Constant "c" ]) []

                    -- the rule
                    , Rule
                        (Atom "reachable" [ Variable "X", Variable "Y" ])
                        [ Atom "link" [ Variable "X", Variable "Y" ] ]
                    , Rule
                        (Atom "reachable" [ Variable "X", Variable "Z" ])
                        [ Atom "link" [ Variable "X", Variable "Y" ]
                        , Atom "reachable" [ Variable "Y", Variable "Z" ]
                        ]
                    ]
                    |> solve
                    |> Dict.get "reachable"
                    |> Expect.equal
                        (Just
                            [ Atom "reachable" [ Constant "a", Constant "c" ]
                            , Atom "reachable" [ Constant "a", Constant "b" ]
                            , Atom "reachable" [ Constant "b", Constant "c" ]
                            ]
                        )
        , test "can solve all-pairs reachability" <|
            \_ ->
                solve allPairsReachability
                    |> Dict.get "query"
                    |> Expect.equal
                        (Just
                            [ Atom "query" [ Constant "b" ]
                            , Atom "query" [ Constant "d" ]
                            , Atom "query" [ Constant "c" ]
                            ]
                        )
        ]


{-| All-pairs reachability example from Datalog and Recursive Query
Programming.
-}
allPairsReachability : Datalog.Program
allPairsReachability =
    Program
        [ -- base data
          Rule (Atom "link" [ Constant "a", Constant "b" ]) []
        , Rule (Atom "link" [ Constant "b", Constant "c" ]) []
        , Rule (Atom "link" [ Constant "c", Constant "c" ]) []
        , Rule (Atom "link" [ Constant "c", Constant "d" ]) []

        -- recursive rule
        , Rule
            (Atom "reachable" [ Variable "X", Variable "Y" ])
            [ Atom "link" [ Variable "X", Variable "Y" ] ]
        , Rule
            (Atom "reachable" [ Variable "X", Variable "Y" ])
            [ Atom "link" [ Variable "X", Variable "Z" ]
            , Atom "reachable" [ Variable "Z", Variable "Y" ]
            ]

        -- query
        , Rule
            (Atom "query" [ Variable "X" ])
            [ Atom "reachable" [ Variable "a", Variable "X" ] ]
        ]
