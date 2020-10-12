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
                    [ Rule (Atom "greek" [ String "Socrates" ]) [] ]
                    |> solve
                    |> Dict.get "greek"
                    |> Expect.equal (Just [ Atom "greek" [ String "Socrates" ] ])
        , test "non-ground rules are solved" <|
            \_ ->
                Program
                    [ Rule (Atom "greek" [ String "Socrates" ]) []
                    , Rule (Atom "mortal" [ Variable "Whom" ]) [ Atom "greek" [ Variable "Whom" ] ]
                    ]
                    |> solve
                    |> Dict.get "mortal"
                    |> Expect.equal (Just [ Atom "mortal" [ String "Socrates" ] ])
        , test "recursive rules are solved" <|
            \_ ->
                Program
                    [ Rule (Atom "link" [ String "a", String "b" ]) []
                    , Rule (Atom "link" [ String "b", String "c" ]) []

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
                            [ Atom "reachable" [ String "a", String "c" ]
                            , Atom "reachable" [ String "a", String "b" ]
                            , Atom "reachable" [ String "b", String "c" ]
                            ]
                        )
        , test "can solve all-pairs reachability" <|
            \_ ->
                solve allPairsReachability
                    |> Dict.get "query"
                    |> Expect.equal
                        (Just
                            [ Atom "query" [ String "b" ]
                            , Atom "query" [ String "d" ]
                            , Atom "query" [ String "c" ]
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
          Rule (Atom "link" [ String "a", String "b" ]) []
        , Rule (Atom "link" [ String "b", String "c" ]) []
        , Rule (Atom "link" [ String "c", String "c" ]) []
        , Rule (Atom "link" [ String "c", String "d" ]) []

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
