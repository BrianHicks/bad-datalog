module DatalogTests exposing (..)

import Datalog exposing (..)
import Datalog.Atom as Atom exposing (Atom, atom)
import Datalog.Term as Term exposing (string, variable)
import Dict exposing (Dict)
import Expect
import Test exposing (..)


solveTest : Test
solveTest =
    describe "solve"
        [ test "ground rules are solved" <|
            \_ ->
                Program
                    [ Rule (atom "greek" [ string "Socrates" ]) [] ]
                    |> solve
                    |> get "greek"
                    |> Expect.equal [ atom "greek" [ string "Socrates" ] ]
        , test "non-ground rules are solved" <|
            \_ ->
                Program
                    [ Rule (atom "greek" [ string "Socrates" ]) []
                    , Rule (atom "mortal" [ variable "Whom" ]) [ atom "greek" [ variable "Whom" ] ]
                    ]
                    |> solve
                    |> get "mortal"
                    |> Expect.equal [ atom "mortal" [ string "Socrates" ] ]
        , test "recursive rules are solved" <|
            \_ ->
                Program
                    [ Rule (atom "link" [ string "a", string "b" ]) []
                    , Rule (atom "link" [ string "b", string "c" ]) []

                    -- the rule
                    , Rule
                        (atom "reachable" [ variable "X", variable "Y" ])
                        [ atom "link" [ variable "X", variable "Y" ] ]
                    , Rule
                        (atom "reachable" [ variable "X", variable "Z" ])
                        [ atom "link" [ variable "X", variable "Y" ]
                        , atom "reachable" [ variable "Y", variable "Z" ]
                        ]
                    ]
                    |> solve
                    |> get "reachable"
                    |> Expect.equal
                        [ atom "reachable" [ string "a", string "c" ]
                        , atom "reachable" [ string "a", string "b" ]
                        , atom "reachable" [ string "b", string "c" ]
                        ]
        , test "can solve all-pairs reachability" <|
            \_ ->
                solve allPairsReachability
                    |> get "query"
                    |> Expect.equal
                        [ atom "query" [ string "b" ]
                        , atom "query" [ string "d" ]
                        , atom "query" [ string "c" ]
                        ]
        ]


{-| All-pairs reachability example from Datalog and Recursive Query
Programming.
-}
allPairsReachability : Datalog.Program
allPairsReachability =
    Program
        [ -- base data
          Rule (atom "link" [ string "a", string "b" ]) []
        , Rule (atom "link" [ string "b", string "c" ]) []
        , Rule (atom "link" [ string "c", string "c" ]) []
        , Rule (atom "link" [ string "c", string "d" ]) []

        -- recursive rule
        , Rule
            (atom "reachable" [ variable "X", variable "Y" ])
            [ atom "link" [ variable "X", variable "Y" ] ]
        , Rule
            (atom "reachable" [ variable "X", variable "Y" ])
            [ atom "link" [ variable "X", variable "Z" ]
            , atom "reachable" [ variable "Z", variable "Y" ]
            ]

        -- query
        , Rule
            (atom "query" [ variable "X" ])
            [ atom "reachable" [ variable "a", variable "X" ] ]
        ]


get : String -> Dict String ( a, List a ) -> List a
get key dict =
    Dict.get key dict
        |> Maybe.map (\( first, rest ) -> first :: rest)
        |> Maybe.withDefault []
