module DatalogTests exposing (..)

import Datalog exposing (..)
import Datalog.Atom as Atom exposing (Atom(..))
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
                    [ Rule (Atom "greek" [ string "Socrates" ]) [] ]
                    |> solve
                    |> get "greek"
                    |> Expect.equal [ Atom "greek" [ string "Socrates" ] ]
        , test "non-ground rules are solved" <|
            \_ ->
                Program
                    [ Rule (Atom "greek" [ string "Socrates" ]) []
                    , Rule (Atom "mortal" [ variable "Whom" ]) [ Atom "greek" [ variable "Whom" ] ]
                    ]
                    |> solve
                    |> get "mortal"
                    |> Expect.equal [ Atom "mortal" [ string "Socrates" ] ]
        , test "recursive rules are solved" <|
            \_ ->
                Program
                    [ Rule (Atom "link" [ string "a", string "b" ]) []
                    , Rule (Atom "link" [ string "b", string "c" ]) []

                    -- the rule
                    , Rule
                        (Atom "reachable" [ variable "X", variable "Y" ])
                        [ Atom "link" [ variable "X", variable "Y" ] ]
                    , Rule
                        (Atom "reachable" [ variable "X", variable "Z" ])
                        [ Atom "link" [ variable "X", variable "Y" ]
                        , Atom "reachable" [ variable "Y", variable "Z" ]
                        ]
                    ]
                    |> solve
                    |> get "reachable"
                    |> Expect.equal
                        [ Atom "reachable" [ string "a", string "c" ]
                        , Atom "reachable" [ string "a", string "b" ]
                        , Atom "reachable" [ string "b", string "c" ]
                        ]
        , test "can solve all-pairs reachability" <|
            \_ ->
                solve allPairsReachability
                    |> get "query"
                    |> Expect.equal
                        [ Atom "query" [ string "b" ]
                        , Atom "query" [ string "d" ]
                        , Atom "query" [ string "c" ]
                        ]
        ]


{-| All-pairs reachability example from Datalog and Recursive Query
Programming.
-}
allPairsReachability : Datalog.Program
allPairsReachability =
    Program
        [ -- base data
          Rule (Atom "link" [ string "a", string "b" ]) []
        , Rule (Atom "link" [ string "b", string "c" ]) []
        , Rule (Atom "link" [ string "c", string "c" ]) []
        , Rule (Atom "link" [ string "c", string "d" ]) []

        -- recursive rule
        , Rule
            (Atom "reachable" [ variable "X", variable "Y" ])
            [ Atom "link" [ variable "X", variable "Y" ] ]
        , Rule
            (Atom "reachable" [ variable "X", variable "Y" ])
            [ Atom "link" [ variable "X", variable "Z" ]
            , Atom "reachable" [ variable "Z", variable "Y" ]
            ]

        -- query
        , Rule
            (Atom "query" [ variable "X" ])
            [ Atom "reachable" [ variable "a", variable "X" ] ]
        ]


get : String -> Dict String ( a, List a ) -> List a
get key dict =
    Dict.get key dict
        |> Maybe.map (\( first, rest ) -> first :: rest)
        |> Maybe.withDefault []
