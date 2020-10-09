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
        [ test "ground rules end up in the result" <|
            \_ ->
                solve allPairsReachability
                    |> Dict.get "link"
                    |> Expect.equal
                        (Just
                            [ Atom "link" [ Constant "c", Constant "d" ]
                            , Atom "link" [ Constant "c", Constant "c" ]
                            , Atom "link" [ Constant "b", Constant "c" ]
                            , Atom "link" [ Constant "a", Constant "b" ]
                            ]
                        )
        ]


allPairsReachability : Datalog.Program
allPairsReachability =
    Program
        [ -- base data
          Rule (Atom "link" [ Constant "a", Constant "b" ]) []
        , Rule (Atom "link" [ Constant "b", Constant "c" ]) []
        , Rule (Atom "link" [ Constant "c", Constant "c" ]) []
        , Rule (Atom "link" [ Constant "c", Constant "d" ]) []

        -- derivations
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
            (Atom "query" [ Variable "X", Variable "Y" ])
            [ Atom "reachable" [ Variable "X", Variable "Y" ] ]
        ]
