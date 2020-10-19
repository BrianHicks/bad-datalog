module Datalog.RuleTests exposing (..)

import Datalog.Atom exposing (atom)
import Datalog.Rule exposing (..)
import Datalog.Term exposing (anonymous, int, string, variable)
import Expect
import Test exposing (..)


ruleTest : Test
ruleTest =
    describe "rule"
        [ test "a range-restricted rule is allowed" <|
            \_ ->
                rule
                    (atom "mortal" [ variable "whom" ])
                    [ atom "greek" [ variable "whom" ] ]
                    |> Expect.ok
        , test "a non-range-restricted rule is not allowed" <|
            \_ ->
                rule
                    (atom "mortal" [ variable "whom" ])
                    []
                    |> Expect.equal (Err NotRangeRestricted)
        , test "anonymous terms are not allowed in the head" <|
            \_ ->
                rule
                    (atom "mortal" [ anonymous ])
                    [ atom "greek" [ anonymous ] ]
                    |> Expect.equal (Err NotRangeRestricted)
        ]


factTest : Test
factTest =
    describe "fact"
        [ test "a fact with all concrete terms is allowed" <|
            \_ ->
                fact (atom "age" [ string "Socrates", int 2490 ])
                    |> Expect.ok
        , test "a fact with a named variable is not allowed" <|
            \_ ->
                fact (atom "notGreat" [ variable "unbound" ])
                    |> Expect.equal (Err NotRangeRestricted)
        , test "a fact with an anonymous variable is not allowed" <|
            \_ ->
                fact (atom "notGreat" [ anonymous ])
                    |> Expect.equal (Err NotRangeRestricted)
        ]
