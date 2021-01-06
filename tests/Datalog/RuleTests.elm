module Datalog.RuleTests exposing (..)

import Datalog.Atom exposing (atom)
import Datalog.Negatable as Negatable exposing (negative, positive)
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
                    [ positive (atom "greek" [ variable "whom" ]) ]
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
                    [ positive (atom "greek" [ anonymous ]) ]
                    |> Expect.equal (Err UnnamedHeadVariable)
        , test "negative terms are allowed if they also appear in a positive form" <|
            \_ ->
                rule
                    (atom "unreachable" [ variable "a", variable "b" ])
                    [ positive (atom "node" [ variable "a" ])
                    , positive (atom "node" [ variable "b" ])
                    , negative (atom "reachable" [ variable "a", variable "b" ])
                    ]
                    |> Expect.ok
        , test "negative terms which are introduced outside the head are still allowed if they appear in positive form" <|
            \_ ->
                rule
                    (atom "peopleWithoutEmails" [ variable "Name" ])
                    [ positive (atom "people" [ variable "Id", variable "Name" ])
                    , negative (atom "peopleToEmails" [ variable "Id", anonymous ])
                    ]
                    |> Expect.ok
        , test "negative terms are not allowed if they don't also appear in a positive form" <|
            \_ ->
                rule
                    (atom "immortal" [ variable "whom" ])
                    [ negative (atom "mortal" [ variable "whom" ]) ]
                    |> Expect.equal (Err VariableAppearsNegatedButNotPositive)
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
                    |> Expect.equal (Err UnnamedHeadVariable)
        ]
