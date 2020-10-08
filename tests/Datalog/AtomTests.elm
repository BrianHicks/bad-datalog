module Datalog.AtomTests exposing (..)

import Datalog.Atom exposing (..)
import Datalog.Term exposing (Term(..))
import Dict
import Expect
import Test exposing (..)


unifyTest : Test
unifyTest =
    describe "unify"
        [ test "atoms with different names do not unify" <|
            \_ ->
                unify (Atom "a" []) (Atom "b" [])
                    |> Expect.equal Nothing
        , test "atoms with different arities do not unify" <|
            \_ ->
                unify
                    (Atom "a" [ Variable "A" ])
                    (Atom "a" [ Variable "A", Variable "B" ])
                    |> Expect.equal Nothing
        , test "conflicting constants do not unify" <|
            \_ ->
                unify
                    (Atom "a" [ Constant "x" ])
                    (Atom "a" [ Constant "y" ])
                    |> Expect.equal Nothing
        , test "compatible constants unify" <|
            \_ ->
                unify
                    (Atom "a" [ Constant "x" ])
                    (Atom "a" [ Constant "x" ])
                    |> Expect.equal (Just Dict.empty)
        , test "a unbound var/constant pair unifies" <|
            \_ ->
                unify
                    (Atom "a" [ Variable "X" ])
                    (Atom "a" [ Constant "a" ])
                    |> Expect.equal (Just (Dict.singleton "X" "a"))
        , test "a bound var/constant pair unifies if it does not conflict" <|
            \_ ->
                unify
                    (Atom "a" [ Variable "X", Variable "X" ])
                    (Atom "a" [ Constant "a", Constant "a" ])
                    |> Expect.equal (Just (Dict.singleton "X" "a"))
        , test "a constant/bound var pair does not unify if it conflicts" <|
            \_ ->
                unify
                    (Atom "a" [ Variable "X", Variable "X" ])
                    (Atom "a" [ Constant "a", Constant "b" ])
                    |> Expect.equal Nothing
        , test "a constant/unbound var pair unifies" <|
            \_ ->
                unify
                    (Atom "a" [ Constant "a" ])
                    (Atom "a" [ Variable "X" ])
                    |> Expect.equal (Just (Dict.singleton "X" "a"))
        , test "a constant/bound var pair unifies if it does not conflict" <|
            \_ ->
                unify
                    (Atom "a" [ Constant "a", Constant "a" ])
                    (Atom "a" [ Variable "X", Variable "X" ])
                    |> Expect.equal (Just (Dict.singleton "X" "a"))
        , test "a bound var/constant pair does not unify if it conflicts" <|
            \_ ->
                unify
                    (Atom "a" [ Constant "a", Constant "b" ])
                    (Atom "a" [ Variable "X", Variable "X" ])
                    |> Expect.equal Nothing
        , test "variables unify with each other but don't generate any bindings" <|
            \_ ->
                unify
                    (Atom "a" [ Variable "X" ])
                    (Atom "a" [ Variable "Y" ])
                    |> Expect.equal (Just Dict.empty)
        , test "more than one variable can be bound in an atom" <|
            \_ ->
                unify
                    (Atom "a" [ Variable "X", Variable "Y" ])
                    (Atom "a" [ Constant "a", Constant "b" ])
                    |> Expect.equal
                        (Just
                            (Dict.fromList
                                [ ( "X", "a" )
                                , ( "Y", "b" )
                                ]
                            )
                        )
        , test "multiple variables can cause conflicts which fail to unify" <|
            \_ ->
                unify
                    (Atom "a" [ Variable "X", Variable "X" ])
                    (Atom "a" [ Constant "a", Constant "b" ])
                    |> Expect.equal Nothing
        ]
