module Datalog.AtomTests exposing (..)

import Datalog.Atom exposing (..)
import Datalog.Term exposing (Term(..))
import Dict
import Expect
import Test exposing (..)


isGroundTest : Test
isGroundTest =
    describe "isGround"
        [ test "if there are terms, the atom is ground" <|
            \_ -> Atom "x" [] |> isGround |> Expect.equal False
        , test "if all terms are constant, the atom is ground" <|
            \_ -> Atom "x" [ Constant "a" ] |> isGround |> Expect.equal True
        , test "if all terms are variable, the atom is not ground" <|
            \_ -> Atom "x" [ Variable "X" ] |> isGround |> Expect.equal False
        , test "with a mix of constant and variable terms, the atom is not ground" <|
            \_ ->
                Atom "x" [ Variable "X", Constant "a" ]
                    |> isGround
                    |> Expect.equal False
        ]


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


substituteTest : Test
substituteTest =
    describe "substitute"
        [ test "an empty substitutions has no effect" <|
            \_ ->
                let
                    atom =
                        Atom "a" [ Variable "X" ]
                in
                substitute atom Dict.empty
                    |> Expect.equal atom
        , test "an atom with no terms is unmodified" <|
            \_ ->
                let
                    atom =
                        Atom "a" []
                in
                substitute atom (Dict.singleton "X" "a")
                    |> Expect.equal atom
        , test "a constant term is not replaed" <|
            \_ ->
                let
                    atom =
                        Atom "a" [ Constant "a" ]
                in
                substitute atom (Dict.singleton "X" "a")
                    |> Expect.equal atom
        , test "a variable term is replaced if there is a replacement" <|
            \_ ->
                substitute
                    (Atom "a" [ Variable "X" ])
                    (Dict.singleton "X" "a")
                    |> Expect.equal (Atom "a" [ Constant "a" ])
        , test "a variable term is not replace if there is no replacement" <|
            \_ ->
                let
                    atom =
                        Atom "a" [ Variable "X" ]
                in
                substitute atom (Dict.singleton "Y" "a")
                    |> Expect.equal atom
        ]
