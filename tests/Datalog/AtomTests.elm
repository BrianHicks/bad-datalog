module Datalog.AtomTests exposing (..)

import Datalog.Atom exposing (..)
import Datalog.Term as Term exposing (anonymous, int, string, variable)
import Expect
import Sort.Dict as Dict
import Test exposing (..)


isGroundTest : Test
isGroundTest =
    describe "atom isGround"
        [ test "if there are terms, the atom is ground" <|
            \_ -> atom "x" [] |> isGround |> Expect.equal False
        , test "if all terms are constant, the atom is ground" <|
            \_ -> atom "x" [ string "a" ] |> isGround |> Expect.equal True
        , test "if all terms are variable, the atom is not ground" <|
            \_ -> atom "x" [ variable "X" ] |> isGround |> Expect.equal False
        , test "with a mix of constant and variable terms, the atom is not ground" <|
            \_ ->
                atom "x" [ variable "X", string "a" ]
                    |> isGround
                    |> Expect.equal False
        ]


variablesTest : Test
variablesTest =
    describe "variables"
        [ test "a variables shows up" <|
            \_ ->
                atom "x" [ variable "x" ]
                    |> variables
                    |> Expect.equal [ Term.Named "x" ]
        , test "concrete terms does not show up" <|
            \_ ->
                atom "x" [ string "a", int 1 ]
                    |> variables
                    |> Expect.equal []
        ]


unifyTest : Test
unifyTest =
    describe "unify"
        [ test "atoms with different names do not unify" <|
            \_ ->
                unify (atom "a" []) (atom "b" [])
                    |> Expect.equal Nothing
        , test "atoms with different arities do not unify" <|
            \_ ->
                unify
                    (atom "a" [ variable "A" ])
                    (atom "a" [ variable "A", variable "B" ])
                    |> Expect.equal Nothing
        , test "conflicting constants do not unify" <|
            \_ ->
                unify
                    (atom "a" [ string "x" ])
                    (atom "a" [ string "y" ])
                    |> Expect.equal Nothing
        , test "compatible constants unify" <|
            \_ ->
                unify
                    (atom "a" [ string "x" ])
                    (atom "a" [ string "x" ])
                    |> Expect.equal (Just emptySubstitutions)
        , test "a unbound var/constant pair unifies" <|
            \_ ->
                unify
                    (atom "a" [ variable "X" ])
                    (atom "a" [ string "a" ])
                    |> Expect.equal (Just (singleton (Term.Named "X") (Term.String "a")))
        , test "a bound var/constant pair unifies if it does not conflict" <|
            \_ ->
                unify
                    (atom "a" [ variable "X", variable "X" ])
                    (atom "a" [ string "a", string "a" ])
                    |> Expect.equal (Just (singleton (Term.Named "X") (Term.String "a")))
        , test "a constant/bound var pair does not unify if it conflicts" <|
            \_ ->
                unify
                    (atom "a" [ variable "X", variable "X" ])
                    (atom "a" [ string "a", string "b" ])
                    |> Expect.equal Nothing
        , test "a constant/unbound var pair unifies" <|
            \_ ->
                unify
                    (atom "a" [ string "a" ])
                    (atom "a" [ variable "X" ])
                    |> Expect.equal (Just (singleton (Term.Named "X") (Term.String "a")))
        , test "a constant/bound var pair unifies if it does not conflict" <|
            \_ ->
                unify
                    (atom "a" [ string "a", string "a" ])
                    (atom "a" [ variable "X", variable "X" ])
                    |> Expect.equal (Just (singleton (Term.Named "X") (Term.String "a")))
        , test "a bound var/constant pair does not unify if it conflicts" <|
            \_ ->
                unify
                    (atom "a" [ string "a", string "b" ])
                    (atom "a" [ variable "X", variable "X" ])
                    |> Expect.equal Nothing
        , test "variables unify with each other but don't generate any bindings" <|
            \_ ->
                unify
                    (atom "a" [ variable "X" ])
                    (atom "a" [ variable "Y" ])
                    |> Expect.equal (Just emptySubstitutions)
        , test "more than one variable can be bound in an atom" <|
            \_ ->
                unify
                    (atom "a" [ variable "X", variable "Y" ])
                    (atom "a" [ string "a", string "b" ])
                    |> Expect.equal
                        (Just
                            (Dict.fromList Term.variableSorter
                                [ ( Term.Named "X", Term.String "a" )
                                , ( Term.Named "Y", Term.String "b" )
                                ]
                            )
                        )
        , test "multiple variables can cause conflicts which fail to unify" <|
            \_ ->
                unify
                    (atom "a" [ variable "X", variable "X" ])
                    (atom "a" [ string "a", string "b" ])
                    |> Expect.equal Nothing
        , test "anonymous variables don't bind and cause conflicts" <|
            \_ ->
                unify
                    (atom "a" [ anonymous, anonymous ])
                    (atom "a" [ string "a", string "b" ])
                    |> Expect.equal (Just (Dict.empty Term.variableSorter))
        ]


substituteTest : Test
substituteTest =
    describe "substitute"
        [ test "an empty substitutions has no effect" <|
            \_ ->
                let
                    subject =
                        atom "a" [ variable "X" ]
                in
                substitute subject emptySubstitutions
                    |> Expect.equal subject
        , test "an atom with no terms is unmodified" <|
            \_ ->
                let
                    subject =
                        atom "a" []
                in
                substitute subject (singleton (Term.Named "X") (Term.String "a"))
                    |> Expect.equal subject
        , test "a constant term is not replaed" <|
            \_ ->
                let
                    subject =
                        atom "a" [ string "a" ]
                in
                substitute subject (singleton (Term.Named "X") (Term.String "a"))
                    |> Expect.equal subject
        , test "a variable term is replaced if there is a replacement" <|
            \_ ->
                substitute
                    (atom "a" [ variable "X" ])
                    (singleton (Term.Named "X") (Term.String "a"))
                    |> Expect.equal (atom "a" [ string "a" ])
        , test "a variable term is not replace if there is no replacement" <|
            \_ ->
                let
                    subject =
                        atom "a" [ variable "X" ]
                in
                substitute subject (singleton (Term.Named "Y") (Term.String "a"))
                    |> Expect.equal subject
        ]


mergeSubstitutionsTest : Test
mergeSubstitutionsTest =
    describe "mergeSubstitutions"
        [ test "keys in left should be preserved" <|
            \_ ->
                mergeSubstitutions
                    (singleton (Term.Named "X") (Term.String "a"))
                    emptySubstitutions
                    |> Dict.get (Term.Named "X")
                    |> Expect.equal (Just (Term.String "a"))
        , test "keys in right should be preserved" <|
            \_ ->
                mergeSubstitutions
                    emptySubstitutions
                    (singleton (Term.Named "X") (Term.String "a"))
                    |> Dict.get (Term.Named "X")
                    |> Expect.equal (Just (Term.String "a"))
        , test "keys in both should be preserved" <|
            \_ ->
                mergeSubstitutions
                    (singleton (Term.Named "X") (Term.String "a"))
                    (singleton (Term.Named "Y") (Term.String "b"))
                    |> Expect.all
                        [ Dict.get (Term.Named "X") >> Expect.equal (Just (Term.String "a"))
                        , Dict.get (Term.Named "Y") >> Expect.equal (Just (Term.String "b"))
                        ]
        , test "keys in left take precedence" <|
            \_ ->
                mergeSubstitutions
                    (singleton (Term.Named "X") (Term.String "a"))
                    (singleton (Term.Named "X") (Term.String "b"))
                    |> Dict.get (Term.Named "X")
                    |> Expect.equal (Just (Term.String "a"))
        ]


singleton =
    Dict.singleton Term.variableSorter
