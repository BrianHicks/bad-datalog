module Datalog.AtomTests exposing (..)

import Datalog.Atom exposing (..)
import Datalog.Term as Term exposing (string, variable)
import Dict
import Expect
import Test exposing (..)


isGroundTest : Test
isGroundTest =
    describe "isGround"
        [ test "if there are terms, the atom is ground" <|
            \_ -> Atom "x" [] |> isGround |> Expect.equal False
        , test "if all terms are constant, the atom is ground" <|
            \_ -> Atom "x" [ string "a" ] |> isGround |> Expect.equal True
        , test "if all terms are variable, the atom is not ground" <|
            \_ -> Atom "x" [ variable "X" ] |> isGround |> Expect.equal False
        , test "with a mix of constant and variable terms, the atom is not ground" <|
            \_ ->
                Atom "x" [ variable "X", string "a" ]
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
                    (Atom "a" [ variable "A" ])
                    (Atom "a" [ variable "A", variable "B" ])
                    |> Expect.equal Nothing
        , test "conflicting constants do not unify" <|
            \_ ->
                unify
                    (Atom "a" [ string "x" ])
                    (Atom "a" [ string "y" ])
                    |> Expect.equal Nothing
        , test "compatible constants unify" <|
            \_ ->
                unify
                    (Atom "a" [ string "x" ])
                    (Atom "a" [ string "x" ])
                    |> Expect.equal (Just Dict.empty)
        , test "a unbound var/constant pair unifies" <|
            \_ ->
                unify
                    (Atom "a" [ variable "X" ])
                    (Atom "a" [ string "a" ])
                    |> Expect.equal (Just (Dict.singleton "X" (Term.String "a")))
        , test "a bound var/constant pair unifies if it does not conflict" <|
            \_ ->
                unify
                    (Atom "a" [ variable "X", variable "X" ])
                    (Atom "a" [ string "a", string "a" ])
                    |> Expect.equal (Just (Dict.singleton "X" (Term.String "a")))
        , test "a constant/bound var pair does not unify if it conflicts" <|
            \_ ->
                unify
                    (Atom "a" [ variable "X", variable "X" ])
                    (Atom "a" [ string "a", string "b" ])
                    |> Expect.equal Nothing
        , test "a constant/unbound var pair unifies" <|
            \_ ->
                unify
                    (Atom "a" [ string "a" ])
                    (Atom "a" [ variable "X" ])
                    |> Expect.equal (Just (Dict.singleton "X" (Term.String "a")))
        , test "a constant/bound var pair unifies if it does not conflict" <|
            \_ ->
                unify
                    (Atom "a" [ string "a", string "a" ])
                    (Atom "a" [ variable "X", variable "X" ])
                    |> Expect.equal (Just (Dict.singleton "X" (Term.String "a")))
        , test "a bound var/constant pair does not unify if it conflicts" <|
            \_ ->
                unify
                    (Atom "a" [ string "a", string "b" ])
                    (Atom "a" [ variable "X", variable "X" ])
                    |> Expect.equal Nothing
        , test "variables unify with each other but don't generate any bindings" <|
            \_ ->
                unify
                    (Atom "a" [ variable "X" ])
                    (Atom "a" [ variable "Y" ])
                    |> Expect.equal (Just Dict.empty)
        , test "more than one variable can be bound in an atom" <|
            \_ ->
                unify
                    (Atom "a" [ variable "X", variable "Y" ])
                    (Atom "a" [ string "a", string "b" ])
                    |> Expect.equal
                        (Just
                            (Dict.fromList
                                [ ( "X", Term.String "a" )
                                , ( "Y", Term.String "b" )
                                ]
                            )
                        )
        , test "multiple variables can cause conflicts which fail to unify" <|
            \_ ->
                unify
                    (Atom "a" [ variable "X", variable "X" ])
                    (Atom "a" [ string "a", string "b" ])
                    |> Expect.equal Nothing
        ]


substituteTest : Test
substituteTest =
    describe "substitute"
        [ test "an empty substitutions has no effect" <|
            \_ ->
                let
                    atom =
                        Atom "a" [ variable "X" ]
                in
                substitute atom Dict.empty
                    |> Expect.equal atom
        , test "an atom with no terms is unmodified" <|
            \_ ->
                let
                    atom =
                        Atom "a" []
                in
                substitute atom (Dict.singleton "X" (Term.String "a"))
                    |> Expect.equal atom
        , test "a constant term is not replaed" <|
            \_ ->
                let
                    atom =
                        Atom "a" [ string "a" ]
                in
                substitute atom (Dict.singleton "X" (Term.String "a"))
                    |> Expect.equal atom
        , test "a variable term is replaced if there is a replacement" <|
            \_ ->
                substitute
                    (Atom "a" [ variable "X" ])
                    (Dict.singleton "X" (Term.String "a"))
                    |> Expect.equal (Atom "a" [ string "a" ])
        , test "a variable term is not replace if there is no replacement" <|
            \_ ->
                let
                    atom =
                        Atom "a" [ variable "X" ]
                in
                substitute atom (Dict.singleton "Y" (Term.String "a"))
                    |> Expect.equal atom
        ]


mergeSubstitutionsTest : Test
mergeSubstitutionsTest =
    describe "mergeSubstitutions"
        [ test "keys in left should be preserved" <|
            \_ ->
                mergeSubstitutions
                    (Dict.singleton "X" (Term.String "a"))
                    Dict.empty
                    |> Dict.get "X"
                    |> Expect.equal (Just (Term.String "a"))
        , test "keys in right should be preserved" <|
            \_ ->
                mergeSubstitutions
                    Dict.empty
                    (Dict.singleton "X" (Term.String "a"))
                    |> Dict.get "X"
                    |> Expect.equal (Just (Term.String "a"))
        , test "keys in both should be preserved" <|
            \_ ->
                mergeSubstitutions
                    (Dict.singleton "X" (Term.String "a"))
                    (Dict.singleton "Y" (Term.String "b"))
                    |> Expect.all
                        [ Dict.get "X" >> Expect.equal (Just (Term.String "a"))
                        , Dict.get "Y" >> Expect.equal (Just (Term.String "b"))
                        ]
        , test "keys in left take precedence" <|
            \_ ->
                mergeSubstitutions
                    (Dict.singleton "X" (Term.String "a"))
                    (Dict.singleton "X" (Term.String "b"))
                    |> Dict.get "X"
                    |> Expect.equal (Just (Term.String "a"))
        ]
