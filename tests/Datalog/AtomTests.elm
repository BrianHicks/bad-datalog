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
            \_ -> Atom "x" [ String "a" ] |> isGround |> Expect.equal True
        , test "if all terms are variable, the atom is not ground" <|
            \_ -> Atom "x" [ Variable "X" ] |> isGround |> Expect.equal False
        , test "with a mix of constant and variable terms, the atom is not ground" <|
            \_ ->
                Atom "x" [ Variable "X", String "a" ]
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
                    (Atom "a" [ String "x" ])
                    (Atom "a" [ String "y" ])
                    |> Expect.equal Nothing
        , test "compatible constants unify" <|
            \_ ->
                unify
                    (Atom "a" [ String "x" ])
                    (Atom "a" [ String "x" ])
                    |> Expect.equal (Just Dict.empty)
        , test "a unbound var/constant pair unifies" <|
            \_ ->
                unify
                    (Atom "a" [ Variable "X" ])
                    (Atom "a" [ String "a" ])
                    |> Expect.equal (Just (Dict.singleton "X" (String "a")))
        , test "a bound var/constant pair unifies if it does not conflict" <|
            \_ ->
                unify
                    (Atom "a" [ Variable "X", Variable "X" ])
                    (Atom "a" [ String "a", String "a" ])
                    |> Expect.equal (Just (Dict.singleton "X" (String "a")))
        , test "a constant/bound var pair does not unify if it conflicts" <|
            \_ ->
                unify
                    (Atom "a" [ Variable "X", Variable "X" ])
                    (Atom "a" [ String "a", String "b" ])
                    |> Expect.equal Nothing
        , test "a constant/unbound var pair unifies" <|
            \_ ->
                unify
                    (Atom "a" [ String "a" ])
                    (Atom "a" [ Variable "X" ])
                    |> Expect.equal (Just (Dict.singleton "X" (String "a")))
        , test "a constant/bound var pair unifies if it does not conflict" <|
            \_ ->
                unify
                    (Atom "a" [ String "a", String "a" ])
                    (Atom "a" [ Variable "X", Variable "X" ])
                    |> Expect.equal (Just (Dict.singleton "X" (String "a")))
        , test "a bound var/constant pair does not unify if it conflicts" <|
            \_ ->
                unify
                    (Atom "a" [ String "a", String "b" ])
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
                    (Atom "a" [ String "a", String "b" ])
                    |> Expect.equal
                        (Just
                            (Dict.fromList
                                [ ( "X", String "a" )
                                , ( "Y", String "b" )
                                ]
                            )
                        )
        , test "multiple variables can cause conflicts which fail to unify" <|
            \_ ->
                unify
                    (Atom "a" [ Variable "X", Variable "X" ])
                    (Atom "a" [ String "a", String "b" ])
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
                substitute atom (Dict.singleton "X" (String "a"))
                    |> Expect.equal atom
        , test "a constant term is not replaed" <|
            \_ ->
                let
                    atom =
                        Atom "a" [ String "a" ]
                in
                substitute atom (Dict.singleton "X" (String "a"))
                    |> Expect.equal atom
        , test "a variable term is replaced if there is a replacement" <|
            \_ ->
                substitute
                    (Atom "a" [ Variable "X" ])
                    (Dict.singleton "X" (String "a"))
                    |> Expect.equal (Atom "a" [ String "a" ])
        , test "a variable term is not replace if there is no replacement" <|
            \_ ->
                let
                    atom =
                        Atom "a" [ Variable "X" ]
                in
                substitute atom (Dict.singleton "Y" (String "a"))
                    |> Expect.equal atom
        ]


mergeSubstitutionsTest : Test
mergeSubstitutionsTest =
    describe "mergeSubstitutions"
        [ test "keys in left should be preserved" <|
            \_ ->
                mergeSubstitutions
                    (Dict.singleton "X" (String "a"))
                    Dict.empty
                    |> Dict.get "X"
                    |> Expect.equal (Just (String "a"))
        , test "keys in right should be preserved" <|
            \_ ->
                mergeSubstitutions
                    Dict.empty
                    (Dict.singleton "X" (String "a"))
                    |> Dict.get "X"
                    |> Expect.equal (Just (String "a"))
        , test "keys in both should be preserved" <|
            \_ ->
                mergeSubstitutions
                    (Dict.singleton "X" (String "a"))
                    (Dict.singleton "Y" (String "b"))
                    |> Expect.all
                        [ Dict.get "X" >> Expect.equal (Just (String "a"))
                        , Dict.get "Y" >> Expect.equal (Just (String "b"))
                        ]
        , test "keys in left take precedence" <|
            \_ ->
                mergeSubstitutions
                    (Dict.singleton "X" (String "a"))
                    (Dict.singleton "X" (String "b"))
                    |> Dict.get "X"
                    |> Expect.equal (Just (String "a"))
        ]
