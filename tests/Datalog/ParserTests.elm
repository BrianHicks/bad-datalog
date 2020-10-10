module Datalog.ParserTests exposing (..)

import Datalog exposing (Program(..))
import Datalog.Atom as Atom
import Datalog.Parser exposing (..)
import Datalog.Term as Term
import Expect
import Test exposing (..)


parseTests : Test
parseTests =
    describe "parse"
        [ describe "success"
            [ test "a fact" <|
                \_ ->
                    Expect.equal
                        (Ok (Program [ Datalog.Rule (Atom.Atom "man" [ Term.Constant "Socrates" ]) [] ]))
                        (parse "man(\"Socrates\")")
            , test "a rule with a variable" <|
                \_ ->
                    Expect.equal
                        (Ok
                            (Program
                                [ Datalog.Rule
                                    (Atom.Atom "mortal" [ Term.Variable "whom" ])
                                    [ Atom.Atom "man" [ Term.Variable "whom" ] ]
                                ]
                            )
                        )
                        (parse "mortal(whom) :- man(whom)")
            , test "a rule with multiple clauses" <|
                \_ ->
                    Expect.equal
                        (Ok
                            (Program
                                [ Datalog.Rule
                                    (Atom.Atom "ancestor" [ Term.Variable "Child", Term.Variable "Ancestor" ])
                                    [ Atom.Atom "parent" [ Term.Variable "Child", Term.Variable "Parent" ]
                                    , Atom.Atom "ancestor" [ Term.Variable "Parent", Term.Variable "Ancestor" ]
                                    ]
                                ]
                            )
                        )
                        (parse "ancestor(Child, Ancestor) :- parent(Child, Parent), ancestor(Parent, Ancestor)")
            ]
        , describe "failure"
            [ test "leaving the terms off an atom is not allowed" <|
                \_ -> Expect.err (parse "man")
            , test "leaving the closing quote off a constant is not allowed" <|
                \_ -> Expect.err (parse "man(\"Socrates")
            , test "leaving the closing parenthesis off a term list is not allowed" <|
                \_ -> Expect.err (parse "man(\"Socrates\"")
            , test "adding a trailing comma in a term list is not allowed" <|
                \_ -> Expect.err (parse "man(\"Socrates\",)")
            , test "having an implies horn but no body is not allowed" <|
                \_ -> Expect.err (parse "mortal(whom) :-")
            ]
        ]
