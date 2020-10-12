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
                        (parse "man(\"Socrates\").")
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
                        (parse "mortal(whom) :- man(whom).")
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
                        (parse "ancestor(Child, Ancestor) :- parent(Child, Parent), ancestor(Parent, Ancestor).")
            , test "a program with multiple rules" <|
                \_ ->
                    Expect.equal
                        (Ok
                            (Program
                                [ Datalog.Rule (Atom.Atom "man" [ Term.Constant "Socrates" ]) []
                                , Datalog.Rule (Atom.Atom "mortal" [ Term.Variable "Whom" ])
                                    [ Atom.Atom "man" [ Term.Variable "Whom" ] ]
                                ]
                            )
                        )
                        (parse "man(\"Socrates\").\nmortal(Whom) :- man(Whom).")
            , test "a program with whitespace between rules" <|
                \_ ->
                    Expect.equal
                        (Ok
                            (Program
                                [ Datalog.Rule (Atom.Atom "man" [ Term.Constant "Socrates" ]) []
                                , Datalog.Rule (Atom.Atom "mortal" [ Term.Variable "Whom" ])
                                    [ Atom.Atom "man" [ Term.Variable "Whom" ] ]
                                ]
                            )
                        )
                        (parse "man(\"Socrates\").\n\nmortal(Whom) :- man(Whom).")
            , test "a rule with newlines in between body atoms" <|
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
                        (parse "ancestor(Child, Ancestor) :-\n    parent(Child, Parent),\n    ancestor(Parent, Ancestor).")
            ]
        , describe "failure"
            [ test "leaving the terms off an atom is not allowed" <|
                \_ -> Expect.err (parse "man")
            , test "leaving the closing quote off a constant is not allowed" <|
                \_ -> Expect.err (parse "man(\"Socrates")
            , test "leaving the closing parenthesis off a term list is not allowed" <|
                \_ -> Expect.err (parse "man(\"Socrates\"")
            , test "leaving a period off a fact is not allowed" <|
                \_ -> Expect.err (parse "man(\"Socrates\")")
            , test "adding a trailing comma in a term list is not allowed" <|
                \_ -> Expect.err (parse "man(\"Socrates\",)")
            , test "having an implies horn but no body is not allowed" <|
                \_ -> Expect.err (parse "mortal(whom) :-")
            , test "having a trailing comma in a rule body is not allowed" <|
                \_ -> Expect.err (parse "ancestor(Child, Ancestor) :- parent(Child, Parent),")
            , test "leaving a period off a rule is not allowed" <|
                \_ -> Expect.err (parse "mortal(Whom) :- man(Whom)")
            ]
        ]
