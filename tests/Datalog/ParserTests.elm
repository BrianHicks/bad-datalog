module Datalog.ParserTests exposing (..)

import Datalog exposing (Program(..))
import Datalog.Atom as Atom
import Datalog.Parser exposing (..)
import Datalog.Rule as Rule exposing (Rule)
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
                        (Ok (program [ Rule.fact (Atom.atom "greek" [ Term.string "Socrates" ]) ]))
                        (parse "greek(\"Socrates\").")
            , test "a fact with leading space" <|
                \_ ->
                    Expect.equal
                        (Ok (program [ Rule.fact (Atom.atom "greek" [ Term.string "Socrates" ]) ]))
                        (parse " greek(\"Socrates\").")
            , test "a fact with a number" <|
                \_ ->
                    Expect.equal
                        (Ok (program [ Rule.fact (Atom.atom "theAnswer" [ Term.int 42 ]) ]))
                        (parse "theAnswer(42).")
            , test "a rule with a variable" <|
                \_ ->
                    Expect.equal
                        (Ok
                            (program
                                [ Rule.rule
                                    (Atom.atom "mortal" [ Term.variable "whom" ])
                                    [ Atom.atom "greek" [ Term.variable "whom" ] ]
                                ]
                            )
                        )
                        (parse "mortal(whom) :- greek(whom).")
            , test "a rule with multiple clauses" <|
                \_ ->
                    Expect.equal
                        (Ok
                            (program
                                [ Rule.rule
                                    (Atom.atom "ancestor" [ Term.variable "Child", Term.variable "Ancestor" ])
                                    [ Atom.atom "parent" [ Term.variable "Child", Term.variable "Parent" ]
                                    , Atom.atom "ancestor" [ Term.variable "Parent", Term.variable "Ancestor" ]
                                    ]
                                ]
                            )
                        )
                        (parse "ancestor(Child, Ancestor) :- parent(Child, Parent), ancestor(Parent, Ancestor).")
            , test "a program with multiple rules" <|
                \_ ->
                    Expect.equal
                        (Ok
                            (program
                                [ Rule.fact (Atom.atom "greek" [ Term.string "Socrates" ])
                                , Rule.rule
                                    (Atom.atom "mortal" [ Term.variable "Whom" ])
                                    [ Atom.atom "greek" [ Term.variable "Whom" ] ]
                                ]
                            )
                        )
                        (parse "greek(\"Socrates\").\nmortal(Whom) :- greek(Whom).")
            , test "a program with whitespace between rules" <|
                \_ ->
                    Expect.equal
                        (Ok
                            (program
                                [ Rule.fact (Atom.atom "greek" [ Term.string "Socrates" ])
                                , Rule.rule
                                    (Atom.atom "mortal" [ Term.variable "Whom" ])
                                    [ Atom.atom "greek" [ Term.variable "Whom" ] ]
                                ]
                            )
                        )
                        (parse "greek(\"Socrates\").\n\nmortal(Whom) :- greek(Whom).")
            , test "a rule with newlines in between body atoms" <|
                \_ ->
                    Expect.equal
                        (Ok
                            (program
                                [ Rule.rule
                                    (Atom.atom "ancestor" [ Term.variable "Child", Term.variable "Ancestor" ])
                                    [ Atom.atom "parent" [ Term.variable "Child", Term.variable "Parent" ]
                                    , Atom.atom "ancestor" [ Term.variable "Parent", Term.variable "Ancestor" ]
                                    ]
                                ]
                            )
                        )
                        (parse "ancestor(Child, Ancestor) :-\n    parent(Child, Parent),\n    ancestor(Parent, Ancestor).")
            , test "a rule using anonymous variables" <|
                \_ ->
                    Expect.equal
                        (Ok
                            (program
                                [ Rule.rule
                                    (Atom.atom "iceCream" [ Term.variable "favoriteFlavor" ])
                                    [ Atom.atom "person" [ Term.anonymous, Term.variable "favoriteFlavor" ] ]
                                ]
                            )
                        )
                        (parse "iceCream(favoriteFlavor) :- person(_, favoriteFlavor).")
            ]
        , describe "failure"
            [ test "leaving the terms off an atom is not allowed" <|
                \_ -> Expect.err (parse "greek")
            , test "leaving the closing quote off a constant is not allowed" <|
                \_ -> Expect.err (parse "greek(\"Socrates")
            , test "leaving the closing parenthesis off a term list is not allowed" <|
                \_ -> Expect.err (parse "greek(\"Socrates\"")
            , test "leaving a period off a fact is not allowed" <|
                \_ -> Expect.err (parse "greek(\"Socrates\")")
            , test "adding a trailing comma in a term list is not allowed" <|
                \_ -> Expect.err (parse "greek(\"Socrates\",)")
            , test "having an implies horn but no body is not allowed" <|
                \_ -> Expect.err (parse "mortal(whom) :-")
            , test "having a trailing comma in a rule body is not allowed" <|
                \_ -> Expect.err (parse "ancestor(Child, Ancestor) :- parent(Child, Parent),")
            , test "leaving a period off a rule is not allowed" <|
                \_ -> Expect.err (parse "mortal(Whom) :- greek(Whom)")
            , test "entering a non-range-restricted rules is not allowed" <|
                \_ -> Expect.err (parse "mortal(unused) :- greek(\"Socrates\").")
            ]
        ]


program : List (Result x Rule) -> Datalog.Program
program =
    List.filterMap
        (\res ->
            case res of
                Ok cool ->
                    Just cool

                Err err ->
                    Debug.todo (Debug.toString err)
        )
        >> Program
