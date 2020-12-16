module Datalog.ParserTests exposing (..)

import Datalog exposing (Program, program)
import Datalog.Atom as Atom
import Datalog.Negatable exposing (negative, positive)
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
                        (Ok (unsafeProgram [ Rule.fact (Atom.atom "greek" [ Term.string "Socrates" ]) ]))
                        (parse "greek(\"Socrates\").")
            , test "a fact with leading space" <|
                \_ ->
                    Expect.equal
                        (Ok (unsafeProgram [ Rule.fact (Atom.atom "greek" [ Term.string "Socrates" ]) ]))
                        (parse " greek(\"Socrates\").")
            , test "a fact with a number" <|
                \_ ->
                    Expect.equal
                        (Ok (unsafeProgram [ Rule.fact (Atom.atom "theAnswer" [ Term.int 42 ]) ]))
                        (parse "theAnswer(42).")
            , test "a rule with a variable" <|
                \_ ->
                    Expect.equal
                        (Ok
                            (unsafeProgram
                                [ Rule.rule
                                    (Atom.atom "mortal" [ Term.variable "whom" ])
                                    [ positive (Atom.atom "greek" [ Term.variable "whom" ]) ]
                                ]
                            )
                        )
                        (parse "mortal(whom) :- greek(whom).")
            , test "a rule with multiple clauses" <|
                \_ ->
                    Expect.equal
                        (Ok
                            (unsafeProgram
                                [ Rule.rule
                                    (Atom.atom "ancestor" [ Term.variable "Child", Term.variable "Ancestor" ])
                                    [ positive (Atom.atom "parent" [ Term.variable "Child", Term.variable "Parent" ])
                                    , positive (Atom.atom "ancestor" [ Term.variable "Parent", Term.variable "Ancestor" ])
                                    ]
                                ]
                            )
                        )
                        (parse "ancestor(Child, Ancestor) :- parent(Child, Parent), ancestor(Parent, Ancestor).")
            , test "a program with multiple rules" <|
                \_ ->
                    Expect.equal
                        (Ok
                            (unsafeProgram
                                [ Rule.fact (Atom.atom "greek" [ Term.string "Socrates" ])
                                , Rule.rule
                                    (Atom.atom "mortal" [ Term.variable "Whom" ])
                                    [ positive (Atom.atom "greek" [ Term.variable "Whom" ]) ]
                                ]
                            )
                        )
                        (parse "greek(\"Socrates\").\nmortal(Whom) :- greek(Whom).")
            , test "a program with whitespace between rules" <|
                \_ ->
                    Expect.equal
                        (Ok
                            (unsafeProgram
                                [ Rule.fact (Atom.atom "greek" [ Term.string "Socrates" ])
                                , Rule.rule
                                    (Atom.atom "mortal" [ Term.variable "Whom" ])
                                    [ positive (Atom.atom "greek" [ Term.variable "Whom" ]) ]
                                ]
                            )
                        )
                        (parse "greek(\"Socrates\").\n\nmortal(Whom) :- greek(Whom).")
            , test "a rule with newlines in between body atoms" <|
                \_ ->
                    Expect.equal
                        (Ok
                            (unsafeProgram
                                [ Rule.rule
                                    (Atom.atom "ancestor" [ Term.variable "Child", Term.variable "Ancestor" ])
                                    [ positive (Atom.atom "parent" [ Term.variable "Child", Term.variable "Parent" ])
                                    , positive (Atom.atom "ancestor" [ Term.variable "Parent", Term.variable "Ancestor" ])
                                    ]
                                ]
                            )
                        )
                        (parse "ancestor(Child, Ancestor) :-\n    parent(Child, Parent),\n    ancestor(Parent, Ancestor).")
            , test "a rule using anonymous variables" <|
                \_ ->
                    Expect.equal
                        (Ok
                            (unsafeProgram
                                [ Rule.rule
                                    (Atom.atom "iceCream" [ Term.variable "favoriteFlavor" ])
                                    [ positive (Atom.atom "person" [ Term.anonymous, Term.variable "favoriteFlavor" ]) ]
                                ]
                            )
                        )
                        (parse "iceCream(favoriteFlavor) :- person(_, favoriteFlavor).")
            , test "a rule using negation in a body atom" <|
                \_ ->
                    Expect.equal
                        (Ok
                            (unsafeProgram
                                [ Rule.rule
                                    (Atom.atom "ancestor" [ Term.variable "Child", Term.variable "Parent" ])
                                    [ positive (Atom.atom "parent" [ Term.variable "Child", Term.variable "Parent" ])
                                    , negative (Atom.atom "samePerson" [ Term.variable "Child", Term.variable "Parent" ])
                                    ]
                                ]
                            )
                        )
                        (parse "ancestor(Child, Parent) :- parent(Child, Parent), not samePerson(Child, Parent).")
            , test "a line comment on a separate line" <|
                \_ -> Expect.ok (parse "-- this is a comment")
            , test "a line comment after a fact" <|
                \_ -> Expect.ok (parse "greek(\"Socrates\"). -- lol classic Socrates")
            , test "a line comment ater a rule" <|
                \_ -> Expect.ok (parse "mortal(thing) :- greek(thing). -- wow, profound")
            , test "a line comment between the body atoms in a rule" <|
                \_ -> Expect.ok (parse "mortal(thing) :- -- just hanging out you know?\n  greek(thing).")
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


unsafeProgram : List (Result x Rule) -> Datalog.Program
unsafeProgram questionableRules =
    let
        rulesOrCrash =
            List.filterMap
                (\res ->
                    case res of
                        Ok cool ->
                            Just cool

                        Err err ->
                            Debug.todo (Debug.toString err)
                )
                questionableRules
    in
    case program rulesOrCrash of
        Ok program_ ->
            program_

        Err err ->
            Debug.todo (Debug.toString err)
