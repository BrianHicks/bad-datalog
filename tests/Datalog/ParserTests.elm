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
        [ test "a fact is parseable" <|
            \_ ->
                Expect.equal
                    (Ok (Program [ Datalog.Rule (Atom.Atom "man" [ Term.Constant "Socrates" ]) [] ]))
                    (parse "man(\"Socrates\")")
        , test "leaving the terms off an atom is not allowed" <|
            \_ ->
                Expect.equal
                    (Err
                        [ { row = 1
                          , col = 4
                          , contextStack =
                                [ { row = 1, col = 4, context = AtomTerms }
                                , { row = 1, col = 4, context = Atom (Just "man") }
                                , { row = 1, col = 1, context = RuleHead }
                                , { row = 1, col = 1, context = Rule }
                                ]
                          , problem = ExpectingOpeningParenthesis
                          }
                        ]
                    )
                    (parse "man")
        , test "leaving the closing quote off a constant is not allowed" <|
            \_ ->
                Expect.equal
                    (Err
                        [ { row = 1
                          , col = 14
                          , contextStack =
                                [ { row = 1, col = 5, context = Constant }
                                , { row = 1, col = 5, context = Term }
                                , { row = 1, col = 4, context = AtomTerms }
                                , { row = 1, col = 4, context = Atom (Just "man") }
                                , { row = 1, col = 1, context = RuleHead }
                                , { row = 1, col = 1, context = Rule }
                                ]
                          , problem = ExpectingClosingQuote
                          }
                        ]
                    )
                    (parse "man(\"Socrates")
        , test "leaving the closing parenthesis off a term list is not allowed" <|
            \_ ->
                Expect.equal
                    (Err
                        [ { row = 1
                          , col = 15
                          , contextStack =
                                [ { row = 1, col = 4, context = AtomTerms }
                                , { row = 1, col = 4, context = Atom (Just "man") }
                                , { row = 1, col = 1, context = RuleHead }
                                , { row = 1, col = 1, context = Rule }
                                ]
                          , problem = ExpectingComma
                          }
                        , { row = 1
                          , col = 15
                          , contextStack =
                                [ { row = 1, col = 4, context = AtomTerms }
                                , { row = 1, col = 4, context = Atom (Just "man") }
                                , { row = 1, col = 1, context = RuleHead }
                                , { row = 1, col = 1, context = Rule }
                                ]
                          , problem = ExpectingClosingParenthesis
                          }
                        ]
                    )
                    (parse "man(\"Socrates\"")
        , test "adding a trailing comma in a term list is not allowed" <|
            \_ ->
                Expect.equal
                    (Err
                        [ { row = 1
                          , col = 16
                          , contextStack =
                                [ { row = 1, col = 16, context = Constant }
                                , { row = 1, col = 16, context = Term }
                                , { row = 1, col = 4, context = AtomTerms }
                                , { row = 1, col = 4, context = Atom (Just "man") }
                                , { row = 1, col = 1, context = RuleHead }
                                , { row = 1, col = 1, context = Rule }
                                ]
                          , problem = ExpectingOpeningQuote
                          }
                        , { row = 1
                          , col = 16
                          , contextStack =
                                [ { row = 1, col = 16, context = Variable }
                                , { row = 1, col = 16, context = Term }
                                , { row = 1, col = 4, context = AtomTerms }
                                , { row = 1, col = 4, context = Atom (Just "man") }
                                , { row = 1, col = 1, context = RuleHead }
                                , { row = 1, col = 1, context = Rule }
                                ]
                          , problem = ExpectingVariable
                          }
                        ]
                    )
                    (parse "man(\"Socrates\",)")
        , test "a rule with a variable is parseable" <|
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
        , test "having an implies horn but no body is not allowed" <|
            \_ ->
                Expect.equal
                    (Err
                        [ { row = 1
                          , col = 16
                          , contextStack =
                                [ { row = 1, col = 16, context = Atom Nothing }
                                , { row = 1, col = 13, context = RuleBody }
                                , { row = 1, col = 1, context = Rule }
                                ]
                          , problem = ExpectingAtomName
                          }
                        ]
                    )
                    (parse "mortal(whom) :-")
        ]
