module Datalog.Parser exposing (Context(..), Problem(..), parse)

import Datalog exposing (Program(..), Rule)
import Datalog.Atom as Atom exposing (Atom)
import Datalog.Term as Term exposing (Term)
import Parser.Advanced as Parser exposing ((|.), (|=), Parser)
import Set


parse : String -> Result (List (Parser.DeadEnd Context Problem)) Datalog.Program
parse source =
    Parser.run parser source


type Context
    = Rule
    | Atom (Maybe String)
    | AtomTerms
    | Term
    | Constant


type Problem
    = ExpectingAtomName
    | ExpectingOpeningParenthesis
    | ExpectingClosingParenthesis
    | ExpectingComma
    | ExpectingOpeningQuote
    | ExpectingClosingQuote


parser : Parser Context Problem Program
parser =
    rule |> Parser.map (Program << List.singleton)


rule : Parser Context Problem Rule
rule =
    Parser.map (\head -> Datalog.Rule head []) atom
        |> Parser.inContext Rule


atom : Parser Context Problem Atom
atom =
    let
        atomName =
            Parser.variable
                { start = Char.isLower
                , inner = Char.isAlphaNum
                , reserved = Set.empty
                , expecting = ExpectingAtomName
                }

        atomTerms =
            Parser.sequence
                { start = Parser.Token "(" ExpectingOpeningParenthesis
                , separator = Parser.Token "," ExpectingComma
                , end = Parser.Token ")" ExpectingClosingParenthesis
                , spaces = spaces
                , item = term
                , trailing = Parser.Forbidden
                }
    in
    Parser.inContext (Atom Nothing) atomName
        |> Parser.andThen
            (\name ->
                Parser.inContext (Atom (Just name)) <|
                    Parser.succeed Atom.Atom
                        |= Parser.succeed name
                        |. spaces
                        |= Parser.inContext AtomTerms atomTerms
            )


term : Parser Context Problem Term
term =
    constant
        |> Parser.inContext Term


constant : Parser Context Problem Term
constant =
    Parser.inContext Constant <|
        Parser.succeed Term.Constant
            |. Parser.token (Parser.Token "\"" ExpectingOpeningQuote)
            |= Parser.getChompedString (Parser.chompWhile (\c -> c /= '"'))
            |. Parser.token (Parser.Token "\"" ExpectingClosingQuote)


spaces : Parser Context Problem ()
spaces =
    Parser.chompWhile (\c -> c == ' ')
