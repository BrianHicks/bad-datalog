module Datalog.Parser exposing (fromString)

import Datalog
import Parser exposing ((|.), (|=), Parser)
import Set


fromString : String -> Result (List Parser.DeadEnd) Datalog.Program
fromString input =
    Parser.run parser input


parser : Parser Datalog.Program
parser =
    Parser.loop [] rules


rules : Datalog.Program -> Parser (Parser.Step Datalog.Program Datalog.Program)
rules state =
    Parser.oneOf
        [ Parser.succeed (\rule_ -> Parser.Loop (rule_ :: state))
            |= rule
        , Parser.succeed (\_ -> Parser.Done (List.reverse state))
            |= Parser.end
        ]


rule : Parser Datalog.Rule
rule =
    Parser.succeed Datalog.Rule
        |= atom
        |= Parser.succeed []
        |. Parser.token "."


atom : Parser Datalog.Atom
atom =
    Parser.succeed Datalog.Atom
        |= Parser.variable
            { start = Char.isLower
            , inner = \c -> Char.isAlphaNum c || c == '_'
            , reserved = Set.empty
            }
        |= Parser.sequence
            { start = "("
            , end = ")"
            , separator = ","
            , spaces = Parser.spaces
            , item = term
            , trailing = Parser.Forbidden
            }


term : Parser Datalog.Term
term =
    Parser.oneOf
        [ Parser.map Datalog.Var
            (Parser.variable
                { start = Char.isUpper
                , inner = \c -> Char.isAlphaNum c || c == '_'
                , reserved = Set.empty
                }
            )
        , Parser.succeed Datalog.Sym
            |. Parser.token "\""
            |= Parser.getChompedString (Parser.chompWhile (\c -> c /= '"'))
            |. Parser.token "\""
        ]
