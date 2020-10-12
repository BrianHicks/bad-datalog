module Datalog.Parser exposing (parse)

import Datalog exposing (Program(..), Rule)
import Datalog.Atom as Atom exposing (Atom)
import Datalog.Term as Term exposing (Term)
import Dict
import Parser.Advanced as Parser exposing ((|.), (|=), Parser)
import Set


parse : String -> Result (List String) Datalog.Program
parse source =
    source
        |> Parser.run parser
        |> Result.mapError (niceErrors source)


niceErrors : String -> List (Parser.DeadEnd Context Problem) -> List String
niceErrors source deadEnds =
    deadEnds
        |> List.map (niceError source)


niceError : String -> Parser.DeadEnd Context Problem -> String
niceError source deadEnd =
    let
        locations =
            ( deadEnd.row, deadEnd.col ) :: List.map (\{ row, col } -> ( row, col )) deadEnd.contextStack

        context =
            List.head deadEnd.contextStack
                |> Maybe.map (.context >> niceContext)
                |> Maybe.withDefault "the program"
    in
    case Maybe.map2 Tuple.pair (List.minimum locations) (List.maximum locations) of
        Just ( ( startRow, _ ), ( endRow, problemCol ) ) ->
            let
                lines =
                    String.lines source
                        |> List.drop (startRow - 1)
                        |> List.take (endRow - startRow + 1)
                        |> String.join "\n"

                pointer =
                    List.concat
                        [ List.repeat (problemCol - 1) ' '
                        , [ '^' ]
                        ]
                        |> String.fromList
            in
            "I ran into a problem while parsing "
                ++ context
                ++ " at line "
                ++ String.fromInt endRow
                ++ ", column "
                ++ String.fromInt problemCol
                ++ ":\n\n"
                ++ lines
                ++ "\n"
                ++ pointer
                ++ "\n\n"
                ++ niceProblem deadEnd.problem

        Nothing ->
            "Couldn't find source location, sorry. The problem was: " ++ niceProblem deadEnd.problem


type Context
    = Rule
    | Atom (Maybe String)
    | ConstantTerm
    | VariableTerm


niceContext : Context -> String
niceContext context =
    case context of
        Rule ->
            "a rule"

        Atom Nothing ->
            "an atom"

        Atom (Just name) ->
            "an atom named " ++ name

        ConstantTerm ->
            "a constant term"

        VariableTerm ->
            "a variable term"


type Problem
    = ExpectingAtomName
    | ExpectingStartOfTerms
    | ExpectingEndOfTerms
    | ExpectingComma
    | ExpectingOpeningQuote
    | ExpectingClosingQuote
    | ExpectingVariable
    | ExpectingImplies
    | ExpectingNewline
    | ExpectingEnd
    | ExpectingPeriod


niceProblem : Problem -> String
niceProblem problem =
    case problem of
        ExpectingAtomName ->
            "expecting an atom name"

        ExpectingStartOfTerms ->
            "expecting an opening parenthesis to start the list of terms in the atom"

        ExpectingEndOfTerms ->
            "expecting a closing parenthesis to end the list of terms in the atom"

        ExpectingComma ->
            "expecting a comma"

        ExpectingOpeningQuote ->
            "expecting a quote (`\"`) to start a constant term"

        ExpectingClosingQuote ->
            "expecting a closing quote (`\"`) to end this constant term"

        ExpectingVariable ->
            "expecting a variable (a name starting with a letter and continuing on with alphanumeric characters)"

        ExpectingImplies ->
            "expecting a `:-` followed by a rule body"

        ExpectingNewline ->
            "expecting a newline"

        ExpectingEnd ->
            "expecting the end of the program"

        ExpectingPeriod ->
            "expecting a period to end a rule"


parser : Parser Context Problem Program
parser =
    Parser.map Program (Parser.loop [] rules)


rules : List Rule -> Parser Context Problem (Parser.Step (List Rule) (List Rule))
rules soFar =
    Parser.oneOf
        [ Parser.succeed (\rule_ -> Parser.Loop (rule_ :: soFar))
            |= rule
            |. spaces
        , Parser.end ExpectingEnd
            |> Parser.map (\() -> Parser.Done (List.reverse soFar))
        ]


rule : Parser Context Problem Rule
rule =
    Parser.inContext Rule <|
        Parser.succeed Datalog.Rule
            |= atom
            |= Parser.oneOf
                [ Parser.succeed (::)
                    |. spaces
                    |. Parser.token (Parser.Token ":-" ExpectingImplies)
                    |. spaces
                    |= atom
                    |= Parser.loop [] ruleTail
                , Parser.succeed []
                    |. Parser.token (Parser.Token "." ExpectingPeriod)
                ]


ruleTail : List Atom -> Parser Context Problem (Parser.Step (List Atom) (List Atom))
ruleTail soFar =
    Parser.oneOf
        [ Parser.succeed (\atom_ -> Parser.Loop (atom_ :: soFar))
            |. spaces
            |. Parser.token (Parser.Token "," ExpectingComma)
            |. spaces
            |= atom
        , Parser.map (\() -> Parser.Done (List.reverse soFar))
            (Parser.token (Parser.Token "." ExpectingPeriod))
        ]


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
                { start = Parser.Token "(" ExpectingStartOfTerms
                , separator = Parser.Token "," ExpectingComma
                , end = Parser.Token ")" ExpectingEndOfTerms
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
                        |= atomTerms
            )


term : Parser Context Problem Term
term =
    Parser.oneOf [ constant, variable ]


constant : Parser Context Problem Term
constant =
    Parser.inContext ConstantTerm <|
        Parser.succeed Term.Constant
            |. Parser.token (Parser.Token "\"" ExpectingOpeningQuote)
            |= Parser.getChompedString (Parser.chompWhile (\c -> c /= '"'))
            |. Parser.token (Parser.Token "\"" ExpectingClosingQuote)


variable : Parser Context Problem Term
variable =
    Parser.variable
        { start = Char.isAlpha
        , inner = Char.isAlphaNum
        , reserved = Set.empty
        , expecting = ExpectingVariable
        }
        |> Parser.map Term.Variable
        |> Parser.inContext VariableTerm


spaces : Parser Context Problem ()
spaces =
    Parser.chompWhile (\c -> c == ' ' || c == '\n')
