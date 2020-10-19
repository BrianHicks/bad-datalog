module Datalog.Parser exposing (parse)

import Datalog exposing (Program(..))
import Datalog.Atom as Atom exposing (Atom)
import Datalog.Rule as Rule exposing (Rule)
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
        |> List.foldr
            (\deadEnd groups ->
                Dict.update
                    ( deadEndSpan deadEnd, niceContextStack deadEnd.contextStack )
                    (\maybeExisting ->
                        case maybeExisting of
                            Just ( first, rest ) ->
                                Just ( deadEnd, first.problem :: rest )

                            Nothing ->
                                Just ( deadEnd, [] )
                    )
                    groups
            )
            Dict.empty
        |> Dict.values
        |> List.map (niceError source)


deadEndSpan : Parser.DeadEnd Context Problem -> ( ( Int, Int ), ( Int, Int ) )
deadEndSpan deadEnd =
    let
        locations =
            ( deadEnd.row, deadEnd.col )
                :: List.map (\{ row, col } -> ( row, col )) deadEnd.contextStack
    in
    Maybe.map2 Tuple.pair
        (List.minimum locations)
        (List.maximum locations)
        |> Maybe.withDefault ( ( 0, 0 ), ( 0, 0 ) )


niceContextStack : List { otherStuff | context : Context } -> String
niceContextStack =
    List.foldr
        (\before after ->
            niceContext before.context
                ++ (if after == "" then
                        after

                    else
                        " inside " ++ after
                   )
        )
        ""


niceError : String -> ( Parser.DeadEnd Context Problem, List Problem ) -> String
niceError source ( deadEnd, moreProblems ) =
    let
        ( ( startRow, _ ), ( endRow, problemCol ) ) =
            deadEndSpan deadEnd

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

        problems =
            case deadEnd.problem :: moreProblems of
                [] ->
                    "This shouldn't be possible, and indicates a bug. Ask for help!"

                [ only ] ->
                    "I was expecting " ++ niceProblem only

                many ->
                    many
                        |> List.map (\problem -> " - " ++ niceProblem problem)
                        |> String.join "\n"
                        |> (++) "I was expecting one of these things:\n\n"
    in
    "I ran into a problem while parsing "
        ++ niceContextStack deadEnd.contextStack
        ++ " at line "
        ++ String.fromInt endRow
        ++ ", column "
        ++ String.fromInt problemCol
        ++ ":\n\n"
        ++ lines
        ++ "\n"
        ++ pointer
        ++ "\n\n"
        ++ problems


type Context
    = Rule
    | Atom (Maybe String)


niceContext : Context -> String
niceContext context =
    case context of
        Rule ->
            "a rule"

        Atom Nothing ->
            "an atom"

        Atom (Just name) ->
            "an atom named " ++ name


type Problem
    = ExpectingAtomName
    | ExpectingStartOfTerms
    | ExpectingEndOfTerms
    | ExpectingComma
    | ExpectingOpeningQuote
    | ExpectingClosingQuote
    | ExpectingNumber
    | InvalidNumber
    | ExpectingVariable
    | ExpectingImplies
    | ExpectingNewline
    | ExpectingEnd
    | ExpectingPeriod
    | InvalidRule Rule.Problem


niceProblem : Problem -> String
niceProblem problem =
    case problem of
        ExpectingAtomName ->
            "an atom name"

        ExpectingStartOfTerms ->
            "an opening parenthesis to start the list of terms in the atom"

        ExpectingEndOfTerms ->
            "a closing parenthesis to end the list of terms in the atom"

        ExpectingComma ->
            "a comma"

        ExpectingOpeningQuote ->
            "a quote (`\"`) to start a constant term"

        ExpectingClosingQuote ->
            "a closing quote (`\"`) to end this constant term"

        ExpectingNumber ->
            "a number for an integer term"

        InvalidNumber ->
            "an integer like 1234 (no floats, hex, octal, etc.)"

        ExpectingVariable ->
            "a variable (a name starting with a letter and continuing on with alphanumeric characters)"

        ExpectingImplies ->
            "a `:-` followed by a rule body"

        ExpectingNewline ->
            "a newline"

        ExpectingEnd ->
            "the end of the program"

        ExpectingPeriod ->
            "a period to end a rule"

        InvalidRule Rule.NotRangeRestricted ->
            "a rule, but it must use all the variables from the head in the body"


parser : Parser Context Problem Program
parser =
    Parser.succeed Program
        |. spaces
        |= Parser.loop [] rules


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
    (Parser.succeed Rule.rule
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
    )
        |> Parser.andThen
            (\ruleResult ->
                case ruleResult of
                    Ok validRule ->
                        Parser.succeed validRule

                    Err err ->
                        Parser.problem (InvalidRule err)
            )
        |> Parser.inContext Rule


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
                    Parser.succeed Atom.atom
                        |= Parser.succeed name
                        |. spaces
                        |= atomTerms
            )


term : Parser Context Problem Term
term =
    Parser.oneOf [ variable, string, int ]


string : Parser Context Problem Term
string =
    Parser.succeed (Term.Constant << Term.String)
        |. Parser.token (Parser.Token "\"" ExpectingOpeningQuote)
        |= Parser.getChompedString (Parser.chompWhile (\c -> c /= '"'))
        |. Parser.token (Parser.Token "\"" ExpectingClosingQuote)


int : Parser Context Problem Term
int =
    Parser.int ExpectingNumber InvalidNumber
        |> Parser.map (Term.Constant << Term.Int)


variable : Parser Context Problem Term
variable =
    Parser.variable
        { start = Char.isAlpha
        , inner = Char.isAlphaNum
        , reserved = Set.empty
        , expecting = ExpectingVariable
        }
        |> Parser.map (Term.Variable << Term.Named)


spaces : Parser Context Problem ()
spaces =
    Parser.chompWhile (\c -> c == ' ' || c == '\n')
