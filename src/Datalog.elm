module Datalog exposing
    ( Database, empty, register, Problem(..), insert
    , read, readOne, Decoder, DecodingProblem(..), into, stringField, intField
    , derive, Rule, rule, with, without, filter, planRule
    , Filter, eq, gt, lt, not_, or
    , Term, var, int, string
    , parse
    )

{-|

@docs Database, empty, register, Problem, insert

@docs read, readOne, Decoder, DecodingProblem, into, stringField, intField

@docs derive, Rule, rule, with, without, filter, planRule

@docs Filter, eq, gt, lt, not_, or

@docs Term, var, int, string

-}

import Array exposing (Array)
import Datalog.Database as Database exposing (Constant)
import Dict
import Graph exposing (Edge, Graph, Node)
import List.Extra exposing (foldrResult, indexOf)
import Murmur3
import Parser.Advanced as Parser exposing ((|.), (|=))
import Set exposing (Set)


type Database
    = Database Database.Database


empty : Database
empty =
    Database Database.empty


type Problem
    = NeedAtLeastOnePositiveAtom
    | NeedAtLeastOneName
    | VariableDoesNotAppearInBody String
    | VariableMustAppearInPositiveAtom String
    | CannotInsertVariable String
    | CannotHaveNegationInRecursiveQuery
    | ExpectedExactlyOneRow Int
    | DatabaseProblem Database.Problem
    | DecodingProblem DecodingProblem
    | ParsingProblem (List (Parser.DeadEnd Context ParsingProblem))


type DecodingProblem
    = FieldNotFound Int
    | UnexpectedFieldType Database.FieldType


insert : String -> List Term -> Database -> Result Problem Database
insert name body (Database db) =
    body
        |> foldrResult
            (\term soFar ->
                case term of
                    Constant constant ->
                        Ok (constant :: soFar)

                    Variable name_ ->
                        Err (CannotInsertVariable name_)
            )
            []
        |> Result.andThen
            (\constants ->
                Database.insert name constants db
                    |> Result.mapError DatabaseProblem
            )
        |> Result.map Database


{-| When displaying data, you'll often want to provide an "empty" view of
the data before you'e inserted anything into the database. This allows you
to do that by letting the database know there will be a table with the given
name _eventually_.
-}
register : String -> List Database.FieldType -> Database -> Result Problem Database
register name schema (Database db) =
    case Database.register name schema db of
        Ok newDb ->
            Ok (Database newDb)

        Err problem ->
            Err (DatabaseProblem problem)


derive : List Rule -> Database -> Result Problem Database
derive rules (Database db) =
    let
        nodes : Result Problem (List (Node ( String, Maybe Database.QueryPlan )))
        nodes =
            rules
                |> foldrResult
                    (\((Rule (Atom name _) _) as rule_) soFar ->
                        case planRule rule_ of
                            Ok plan ->
                                soFar
                                    |> Dict.insert
                                        (Murmur3.hashString 0 name)
                                        ( name, Nothing )
                                    |> Dict.insert
                                        (Murmur3.hashString 0 (ruleToString rule_))
                                        ( name, Just plan )
                                    |> Ok

                            Err problem ->
                                Err problem
                    )
                    Dict.empty
                |> Result.map
                    (Dict.foldr
                        (\id maybePlan soFar ->
                            Node id maybePlan :: soFar
                        )
                        []
                    )

        edges : List (Edge Negation)
        edges =
            List.concatMap
                (\((Rule (Atom headName _) bodyAtoms) as rule_) ->
                    Edge
                        (Murmur3.hashString 0 headName)
                        (Murmur3.hashString 0 (ruleToString rule_))
                        Positive
                        :: List.filterMap
                            (\bodyAtom ->
                                case bodyAtom of
                                    BodyAtom negation (Atom bodyName _) ->
                                        Just
                                            (Edge
                                                (Murmur3.hashString 0 (ruleToString rule_))
                                                (Murmur3.hashString 0 bodyName)
                                                negation
                                            )

                                    Filter _ ->
                                        -- filters don't actually create
                                        -- dependencies between atoms; they only
                                        -- filter on names that have already been
                                        -- bound from those dependencies. So we're
                                        -- good to just drop them at this stage.
                                        Nothing
                            )
                            bodyAtoms
                )
                rules

        strataResult : Result Problem (List (Graph ( String, Maybe Database.QueryPlan ) Negation))
        strataResult =
            Result.andThen
                (\nodes_ ->
                    let
                        graph : Graph ( String, Maybe Database.QueryPlan ) Negation
                        graph =
                            Graph.fromNodesAndEdges nodes_ edges
                    in
                    case Graph.stronglyConnectedComponents graph of
                        Ok _ ->
                            Ok [ graph ]

                        Err strata ->
                            foldrResult
                                (\stratum soFar ->
                                    if List.any (\{ label } -> label == Negative) (Graph.edges stratum) then
                                        Err CannotHaveNegationInRecursiveQuery

                                    else
                                        Ok (stratum :: soFar)
                                )
                                []
                                strata
                )
                nodes
    in
    strataResult
        |> Result.andThen
            (\strata ->
                foldrResult
                    deriveUntilExhausted
                    db
                    strata
            )
        |> Result.map Database


deriveUntilExhausted :
    Graph ( String, Maybe Database.QueryPlan ) Negation
    -> Database.Database
    -> Result Problem Database.Database
deriveUntilExhausted stratum db =
    deriveUntilExhaustedHelp stratum db db


deriveUntilExhaustedHelp :
    Graph ( String, Maybe Database.QueryPlan ) Negation
    -> Database.Database
    -> Database.Database
    -> Result Problem Database.Database
deriveUntilExhaustedHelp stratum db finalDb =
    -- the goal of semi-naive evaluation is to only read "new" tuples on each
    -- iteration towards exhaustion (which is what I'm calling the state of
    -- having found all the tuples.) This helps with performance: instead of
    -- having to do joins over the entire data set plus new tuples, you only
    -- have to do new tuples. This is safe because you've already evaluated the
    -- "old" tuples in previous iterations.
    --
    -- The mental model here is that we're keeping track of a stack of relations
    -- for each name instead of merging them all immediately. In practice,
    -- we actually only need to keep track of a "new" database and the final
    -- database. Once we don't get any new tuples in the database, we can
    -- quit looping.
    let
        iterationResult : Result Problem ( Database.Database, Database.Database )
        iterationResult =
            Graph.nodes stratum
                |> List.filterMap
                    (\{ label } ->
                        case label of
                            ( name, Just plan ) ->
                                Just ( name, plan )

                            ( _, Nothing ) ->
                                Nothing
                    )
                |> foldrResult
                    (\( name, plan ) ( dbSoFar, finalDbSoFar ) ->
                        let
                            -- We only want to get the new rows in order to avoid
                            -- recomputing previous tuples, so we want an outer
                            -- join on the existing rows! But, the relation
                            -- we're joining on might not be in the database
                            -- yet. So we try to look up the relation first
                            -- (which is pretty quick.) If it exists, we know
                            -- that our join is at least feasible, but if the
                            -- lookup fails for any reason we'd better not try it!
                            finalPlan : Database.QueryPlan
                            finalPlan =
                                case Database.read name dbSoFar of
                                    Ok _ ->
                                        Database.OuterJoin
                                            { keep = plan
                                            , drop = Database.Read name
                                            }

                                    Err _ ->
                                        plan
                        in
                        dbSoFar
                            |> Database.query finalPlan
                            |> Result.andThen
                                (\relation ->
                                    Result.map
                                        (\merged ->
                                            ( Database.replaceRelation name relation dbSoFar
                                            , merged
                                            )
                                        )
                                        (Database.mergeRelations name relation finalDbSoFar)
                                )
                            |> Result.mapError DatabaseProblem
                    )
                    ( db, finalDb )
    in
    case iterationResult of
        Ok ( nextDb, newFinalDb ) ->
            if newFinalDb == finalDb then
                Ok newFinalDb

            else
                deriveUntilExhaustedHelp stratum nextDb newFinalDb

        Err problem ->
            Err problem


type Decoder a
    = Decoder (Array Database.Constant -> Result DecodingProblem a)


into : (a -> b) -> Decoder (a -> b)
into fn =
    Decoder (\_ -> Ok fn)


stringField : Int -> Decoder (String -> a) -> Decoder a
stringField index (Decoder fn) =
    Decoder <|
        \row ->
            Result.andThen
                (\nextFn ->
                    Result.andThen
                        (\constant ->
                            case constant of
                                Database.String string_ ->
                                    Ok (nextFn string_)

                                Database.Int _ ->
                                    Err (UnexpectedFieldType Database.IntType)
                        )
                        (at index row)
                )
                (fn row)


intField : Int -> Decoder (Int -> a) -> Decoder a
intField index (Decoder fn) =
    Decoder <|
        \row ->
            Result.andThen
                (\nextFn ->
                    Result.andThen
                        (\constant ->
                            case constant of
                                Database.String _ ->
                                    Err (UnexpectedFieldType Database.StringType)

                                Database.Int int_ ->
                                    Ok (nextFn int_)
                        )
                        (at index row)
                )
                (fn row)


at : Int -> Array Database.Constant -> Result DecodingProblem Database.Constant
at index row =
    case Array.get index row of
        Just constant ->
            Ok constant

        Nothing ->
            Err (FieldNotFound index)


read : String -> Decoder a -> Database -> Result Problem (List a)
read name (Decoder decode) (Database db) =
    Database.read name db
        |> Result.mapError DatabaseProblem
        |> Result.map Database.rows
        |> Result.andThen
            (foldrResult
                (\row soFar ->
                    case decode row of
                        Ok decoded ->
                            Ok (decoded :: soFar)

                        Err problem ->
                            Err (DecodingProblem problem)
                )
                []
            )


readOne : String -> Decoder a -> Database -> Result Problem a
readOne name decoder database =
    Result.andThen
        (\rows ->
            case rows of
                [ only ] ->
                    Ok only

                _ ->
                    Err (ExpectedExactlyOneRow (List.length rows))
        )
        (read name decoder database)


type Rule
    = Rule Atom (List BodyAtom)


{-| Start making a new rule! You'll need to name it (the first argument)
and then name the fields you'll end up exporting.

Some rules to keep in mind:

  - You have to provide at least one name.
  - You have to bind every name you define using `with`.

If you have multiple rules with the same name, they'll be merged together
(for an example, see the docs for [`with`](#with).)

-}
rule : String -> List String -> Rule
rule name headVars =
    Rule (Atom name (List.map Variable headVars)) []


{-| Add matches from the given name (TODO: table? rule? named tuple store?)

For example, if you have some greeks (Socrates, say) you can write a rule
like this to see which of them are mortal:

    rule "mortal" [ "name" ]
        |> with "greek" [ var "name" ]

It's fine to use this to set up recursive queries. For example, you could
compute reachability for all nodes in a graph using two rules like this:

    [ rule "reachable" [ "a", "b" ]
        |> with "link" [ var "a", var "b" ]
    , rule "reachable" [ "a", "c" ]
        |> with "link" [ var "a", var "b" ]
        |> with "reachable" [ var "b", var "c" ]
    ]

If you introduce a variable in a `with` like that above, it's also fine!

-}
with : String -> List Term -> Rule -> Rule
with name terms (Rule head body) =
    Rule head (BodyAtom Positive (Atom name terms) :: body)


{-| The opposite of [`with`](#with): remove any matching tuples based on
these names.

This has a few more rules than `with`, though:

  - You can't introduce new names in a `without` (every name must be used
    in a positive clause. If you could, we wouldn't have a way to know which
    values are permissible and we'd have to invent stuff; a big no-no!)
  - You can't use `without` recursively (if you could, you could get
    inconsistent outcomes based on which rules you evaluate first.)

If you've used another datalog implementation before: this is just negation,
and the rules are more-or-less the same.

Here's an example of computing all the nodes in a graph that _aren't_
reachable from each other:

    [ -- first, define `reachable` as in the example in `with`:
      rule "reachable" [ "a", "b" ]
        |> with "link" [ var "a", var "b" ]
    , rule "reachable" [ "a", "c" ]
        |> with "link" [ var "a", var "b" ]
        |> with "reachable" [ var "b", var "c" ]

    -- next, we need to know what is a valid node so we can
    , rule "node" [ "a" ]
        |> with "link" [ var "a", var "b" ]
    , rule "node" [ "b" ]
        |> with "link" [ var "a", var "b" ]

    -- finally, we just say "a set of two nodes is unreachable if they're
    -- individually in `node` but not together in `reachable`"
    , rule "unreachable" [ "a", "b" ]
        |> with "node" [ var "a" ]
        |> with "node" [ var "b" ]
        |> without "reachable" [ var "a", var "b" ]
    ]

-}
without : String -> List Term -> Rule -> Rule
without name terms (Rule head body) =
    Rule head (BodyAtom Negative (Atom name terms) :: body)


planRule : Rule -> Result Problem Database.QueryPlan
planRule (Rule (Atom _ headTerms) bodyAtoms) =
    let
        ( positiveAtoms, negativeAtoms, filters ) =
            List.foldl
                (\bodyAtom ( positiveAtomsSoFar, negativeAtomsSoFar, filtersSoFar ) ->
                    case bodyAtom of
                        BodyAtom Positive atom_ ->
                            ( atom_ :: positiveAtomsSoFar, negativeAtomsSoFar, filtersSoFar )

                        BodyAtom Negative atom_ ->
                            ( positiveAtomsSoFar, atom_ :: negativeAtomsSoFar, filtersSoFar )

                        Filter filter_ ->
                            ( positiveAtomsSoFar, negativeAtomsSoFar, filter_ :: filtersSoFar )
                )
                ( [], [], [] )
                bodyAtoms

        plannedPositiveAtoms : Result Problem ( List String, Database.QueryPlan )
        plannedPositiveAtoms =
            case positiveAtoms of
                [] ->
                    Err NeedAtLeastOnePositiveAtom

                first :: rest ->
                    List.foldl
                        (\nextAtom ( rightNames, rightPlan ) ->
                            let
                                ( leftNames, leftPlan ) =
                                    atomToPlan nextAtom
                            in
                            ( leftNames ++ rightNames
                            , Database.JoinOn
                                { left = leftPlan
                                , right = rightPlan
                                , fields =
                                    Dict.merge
                                        (\_ _ soFar -> soFar)
                                        (\_ left right soFar -> ( left, right ) :: soFar)
                                        (\_ _ soFar -> soFar)
                                        (Dict.fromList (List.indexedMap (\i field -> ( field, i )) leftNames))
                                        (Dict.fromList (List.indexedMap (\i field -> ( field, i )) rightNames))
                                        []
                                }
                            )
                        )
                        (atomToPlan first)
                        rest
                        |> Ok

        plannedNegativeAtoms : Result Problem ( List String, Database.QueryPlan )
        plannedNegativeAtoms =
            case ( negativeAtoms, plannedPositiveAtoms ) of
                ( [], _ ) ->
                    plannedPositiveAtoms

                ( _, Err _ ) ->
                    plannedPositiveAtoms

                ( _, Ok starter ) ->
                    foldrResult
                        (\nextAtom ( keepNames, keepPlan ) ->
                            let
                                ( dropNames, dropPlan ) =
                                    atomToPlan nextAtom
                            in
                            dropNames
                                |> List.indexedMap Tuple.pair
                                |> foldrResult
                                    (\( dropIndex, dropName ) soFar ->
                                        case indexOf dropName keepNames of
                                            Just keepIndex ->
                                                Ok (( keepIndex, dropIndex ) :: soFar)

                                            Nothing ->
                                                Err (VariableMustAppearInPositiveAtom dropName)
                                    )
                                    []
                                |> Result.map
                                    (\fields ->
                                        ( keepNames
                                        , Database.OuterJoinOn
                                            { keep = keepPlan
                                            , drop = dropPlan
                                            , fields = fields
                                            }
                                        )
                                    )
                        )
                        starter
                        negativeAtoms

        planned : Result Problem ( List String, Database.QueryPlan )
        planned =
            case ( filters, plannedNegativeAtoms ) of
                ( [], _ ) ->
                    plannedNegativeAtoms

                ( _, Err _ ) ->
                    plannedNegativeAtoms

                ( _, Ok starter ) ->
                    foldrResult
                        (\nextFilter ( names, plan ) -> filterToPlan nextFilter names plan)
                        starter
                        filters
    in
    Result.andThen
        (\( names, plan ) ->
            if List.isEmpty headTerms then
                Err NeedAtLeastOneName

            else
                headTerms
                    |> foldrResult
                        (\term soFar ->
                            case term of
                                Variable name ->
                                    case indexOf name names of
                                        Just idx ->
                                            Ok (idx :: soFar)

                                        Nothing ->
                                            Err (VariableDoesNotAppearInBody name)

                                Constant _ ->
                                    -- It's fine to just ignore this, since
                                    -- we disallow rules having constants by
                                    -- construction. This will be an unfortunate
                                    -- bug if we ever change that, though! :\
                                    Ok soFar
                        )
                        []
                    |> Result.map (\indexes -> Database.Project indexes plan)
        )
        planned


ruleToString : Rule -> String
ruleToString (Rule head body) =
    atomToString head ++ " :- " ++ String.join ", " (List.map bodyAtomToString body)


type Negation
    = Positive
    | Negative


type BodyAtom
    = BodyAtom Negation Atom
    | Filter Filter


bodyAtomToString : BodyAtom -> String
bodyAtomToString bodyAtom =
    case bodyAtom of
        BodyAtom negation atom_ ->
            let
                notString : String
                notString =
                    case negation of
                        Positive ->
                            ""

                        Negative ->
                            "not "
            in
            notString ++ atomToString atom_

        Filter filter_ ->
            filterToString filter_


type Atom
    = Atom String (List Term)


atomToString : Atom -> String
atomToString (Atom name terms) =
    name ++ "(" ++ String.join ", " (List.map termToString terms) ++ ")"


atomToPlan : Atom -> ( List String, Database.QueryPlan )
atomToPlan (Atom name terms) =
    terms
        |> List.indexedMap Tuple.pair
        |> List.foldr
            (\( fieldNum, term ) ( termNames, plan ) ->
                case term of
                    Variable var_ ->
                        ( var_ :: termNames, plan )

                    Constant constant ->
                        ( "_" :: termNames
                        , plan
                            |> Database.Select
                                (Database.Predicate
                                    fieldNum
                                    Database.Eq
                                    (Database.Constant constant)
                                )
                        )
            )
            ( [], Database.Read name )


{-| Note: we don't need AND here because it's implicit in the list of
conditions in a rule.
-}
type Filter
    = Predicate String Op Term
    | Not Filter
    | Or Filter Filter


type Op
    = Eq
    | Gt
    | Lt


filter : Filter -> Rule -> Rule
filter filter_ (Rule head body) =
    Rule head (Filter filter_ :: body)


eq : String -> Term -> Filter
eq lhs rhs =
    Predicate lhs Eq rhs


gt : String -> Term -> Filter
gt lhs rhs =
    Predicate lhs Gt rhs


lt : String -> Term -> Filter
lt lhs rhs =
    Predicate lhs Lt rhs


not_ : Filter -> Filter
not_ =
    Not


or : Filter -> Filter -> Filter
or =
    Or


filterToPlan : Filter -> List String -> Database.QueryPlan -> Result Problem ( List String, Database.QueryPlan )
filterToPlan topFilter names plan =
    let
        convertField : String -> Result Problem Database.Field
        convertField name =
            case indexOf name names of
                Just idx ->
                    Ok idx

                Nothing ->
                    Err (VariableDoesNotAppearInBody name)

        convertTerm : Term -> Result Problem Database.FieldOrConstant
        convertTerm term =
            case term of
                Variable name ->
                    Result.map Database.Field (convertField name)

                Constant constant ->
                    Ok (Database.Constant constant)

        convertOp : Op -> Database.Op
        convertOp op =
            case op of
                Eq ->
                    Database.Eq

                Lt ->
                    Database.Lt

                Gt ->
                    Database.Gt

        toSelection : Filter -> Result Problem Database.Selection
        toSelection filter_ =
            case filter_ of
                Predicate lhs op rhs ->
                    Result.map3 Database.Predicate
                        (convertField lhs)
                        (Ok (convertOp op))
                        (convertTerm rhs)

                Not inner ->
                    Result.map Database.Not (toSelection inner)

                Or left right ->
                    Result.map2 Database.Or
                        (toSelection left)
                        (toSelection right)
    in
    Result.map
        (\selection -> ( names, Database.Select selection plan ))
        (toSelection topFilter)


filterToString : Filter -> String
filterToString filter_ =
    case filter_ of
        Predicate lhs op rhs ->
            lhs ++ " " ++ opToString op ++ " " ++ termToString rhs

        Not notFilter ->
            "not " ++ filterToString notFilter

        Or left right ->
            filterToString left ++ " or " ++ filterToString right


opToString : Op -> String
opToString op =
    case op of
        Eq ->
            "="

        Lt ->
            "<"

        Gt ->
            ">"


type Term
    = Variable String
    | Constant Constant


var : String -> Term
var =
    Variable


string : String -> Term
string =
    Constant << Database.String


int : Int -> Term
int =
    Constant << Database.Int


termToString : Term -> String
termToString term =
    case term of
        Variable var_ ->
            var_

        Constant (Database.String string_) ->
            "\"" ++ string_ ++ "\""

        Constant (Database.Int int_) ->
            String.fromInt int_


parse : String -> Result Problem (List Rule)
parse input =
    input
        |> Parser.run parser
        |> Result.mapError ParsingProblem


type Context
    = RuleHead
    | VariableInRuleHead
    | NameOfRule
    | AtomInBody
    | TermInAtom


type ParsingProblem
    = ExpectedToken Token
    | ExpectedValidName
    | ExpectedNumber
    | InvalidNumber
    | FloatsAreNotAllowedYet


type Token
    = OpenParenthesis
    | ClosingParenthesis
    | Comma
    | Period
    | Horn
    | DoubleQuote


type alias Parser a =
    Parser.Parser Context ParsingProblem a


parser : Parser (List Rule)
parser =
    Parser.loop [] parserHelp


parserHelp : List Rule -> Parser (Parser.Step (List Rule) (List Rule))
parserHelp soFar =
    Parser.oneOf
        [ Parser.succeed (\newRule -> Parser.Loop (newRule :: soFar))
            |= ruleParser
            |. Parser.spaces
        , Parser.lazy (\_ -> Parser.succeed (Parser.Done (List.reverse soFar)))
        ]


ruleParser : Parser Rule
ruleParser =
    Parser.succeed (List.foldl (\with_ rule_ -> with_ rule_))
        |= ruleHeadParser
        |. Parser.spaces
        |= ruleBodyParser


ruleHeadParser : Parser Rule
ruleHeadParser =
    Parser.succeed rule
        |= Parser.inContext NameOfRule nameParser
        |. Parser.spaces
        |= Parser.sequence
            { start = openParenToken
            , separator = commaToken
            , end = closeParenToken
            , spaces = Parser.spaces
            , item = Parser.inContext VariableInRuleHead nameParser
            , trailing = Parser.Forbidden
            }
        |> Parser.inContext RuleHead


ruleBodyParser : Parser (List (Rule -> Rule))
ruleBodyParser =
    Parser.sequence
        { start = hornToken
        , separator = commaToken
        , end = periodToken
        , spaces = Parser.spaces
        , item = Parser.inContext AtomInBody bodyAtomParser
        , trailing = Parser.Forbidden
        }


bodyAtomParser : Parser (Rule -> Rule)
bodyAtomParser =
    Parser.succeed with
        |= nameParser
        |. Parser.spaces
        |= Parser.sequence
            { start = openParenToken
            , separator = commaToken
            , end = closeParenToken
            , spaces = Parser.spaces
            , item = Parser.inContext TermInAtom termParser
            , trailing = Parser.Forbidden
            }


termParser : Parser Term
termParser =
    Parser.oneOf
        [ Parser.number
            { int = Ok identity
            , hex = Ok identity
            , octal = Ok identity
            , binary = Ok identity
            , float = Err FloatsAreNotAllowedYet
            , invalid = InvalidNumber
            , expecting = ExpectedNumber
            }
            |> Parser.map int
        , Parser.succeed string
            |. Parser.token doubleQuoteToken
            |= Parser.getChompedString (Parser.chompWhile (\c -> c /= doubleQuoteChar))
            |. Parser.token doubleQuoteToken
        , Parser.map var nameParser
        ]


nameParser : Parser String
nameParser =
    Parser.variable
        { start = \c -> not (Set.member c disallowedNameChar)
        , inner = \c -> not (Set.member c disallowedNameChar)
        , reserved =
            Set.fromList
                [ "_" -- reserved for future use as an anonymous placeholder
                ]
        , expecting = ExpectedValidName
        }


disallowedNameChar : Set Char
disallowedNameChar =
    Set.fromList
        [ -- https://en.wikipedia.org/wiki/Whitespace_character
          '\t'
        , '\n'
        , '\u{000B}'
        , '\u{000C}'
        , '\u{000D}'
        , ' '
        , '\u{0085}'
        , '\u{00A0}'
        , '\u{1680}'
        , '\u{2000}'
        , '\u{2001}'
        , '\u{2002}'
        , '\u{2003}'
        , '\u{2004}'
        , '\u{2005}'
        , '\u{2006}'
        , '\u{2007}'
        , '\u{2008}'
        , '\u{2009}'
        , '\u{200A}'
        , '\u{2028}'
        , '\u{2029}'
        , '\u{202F}'
        , '\u{205F}'
        , '\u{3000}'
        , '\u{180E}'
        , '\u{200B}'
        , '\u{200C}'
        , '\u{200D}'
        , '\u{2060}'
        , '\u{FEFF}'

        -- symbols that follow names. We can't allow these or we won't know
        -- where a name ends and the next thing we need to parse begins!
        , '('
        , ')'
        , ','
        ]



-- TOKENS


openParenToken : Parser.Token ParsingProblem
openParenToken =
    Parser.Token "(" (ExpectedToken OpenParenthesis)


closeParenToken : Parser.Token ParsingProblem
closeParenToken =
    Parser.Token ")" (ExpectedToken ClosingParenthesis)


commaToken : Parser.Token ParsingProblem
commaToken =
    Parser.Token "," (ExpectedToken Comma)


periodToken : Parser.Token ParsingProblem
periodToken =
    Parser.Token "." (ExpectedToken Period)


hornToken : Parser.Token ParsingProblem
hornToken =
    Parser.Token ":-" (ExpectedToken Horn)


doubleQuoteToken : Parser.Token ParsingProblem
doubleQuoteToken =
    Parser.Token ":-" (ExpectedToken DoubleQuote)


{-| This is down here because my editor's highlighting is busted and it
thinks everything after '"' is a string.
-}
doubleQuoteChar : Char
doubleQuoteChar =
    '"'
