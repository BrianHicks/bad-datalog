module Datalog exposing
    ( Database, empty, Problem(..), insert, query
    , Rule, rule, ruleToPlan
    , BodyAtom, atom, notAtom
    , Atom, headAtom
    , Filter, filter, eq, gt, lt, not_, or
    , Term, var, int, string
    )

{-|

@docs Database, empty, Problem, insert, query

@docs Rule, rule, ruleToPlan

@docs BodyAtom, atom, notAtom

@docs Atom, headAtom

@docs Filter, filter, eq, gt, lt, not_, or

@docs Term, var, int, string

-}

import Database exposing (Constant)
import Dict
import Graph exposing (Edge, Graph, Node)
import List.Extra exposing (foldrResult, indexOf)
import Murmur3
import Sort.Set


type Database
    = Database Database.Database


empty : Database
empty =
    Database Database.empty


type Problem
    = NeedAtLeastOnePositiveAtom
    | VariableDoesNotAppearInBody String
    | VariableMustAppearInPositiveAtom String
    | CannotInsertVariable String
    | CannotHaveNegationInRecursiveQuery
    | DatabaseProblem Database.Problem


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


query : List Rule -> Database -> Result Problem Database.Database
query rules (Database db) =
    let
        nodes : Result Problem (List (Node ( String, Maybe Database.QueryPlan )))
        nodes =
            rules
                |> foldrResult
                    (\((Rule (Atom name _) _) as rule_) soFar ->
                        case ruleToPlan rule_ of
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
    Result.andThen
        (\strata ->
            foldrResult
                runUntilExhausted
                db
                strata
        )
        strataResult


runUntilExhausted :
    Graph ( String, Maybe Database.QueryPlan ) Negation
    -> Database.Database
    -> Result Problem Database.Database
runUntilExhausted stratum db =
    runUntilExhaustedHelp stratum db db


runUntilExhaustedHelp :
    Graph ( String, Maybe Database.QueryPlan ) Negation
    -> Database.Database
    -> Database.Database
    -> Result Problem Database.Database
runUntilExhaustedHelp stratum db finalDb =
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
                runUntilExhaustedHelp stratum nextDb newFinalDb

        Err problem ->
            Err problem


type Rule
    = Rule Atom (List BodyAtom)


rule : Atom -> List BodyAtom -> Rule
rule =
    Rule


ruleToString : Rule -> String
ruleToString (Rule head body) =
    atomToString head ++ " :- " ++ String.join ", " (List.map bodyAtomToString body)


ruleToPlan : Rule -> Result Problem Database.QueryPlan
ruleToPlan (Rule (Atom _ headTerms) bodyAtoms) =
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


type Negation
    = Positive
    | Negative


type BodyAtom
    = BodyAtom Negation Atom
    | Filter Filter


atom : String -> List Term -> BodyAtom
atom name terms =
    BodyAtom Positive (Atom name terms)


notAtom : String -> List Term -> BodyAtom
notAtom name terms =
    BodyAtom Negative (Atom name terms)


bodyAtomToString : BodyAtom -> String
bodyAtomToString bodyAtom =
    case bodyAtom of
        BodyAtom negation atom_ ->
            let
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


headAtom : String -> List String -> Atom
headAtom name vars =
    Atom name (List.map Variable vars)


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


filter : Filter -> BodyAtom
filter =
    Filter


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
