module Datalog exposing (Atom, BodyAtom, Database, Problem(..), Rule, Term, atom, empty, headAtom, insert, int, query, rule, ruleToPlan, string, var)

import Database exposing (Constant)
import Dict
import Graph exposing (Edge, Graph, Node)
import List.Extra exposing (foldrResult, indexOf)
import Murmur3


type Database
    = Database Database.Database


empty : Database
empty =
    Database Database.empty


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

        edges : List (Edge ())
        edges =
            List.concatMap
                (\((Rule (Atom headName _) bodyAtoms) as rule_) ->
                    Edge
                        (Murmur3.hashString 0 headName)
                        (Murmur3.hashString 0 (ruleToString rule_))
                        ()
                        :: List.map
                            (\bodyAtom ->
                                case bodyAtom of
                                    BodyAtom (Atom bodyName _) ->
                                        Edge
                                            (Murmur3.hashString 0 (ruleToString rule_))
                                            (Murmur3.hashString 0 bodyName)
                                            ()
                            )
                            bodyAtoms
                )
                rules

        strataResult : Result Problem (List (Graph ( String, Maybe Database.QueryPlan ) ()))
        strataResult =
            Result.map
                (\nodes_ ->
                    let
                        graph : Graph ( String, Maybe Database.QueryPlan ) ()
                        graph =
                            Graph.fromNodesAndEdges nodes_ edges
                    in
                    case Graph.stronglyConnectedComponents graph of
                        Ok _ ->
                            [ graph ]

                        Err stronglyConnectedComponents ->
                            stronglyConnectedComponents
                )
                nodes
    in
    Result.andThen
        (\strata ->
            foldrResult runUntilExhausted
                db
                strata
        )
        strataResult


runUntilExhausted stratum db =
    let
        newResult =
            foldrResult
                (\{ label } soFar ->
                    let
                        ( name, maybePlan ) =
                            label
                    in
                    case maybePlan of
                        Just plan ->
                            soFar
                                |> Database.query plan
                                |> Result.andThen (\relation -> Database.insertRelation name relation soFar)
                                |> Result.mapError DatabaseProblem

                        Nothing ->
                            Ok soFar
                )
                db
                (Graph.nodes stratum)
    in
    case newResult of
        Ok new ->
            if new == db then
                newResult

            else
                runUntilExhausted stratum new

        Err _ ->
            newResult


type Problem
    = CannotPlanFact
    | VariableDoesNotAppearInBody String
    | CannotInsertVariable String
    | DatabaseProblem Database.Problem


type Rule
    = Rule Atom (List BodyAtom)


ruleToString : Rule -> String
ruleToString (Rule head body) =
    atomToString head ++ " :- " ++ String.join ", " (List.map bodyAtomToString body)


{-| TODO: predicates
-}
type BodyAtom
    = BodyAtom Atom


bodyAtomToString : BodyAtom -> String
bodyAtomToString bodyAtom =
    case bodyAtom of
        BodyAtom atom_ ->
            atomToString atom_


rule : Atom -> List BodyAtom -> Rule
rule =
    Rule


ruleToPlan : Rule -> Result Problem Database.QueryPlan
ruleToPlan (Rule (Atom _ headTerms) bodyAtoms) =
    let
        planned : Result Problem ( List String, Database.QueryPlan )
        planned =
            case bodyAtoms of
                [] ->
                    Err CannotPlanFact

                first :: rest ->
                    List.foldl
                        (\nextAtom ( rightNames, rightPlan ) ->
                            let
                                ( leftNames, leftPlan ) =
                                    bodyAtomToPlan nextAtom
                            in
                            ( leftNames ++ rightNames
                            , Database.Join
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
                        (bodyAtomToPlan first)
                        rest
                        |> Ok
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


bodyAtomToPlan : BodyAtom -> ( List String, Database.QueryPlan )
bodyAtomToPlan bodyAtom =
    case bodyAtom of
        BodyAtom atom_ ->
            atomToPlan atom_


type Atom
    = Atom String (List Term)


atomToString : Atom -> String
atomToString (Atom name terms) =
    name ++ "(" ++ String.join ", " (List.map termToString terms) ++ ")"


atom : String -> List Term -> BodyAtom
atom name terms =
    BodyAtom (Atom name terms)


headAtom : String -> List String -> Atom
headAtom name vars =
    Atom name (List.map Variable vars)


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


type Term
    = Variable String
    | Constant Constant


termToString : Term -> String
termToString term =
    case term of
        Variable var_ ->
            var_

        Constant (Database.String string_) ->
            "\"" ++ string_ ++ "\""

        Constant (Database.Int int_) ->
            String.fromInt int_


var : String -> Term
var =
    Variable


string : String -> Term
string =
    Constant << Database.String


int : Int -> Term
int =
    Constant << Database.Int
