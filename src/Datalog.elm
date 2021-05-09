module Datalog exposing (Atom, Database, Problem(..), Rule, Term, atom, empty, headAtom, insert, query, rule, ruleToPlan, string, var)

import Database exposing (Constant)
import Dict
import List.Extra exposing (foldrResult, indexOf)


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
    -- 1. get a topological sort of the body rules starting from the query rule
    -- 2. exclude any body rules that end up unused
    -- 3. starting from the leafmost dependencies, perform naive (or semi-naive) evaluation, continually inserting the new rows into the database
    -- 4. read the final name of `queryRule` once everything's done
    Debug.todo "query"


type Problem
    = CannotPlanFact
    | VariableDoesNotAppearInBody String
    | CannotInsertVariable String
    | DatabaseProblem Database.Problem


type Rule
    = Rule Atom (List BodyAtom)


{-| TODO: predicates
-}
type BodyAtom
    = BodyAtom Atom


rule : Atom -> List BodyAtom -> Rule
rule =
    Rule


ruleToPlan : Rule -> Result Problem Database.QueryPlan
ruleToPlan (Rule (Atom _ headTerms) bodyAtoms) =
    let
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


var : String -> Term
var =
    Variable


string : String -> Term
string =
    Constant << Database.String
