module Datalog exposing (Atom, Rule, Term, atom, headAtom, rule, ruleToPlan, string, var)

import Database exposing (Constant)
import Dict


type Problem
    = CannotPlanFact


type Rule
    = Rule Atom (List BodyAtom)


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
    planned
        |> Result.map
            (\( names, plan ) ->
                Database.Project
                    (List.filterMap
                        (\term ->
                            case term of
                                Variable name ->
                                    -- TODO: validate that all atoms in the head appear in
                                    -- the body
                                    indexOf name names

                                Constant _ ->
                                    -- TODO: validate that constant terms don't appear in the
                                    -- head, or deal with them somehow.
                                    Nothing
                        )
                        headTerms
                    )
                    plan
            )


bodyAtomToPlan : BodyAtom -> ( List String, Database.QueryPlan )
bodyAtomToPlan bodyAtom =
    case bodyAtom of
        BodyAtom atom_ ->
            atomToPlan atom_


indexOf : a -> List a -> Maybe Int
indexOf =
    indexOfHelp 0


indexOfHelp : Int -> a -> List a -> Maybe Int
indexOfHelp idx item items =
    case items of
        [] ->
            Nothing

        first :: rest ->
            if first == item then
                Just idx

            else
                indexOfHelp (idx + 1) item rest


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
