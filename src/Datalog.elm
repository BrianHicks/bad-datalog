module Datalog exposing (Atom, Rule, Term, atom, rule, ruleToPlan, var)

import Database exposing (Constant)


type Problem
    = CannotPlanFact


type Rule
    = Rule Atom (List Atom)


rule : Atom -> List Atom -> Rule
rule =
    Rule


ruleToPlan : Rule -> Result Problem Database.QueryPlan
ruleToPlan (Rule (Atom _ headTerms) bodyAtoms) =
    let
        planned =
            case bodyAtoms of
                [] ->
                    Err CannotPlanFact

                [ only ] ->
                    Ok (atomToPlan only)

                first :: rest ->
                    Debug.todo "combine more than one atom"
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


atom : String -> List Term -> Atom
atom =
    Atom


atomToPlan : Atom -> ( List String, Database.QueryPlan )
atomToPlan (Atom name terms) =
    terms
        |> List.indexedMap Tuple.pair
        |> List.foldr
            (\( fieldNum, term ) ( termNames, plan ) ->
                case term of
                    Variable var_ ->
                        ( var_ :: termNames, plan )

                    Constant _ ->
                        Debug.todo "constant"
            )
            ( [], Database.Read name )


type Term
    = Variable String
    | Constant Constant


var : String -> Term
var =
    Variable
