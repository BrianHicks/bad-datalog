module Datalog.Atom exposing
    ( Atom, atom, key, isGround, variables
    , Substitutions, emptySubstitutions, unify, substitute, mergeSubstitutions
    , toString
    )

{-|

@docs Atom, atom, key, isGround, variables

@docs Substitutions, emptySubstitutions, unify, substitute, mergeSubstitutions

@docs toString

-}

import Datalog.Term as Term exposing (Term(..))
import Sort.Dict as Dict exposing (Dict)


type Atom
    = Atom String Int (List Term)


atom : String -> List Term -> Atom
atom name terms =
    Atom name (List.length terms) terms


{-| TODO: this is useful but kind of internal. Should it move somewhere else,
or split into `name` and `size`?
-}
key : Atom -> ( String, Int )
key (Atom name arity _) =
    ( name, arity )


isGround : Atom -> Bool
isGround (Atom _ _ terms) =
    not (List.isEmpty terms) && List.all Term.isGround terms


variables : Atom -> List Term.Variable
variables (Atom _ _ terms) =
    List.filterMap
        (\term ->
            case term of
                Term.Constant _ ->
                    Nothing

                Term.Variable var ->
                    Just var
        )
        terms


type alias Substitutions =
    Dict Term.Variable Term.Constant


emptySubstitutions : Substitutions
emptySubstitutions =
    Dict.empty Term.variableSorter


unify : Atom -> Atom -> Maybe Substitutions
unify (Atom aName aArity aTerms) (Atom bName bArity bTerms) =
    if aName == bName && aArity == bArity then
        unifyHelp (List.map2 Tuple.pair aTerms bTerms) emptySubstitutions

    else
        Nothing


unifyHelp : List ( Term, Term ) -> Substitutions -> Maybe Substitutions
unifyHelp termPairs substitutions =
    let
        variableToConstant : Term.Variable -> Term.Constant -> List ( Term, Term ) -> Maybe Substitutions
        variableToConstant var constant rest =
            if Term.isAnonymous var then
                unifyHelp rest substitutions

            else
                case Dict.get var substitutions of
                    Nothing ->
                        unifyHelp rest (Dict.insert var constant substitutions)

                    Just alreadyBound ->
                        if alreadyBound == constant then
                            unifyHelp rest substitutions

                        else
                            Nothing
    in
    case termPairs of
        [] ->
            Just substitutions

        ( Constant a, Constant b ) :: rest ->
            if a == b then
                unifyHelp rest substitutions

            else
                Nothing

        ( Variable _, Variable _ ) :: rest ->
            unifyHelp rest substitutions

        ( Variable var, Constant constant ) :: rest ->
            variableToConstant var constant rest

        ( Constant constant, Variable var ) :: rest ->
            variableToConstant var constant rest


substitute : Atom -> Substitutions -> Atom
substitute (Atom name arity terms) substitutions =
    terms
        |> List.map
            (\term ->
                case term of
                    Constant _ ->
                        term

                    Variable var ->
                        case Dict.get var substitutions of
                            Just boundTerm ->
                                Constant boundTerm

                            Nothing ->
                                term
            )
        |> Atom name arity


mergeSubstitutions : Substitutions -> Substitutions -> Substitutions
mergeSubstitutions a b =
    Dict.insertAll a b


toString : Atom -> String
toString (Atom name _ terms) =
    name ++ "(" ++ String.join ", " (List.map Term.toString terms) ++ ")"
