module Datalog.Atom exposing
    ( Atom, atom, name, isGround
    , Substitutions, emptySubstitutions, unify, substitute, mergeSubstitutions
    , toString
    )

{-|

@docs Atom, atom, name, isGround

@docs Substitutions, emptySubstitutions, unify, substitute, mergeSubstitutions

@docs toString

-}

import Datalog.Term as Term exposing (Term(..))
import Sort.Dict as Dict exposing (Dict)


type Atom
    = Atom String (List Term)


atom : String -> List Term -> Atom
atom name_ terms =
    Atom name_ terms


name : Atom -> String
name (Atom name_ _) =
    name_


isGround : Atom -> Bool
isGround (Atom _ terms) =
    not (List.isEmpty terms) && List.all Term.isGround terms


type alias Substitutions =
    Dict Term.Variable Term.Constant


emptySubstitutions : Substitutions
emptySubstitutions =
    Dict.empty Term.variableSorter


unify : Atom -> Atom -> Maybe Substitutions
unify (Atom aName aTerms) (Atom bName bTerms) =
    if aName == bName && List.length aTerms == List.length bTerms then
        unifyHelp (List.map2 Tuple.pair aTerms bTerms) emptySubstitutions

    else
        Nothing


unifyHelp : List ( Term, Term ) -> Substitutions -> Maybe Substitutions
unifyHelp termPairs substitutions =
    let
        variableToConstant : Term.Variable -> Term.Constant -> List ( Term, Term ) -> Maybe Substitutions
        variableToConstant var constant rest =
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
substitute (Atom name_ terms) substitutions =
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
        |> Atom name_


mergeSubstitutions : Substitutions -> Substitutions -> Substitutions
mergeSubstitutions a b =
    Dict.insertAll a b


toString : Atom -> String
toString (Atom name_ terms) =
    name_ ++ "(" ++ String.join ", " (List.map Term.toString terms) ++ ")"
