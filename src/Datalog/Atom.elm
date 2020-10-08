module Datalog.Atom exposing (Atom(..), Substitutions, unify)

import Datalog.Term exposing (Term(..))
import Dict exposing (Dict)


type Atom
    = Atom String (List Term)


type alias Substitutions =
    Dict String String


unify : Atom -> Atom -> Maybe Substitutions
unify (Atom aName aTerms) (Atom bName bTerms) =
    if aName == bName && List.length aTerms == List.length bTerms then
        unifyHelp (List.map2 Tuple.pair aTerms bTerms) Dict.empty

    else
        Nothing


unifyHelp : List ( Term, Term ) -> Substitutions -> Maybe Substitutions
unifyHelp termPairs substitutions =
    let
        variableToConstant var const rest =
            case Dict.get var substitutions of
                Nothing ->
                    unifyHelp rest (Dict.insert var const substitutions)

                Just alreadyBound ->
                    if alreadyBound == const then
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

        ( Variable var, Constant const ) :: rest ->
            variableToConstant var const rest

        ( Constant const, Variable var ) :: rest ->
            variableToConstant var const rest

        ( Variable _, Variable _ ) :: rest ->
            unifyHelp rest substitutions
