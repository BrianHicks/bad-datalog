module Datalog.Atom exposing
    ( Atom(..), isGround
    , Substitutions, emptySubstitutions, unify, substitute, mergeSubstitutions
    , toString
    )

{-|

@docs Atom, isGround

@docs Substitutions, emptySubstitutions, unify, substitute, mergeSubstitutions

@docs toString

-}

import Datalog.Term as Term exposing (Term(..))
import Dict exposing (Dict)


type Atom
    = Atom String (List Term)


isGround : Atom -> Bool
isGround (Atom _ terms) =
    not (List.isEmpty terms) && List.all Term.isGround terms


type alias Substitutions =
    Dict String Term


emptySubstitutions : Substitutions
emptySubstitutions =
    Dict.empty


unify : Atom -> Atom -> Maybe Substitutions
unify (Atom aName aTerms) (Atom bName bTerms) =
    if aName == bName && List.length aTerms == List.length bTerms then
        unifyHelp (List.map2 Tuple.pair aTerms bTerms) Dict.empty

    else
        Nothing


unifyHelp : List ( Term, Term ) -> Substitutions -> Maybe Substitutions
unifyHelp termPairs substitutions =
    let
        variableToConstant : String -> Term -> List ( Term, Term ) -> Maybe Substitutions
        variableToConstant var term rest =
            case Dict.get var substitutions of
                Nothing ->
                    unifyHelp rest (Dict.insert var term substitutions)

                Just alreadyBound ->
                    if alreadyBound == term then
                        unifyHelp rest substitutions

                    else
                        Nothing
    in
    case termPairs of
        [] ->
            Just substitutions

        ( String a, String b ) :: rest ->
            if a == b then
                unifyHelp rest substitutions

            else
                Nothing

        ( Int a, Int b ) :: rest ->
            if a == b then
                unifyHelp rest substitutions

            else
                Nothing

        ( Variable _, Variable _ ) :: rest ->
            unifyHelp rest substitutions

        ( String _, Int _ ) :: rest ->
            Nothing

        ( Int _, String _ ) :: rest ->
            Nothing

        ( Variable var, otherTerm ) :: rest ->
            variableToConstant var otherTerm rest

        ( otherTerm, Variable var ) :: rest ->
            variableToConstant var otherTerm rest


substitute : Atom -> Substitutions -> Atom
substitute (Atom name terms) substitutions =
    terms
        |> List.map
            (\term ->
                case term of
                    String _ ->
                        term

                    Int _ ->
                        term

                    Variable var ->
                        case Dict.get var substitutions of
                            Just boundTerm ->
                                boundTerm

                            Nothing ->
                                term
            )
        |> Atom name


mergeSubstitutions : Substitutions -> Substitutions -> Substitutions
mergeSubstitutions a b =
    Dict.union a b


toString : Atom -> String
toString (Atom name terms) =
    name ++ "(" ++ String.join ", " (List.map Term.toString terms) ++ ")"
