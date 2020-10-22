module Datalog exposing (Database, Program(..), solve)

import Datalog.Atom as Atom exposing (Atom, Substitutions)
import Datalog.Negatable as Negatable exposing (Negatable)
import Datalog.Rule as Rule exposing (Rule)
import Datalog.Term as Term exposing (Term(..))
import Dict exposing (Dict)


type Program
    = Program (List Rule)


{-| This is cheating a bit. A database is only ground atoms--that is, atoms
whose terms are all constants.
-}
type alias Database =
    Dict ( String, Int ) ( Atom, List Atom )


insertAtom : Atom -> Database -> Database
insertAtom atom database =
    Dict.update (Atom.key atom)
        (\maybeExisting ->
            case maybeExisting of
                Just ( first, rest ) ->
                    if atom == first || List.member atom rest then
                        Just ( first, rest )

                    else
                        Just ( atom, first :: rest )

                Nothing ->
                    Just ( atom, [] )
        )
        database



-- EVALUATION


solve : Program -> Database
solve program =
    solveHelp program Dict.empty


solveHelp : Program -> Database -> Database
solveHelp ((Program rules) as program) database =
    let
        expanded =
            List.foldl evaluateRule database rules
    in
    if expanded == database then
        database

    else
        solveHelp program expanded


evaluateRule : Rule -> Database -> Database
evaluateRule rule database =
    if Rule.isFact rule then
        insertAtom (Rule.head rule) database

    else
        Rule.body rule
            |> List.foldl
                (\bodyAtom substitutions ->
                    List.concatMap
                        (evaluateAtom database bodyAtom)
                        substitutions
                )
                [ Atom.emptySubstitutions ]
            |> List.foldl
                (\substitution dbProgress ->
                    let
                        possiblyGround =
                            Atom.substitute (Rule.head rule) substitution
                    in
                    if Atom.isGround possiblyGround then
                        insertAtom possiblyGround dbProgress

                    else
                        dbProgress
                )
                database


evaluateAtom : Database -> Negatable Atom -> Substitutions -> List Substitutions
evaluateAtom database negatableAtom substitutions =
    let
        bound =
            Negatable.map (\atom -> Atom.substitute atom substitutions) negatableAtom
    in
    case Dict.get (Atom.key (Negatable.unwrap bound)) database of
        -- TODO: would it be possible to specify that the database values are non-empty?
        Nothing ->
            []

        Just ( first, rest ) ->
            evaluateAtomHelp bound substitutions (first :: rest) []


evaluateAtomHelp : Negatable Atom -> Substitutions -> List Atom -> List Substitutions -> List Substitutions
evaluateAtomHelp negatableAtom substitutions facts soFar =
    let
        bound =
            Negatable.unwrap negatableAtom
    in
    case facts of
        [] ->
            soFar

        fact :: rest ->
            case ( Negatable.isPositive negatableAtom, Atom.unify bound fact ) of
                ( True, Just outcome ) ->
                    evaluateAtomHelp
                        negatableAtom
                        substitutions
                        rest
                        (Atom.mergeSubstitutions substitutions outcome :: soFar)

                ( True, Nothing ) ->
                    evaluateAtomHelp negatableAtom substitutions rest soFar

                -- negative: we just flip the result of unification around!
                ( False, Just outcome ) ->
                    []

                ( False, Nothing ) ->
                    evaluateAtomHelp
                        negatableAtom
                        substitutions
                        rest
                        (substitutions :: soFar)
