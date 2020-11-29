module Datalog exposing (Database, Program, program, solve)

import Datalog.Atom as Atom exposing (Atom, Substitutions)
import Datalog.Negatable as Negatable exposing (Direction(..), Negatable(..))
import Datalog.Rule as Rule exposing (Rule)
import Datalog.Term as Term exposing (Term(..))
import Dict exposing (Dict)


type Program
    = Program (List Rule)


program : List Rule -> Program
program =
    Program



-- EVALUATION


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


solve : Program -> Database
solve program_ =
    solveHelp program_ Dict.empty


solveHelp : Program -> Database -> Database
solveHelp ((Program rules) as program_) database =
    let
        expanded =
            List.foldl evaluateRule database rules
    in
    if expanded == database then
        database

    else
        solveHelp program_ expanded


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
    case Dict.get (Atom.key (Negatable.value bound)) database of
        Nothing ->
            []

        Just ( first, rest ) ->
            evaluateAtomHelp bound substitutions (first :: rest) []


evaluateAtomHelp : Negatable Atom -> Substitutions -> List Atom -> List Substitutions -> List Substitutions
evaluateAtomHelp bound substitutions facts soFar =
    case facts of
        [] ->
            soFar

        fact :: rest ->
            case Negatable.map (Atom.unify fact) bound of
                Negatable Positive (Just outcome) ->
                    evaluateAtomHelp
                        bound
                        substitutions
                        rest
                        (Atom.mergeSubstitutions substitutions outcome :: soFar)

                Negatable Positive Nothing ->
                    evaluateAtomHelp bound substitutions rest soFar

                -- the idea with negative atoms is essentially that we
                -- select the complement of things that it would select if
                -- it were positive. We do this by just flipping around the
                -- positive/negative results! It seems to give weird results in
                -- some cases (probably because I haven't implemented stratified
                -- negation yet.)
                Negatable Negative (Just _) ->
                    []

                Negatable Negative Nothing ->
                    evaluateAtomHelp
                        bound
                        substitutions
                        rest
                        (substitutions :: soFar)
