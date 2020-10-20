module Datalog exposing (Database, Program(..), solve)

import Datalog.Atom as Atom exposing (Atom, Substitutions)
import Datalog.Negatable as Negatable
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
                        (evaluateAtom database (Negatable.unwrap bodyAtom))
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


evaluateAtom : Database -> Atom -> Substitutions -> List Substitutions
evaluateAtom database atom substitutions =
    let
        bound =
            Atom.substitute atom substitutions
    in
    case Dict.get (Atom.key atom) database of
        -- TODO: would it be possible to specify that the database values are non-empty?
        Nothing ->
            []

        Just ( first, rest ) ->
            List.filterMap
                (Atom.unify bound >> Maybe.map (Atom.mergeSubstitutions substitutions))
                (first :: rest)
