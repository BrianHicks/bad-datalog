module Datalog exposing (Database, Program(..), Rule(..), solve)

import Datalog.Atom as Atom exposing (Atom(..), Substitutions)
import Datalog.Term as Term exposing (Term(..))
import Dict exposing (Dict)


type Program
    = Program (List Rule)


type Rule
    = Rule Atom (List Atom)


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
evaluateRule ((Rule head body) as rule) database =
    if Atom.isGround head then
        insertAtom head database

    else
        body
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
                            Atom.substitute head substitution
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
