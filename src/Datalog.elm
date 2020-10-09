module Datalog exposing (Program(..), Rule(..), solve)

import Datalog.Atom as Atom exposing (Atom(..))
import Datalog.Term as Term exposing (Term(..))
import Dict exposing (Dict)
import Sort exposing (Sorter)


type Program
    = Program (List Rule)


type Rule
    = Rule Atom (List Atom)


{-| This is cheating a bit. A database is only ground atoms--that is, atoms
whose terms are all constants.
-}
type alias Database =
    Dict String (List Atom)


insertAtom : Atom -> Database -> Database
insertAtom ((Atom name _) as atom) database =
    Dict.update name
        (\maybeExisting ->
            case maybeExisting of
                Just existing ->
                    if List.member atom existing then
                        Just existing

                    else
                        Just (atom :: existing)

                Nothing ->
                    Just [ atom ]
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
evaluateRule (Rule head body) database =
    if Atom.isGround head then
        insertAtom head database

    else
        database
