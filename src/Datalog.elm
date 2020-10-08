module Datalog exposing (..)

import Dict exposing (Dict)
import Sort exposing (Sorter)


type Program
    = Program (List Rule)


type Rule
    = Rule Atom (List Atom)


type Atom
    = Atom String (List Term)


type Term
    = Constant String
    | Variable String


{-| This is cheating a bit. A database is only ground atoms--that is, atoms
whose terms are all constants.
-}
type Database
    = Database (List Atom)



-- EVALUATION


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



-- EXAMPLES


allPairsReachability : Program
allPairsReachability =
    Program
        [ -- base data
          Rule (Atom "link" [ Constant "a", Constant "b" ]) []
        , Rule (Atom "link" [ Constant "b", Constant "c" ]) []
        , Rule (Atom "link" [ Constant "c", Constant "c" ]) []
        , Rule (Atom "link" [ Constant "c", Constant "d" ]) []

        -- derivations
        , Rule
            (Atom "reachable" [ Variable "X", Variable "Y" ])
            [ Atom "link" [ Variable "X", Variable "Y" ] ]
        , Rule
            (Atom "reachable" [ Variable "X", Variable "Y" ])
            [ Atom "link" [ Variable "X", Variable "Z" ]
            , Atom "reachable" [ Variable "Z", Variable "Y" ]
            ]

        -- query
        , Rule
            (Atom "query" [ Variable "X", Variable "Y" ])
            [ Atom "reachable" [ Variable "X", Variable "Y" ] ]
        ]
