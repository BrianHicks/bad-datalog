module Datalog exposing (Database, Problem(..), Program, program, solve)

import Datalog.Atom as Atom exposing (Atom, Substitutions)
import Datalog.Negatable as Negatable exposing (Direction(..), Negatable(..))
import Datalog.Rule as Rule exposing (Rule)
import Datalog.Term as Term exposing (Term(..))
import Dict exposing (Dict)
import Graph


type Program
    = Program (List (List Rule))


program : List Rule -> Result Problem Program
program rules =
    Result.map Program (stratify rules)


type Problem
    = CycleWithNegation



-- STRATIFICATION


stratify : List Rule -> Result Problem (List (List Rule))
stratify rules =
    -- some future version of this function may want to create the entire graph
    -- in a single pass over the rules. That'd be fine, of course, and faster,
    -- but potentially way harder to work with. Keep it simple for now!
    let
        namesToIds =
            rules
                |> List.concatMap
                    (\rule ->
                        Atom.key (Rule.head rule)
                            :: List.map
                                (\atom -> Atom.key (Negatable.value atom))
                                (Rule.body rule)
                    )
                |> List.foldl
                    (\key soFar ->
                        Dict.update key
                            (\value ->
                                case value of
                                    Just id ->
                                        Just id

                                    Nothing ->
                                        Just (Dict.size soFar)
                            )
                            soFar
                    )
                    Dict.empty

        nodes =
            Dict.foldl
                (\key id soFar ->
                    Graph.Node id key :: soFar
                )
                []
                namesToIds

        edges =
            rules
                |> List.concatMap
                    (\rule ->
                        let
                            headId =
                                namesToIds
                                    |> Dict.get (Atom.key (Rule.head rule))
                                    |> Maybe.withDefault -1
                        in
                        List.map
                            (\(Negatable direction bodyAtom) ->
                                Graph.Edge
                                    (namesToIds
                                        |> Dict.get (Atom.key bodyAtom)
                                        |> Maybe.withDefault -1
                                    )
                                    headId
                                    direction
                            )
                            (Rule.body rule)
                    )

        precedenceGraph =
            Graph.fromNodesAndEdges nodes edges

        rulesByName =
            List.foldr
                (\rule soFar ->
                    Dict.update (Atom.key (Rule.head rule))
                        (\maybeRules ->
                            case maybeRules of
                                Nothing ->
                                    Just [ rule ]

                                Just already ->
                                    Just (rule :: already)
                        )
                        soFar
                )
                Dict.empty
                rules

        scc =
            Graph.stronglyConnectedComponents precedenceGraph
                |> Result.mapError (List.map Graph.edges)
    in
    case Graph.stronglyConnectedComponents precedenceGraph of
        -- pretty unlikely, but ok fine we'll handle it
        Ok acyclic ->
            Ok [ rules ]

        Err condensation ->
            if List.any (Graph.edges >> List.map .label >> List.member Negative) condensation then
                Err CycleWithNegation

            else
                condensation
                    |> List.concatMap
                        (Graph.nodes
                            >> List.filterMap (\{ label } -> Dict.get label rulesByName |> Maybe.map (Tuple.pair label))
                            >> Dict.fromList
                            >> Dict.values
                        )
                    |> Ok



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
solve (Program rules) =
    List.foldl solveHelp Dict.empty rules


solveHelp : List Rule -> Database -> Database
solveHelp rules database =
    let
        expanded =
            List.foldl evaluateRule database rules
    in
    if expanded == database then
        database

    else
        solveHelp rules expanded


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

                Negatable Negative (Just _) ->
                    []

                Negatable Negative Nothing ->
                    evaluateAtomHelp
                        bound
                        substitutions
                        rest
                        (substitutions :: soFar)
