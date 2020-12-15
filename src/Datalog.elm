module Datalog exposing (Database, Program, program, solve)

import Datalog.Atom as Atom exposing (Atom, Substitutions)
import Datalog.Negatable as Negatable exposing (Direction(..), Negatable(..))
import Datalog.Rule as Rule exposing (Rule)
import Datalog.Term as Term exposing (Term(..))
import Dict exposing (Dict)
import Graph


type Program
    = Program (List (List Rule))


program : List Rule -> Program
program rules =
    Program (stratify rules)



-- STRATIFICATION
{-

   We have a couple of steps to complete here:

      - [ ] make a precedence graph (edges from body atoms to head atom
            for each rule including whether the body atom was positive
            or negative)
      - [ ] figure out the cycles in that graph. If there are any cycles
            including a negative edge, the graph cannot be stratified
            and should be rejected.
      - [ ] make a topological sort of the rules using the graph.
      - [ ] evaluate each layer of the graph in the normal way, building up
            an answer

-}


stratify : List Rule -> List (List Rule)
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
            [ rules ]

        Err condensation ->
            if List.any (Graph.edges >> List.map .label >> List.member Negative) condensation then
                -- this should be an error later
                []

            else
                List.concatMap
                    (Graph.nodes
                        >> List.filterMap (\{ label } -> Dict.get label rulesByName |> Maybe.map (Tuple.pair label))
                        >> Dict.fromList
                        >> Dict.values
                    )
                    condensation



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
            List.foldl evaluateRule database (List.concat rules)
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
