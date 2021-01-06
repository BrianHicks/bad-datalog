module Datalog.Rule exposing (Problem(..), Rule, body, fact, head, isFact, rule, toString)

import Datalog.Atom as Atom exposing (Atom)
import Datalog.Negatable as Negatable exposing (Direction(..), Negatable(..))
import Datalog.Term as Term
import Sort.Set as Set


type Rule
    = Rule Atom (List (Negatable Atom))


type Problem
    = NotRangeRestricted
    | UnnamedHeadVariable
    | VariableAppearsNegatedButNotPositive


rule : Atom -> List (Negatable Atom) -> Result Problem Rule
rule head_ body_ =
    let
        candidate =
            Rule head_ body_
    in
    if hasUnnamedHeadVariable candidate then
        Err UnnamedHeadVariable

    else if not (isRangeRestricted candidate) then
        Err NotRangeRestricted

    else if not (isNegationSafe candidate) then
        Err VariableAppearsNegatedButNotPositive

    else
        Ok candidate


fact : Atom -> Result Problem Rule
fact fact_ =
    rule fact_ []


isFact : Rule -> Bool
isFact (Rule head_ body_) =
    Atom.isGround head_ && List.isEmpty body_


hasUnnamedHeadVariable : Rule -> Bool
hasUnnamedHeadVariable (Rule head_ _) =
    List.any Term.isAnonymous (Atom.variables head_)


{-| Do all the variables in the head occur in the body?
-}
isRangeRestricted : Rule -> Bool
isRangeRestricted (Rule head_ body_) =
    let
        bodyVars =
            List.concatMap (Negatable.value >> Atom.variables) body_
    in
    List.all
        (\headVar -> List.member headVar bodyVars)
        (Atom.variables head_)


{-| Do all the variables in negated expressions also appear in positive
expressions?
-}
isNegationSafe : Rule -> Bool
isNegationSafe (Rule _ body_) =
    body_
        |> List.foldl
            (\(Negatable direction atom) occurrences_ ->
                List.foldl
                    (\variable occurrences ->
                        case direction of
                            Positive ->
                                { occurrences | positive = Set.insert variable occurrences.positive }

                            Negative ->
                                { occurrences | negative = Set.insert variable occurrences.negative }
                    )
                    occurrences_
                    (Atom.variables atom)
            )
            { positive = Set.empty Term.variableSorter
            , negative = Set.empty Term.variableSorter
            }
        |> (\{ positive, negative } ->
                negative
                    |> Set.dropIf (Set.memberOf positive)
                    |> Set.dropIf ((==) Term.Anonymous)
                    |> Set.isEmpty
           )


head : Rule -> Atom
head (Rule head_ _) =
    head_


body : Rule -> List (Negatable Atom)
body (Rule _ body_) =
    body_


toString : Rule -> String
toString (Rule head_ body_) =
    case body_ of
        [] ->
            Atom.toString head_ ++ "."

        _ ->
            Atom.toString head_
                ++ " :- "
                ++ String.join ", "
                    (List.map
                        (\negatableAtom ->
                            case negatableAtom of
                                Negatable Positive atom ->
                                    Atom.toString atom

                                Negatable Negative atom ->
                                    "not " ++ Atom.toString atom
                        )
                        body_
                    )
                ++ "."
