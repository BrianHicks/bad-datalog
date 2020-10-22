module Datalog.Rule exposing (Problem(..), Rule, body, fact, head, isFact, rule, toString)

import Datalog.Atom as Atom exposing (Atom)
import Datalog.Negatable as Negatable exposing (Negatable)
import Datalog.Term as Term


type Rule
    = Rule Atom (List (Negatable Atom))


type Problem
    = NotRangeRestricted
    | UnnamedHeadVariable


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
            List.concatMap (Negatable.unwrap >> Atom.variables) body_
    in
    List.all
        (\headVar -> List.member headVar bodyVars)
        (Atom.variables head_)


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
                            if Negatable.isPositive negatableAtom then
                                Atom.toString (Negatable.unwrap negatableAtom)

                            else
                                "not " ++ Atom.toString (Negatable.unwrap negatableAtom)
                        )
                        body_
                    )
                ++ "."
