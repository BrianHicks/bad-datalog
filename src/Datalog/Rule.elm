module Datalog.Rule exposing (Problem(..), Rule, body, fact, head, isFact, rule)

import Datalog.Atom as Atom exposing (Atom)
import Datalog.Term as Term


type Rule
    = Rule Atom (List Atom)


type Problem
    = NotRangeRestricted


rule : Atom -> List Atom -> Result Problem Rule
rule head_ body_ =
    let
        candidate =
            Rule head_ body_
    in
    if isRangeRestricted candidate then
        Ok candidate

    else
        Err NotRangeRestricted


fact : Atom -> Result Problem Rule
fact fact_ =
    rule fact_ []


isFact : Rule -> Bool
isFact (Rule head_ body_) =
    Atom.isGround head_ && List.isEmpty body_


{-| Do all the variables in the head occur in the body?
-}
isRangeRestricted : Rule -> Bool
isRangeRestricted (Rule head_ body_) =
    let
        headVars =
            Atom.variables head_

        bodyVars =
            List.concatMap Atom.variables body_

        noAnonymousHead =
            not (List.any Term.isAnonymous headVars)

        rangeRestrictedBody =
            List.all
                (\headVar -> List.member headVar bodyVars)
                headVars
    in
    noAnonymousHead && rangeRestrictedBody


head : Rule -> Atom
head (Rule head_ _) =
    head_


body : Rule -> List Atom
body (Rule _ body_) =
    body_
