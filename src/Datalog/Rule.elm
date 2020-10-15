module Datalog.Rule exposing (Rule, body, fact, head, isFact, rule)

import Datalog.Atom as Atom exposing (Atom)


type Rule
    = Rule Atom (List Atom)


rule : Atom -> List Atom -> Rule
rule =
    Rule


fact : Atom -> Rule
fact fact_ =
    Rule fact_ []


isFact : Rule -> Bool
isFact (Rule head_ body_) =
    Atom.isGround head_ && List.isEmpty body_


head : Rule -> Atom
head (Rule head_ _) =
    head_


body : Rule -> List Atom
body (Rule _ body_) =
    body_
