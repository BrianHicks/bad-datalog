module Datalog exposing (..)

import Database exposing (Constant)


type Rule
    = Rule Atom (List Atom)


type Atom
    = Atom String (List Term)


type Term
    = Variable String
    | Constant Constant
