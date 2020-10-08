module Datalog.Term exposing (Term(..))


type Term
    = Constant String
    | Variable String
