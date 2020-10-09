module Datalog.Term exposing (Term(..), isGround)


type Term
    = Constant String
    | Variable String


isGround : Term -> Bool
isGround term =
    case term of
        Constant _ ->
            True

        Variable _ ->
            False
