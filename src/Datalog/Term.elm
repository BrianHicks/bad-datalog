module Datalog.Term exposing (Term(..), isGround, toString)


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


toString : Term -> String
toString term =
    case term of
        Constant constant ->
            "\"" ++ constant ++ "\""

        Variable variable ->
            variable
