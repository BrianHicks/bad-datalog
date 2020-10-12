module Datalog.Term exposing (Term(..), isGround, toString)


type Term
    = String String
    | Variable String


isGround : Term -> Bool
isGround term =
    case term of
        String _ ->
            True

        Variable _ ->
            False


toString : Term -> String
toString term =
    case term of
        String constant ->
            "\"" ++ constant ++ "\""

        Variable variable ->
            variable
