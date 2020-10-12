module Datalog.Term exposing (Term(..), isGround, toString)


type Term
    = String String
    | Int Int
    | Variable String


isGround : Term -> Bool
isGround term =
    case term of
        String _ ->
            True

        Int _ ->
            True

        Variable _ ->
            False


toString : Term -> String
toString term =
    case term of
        String constant ->
            "\"" ++ constant ++ "\""

        Int int ->
            String.fromInt int

        Variable variable ->
            variable
