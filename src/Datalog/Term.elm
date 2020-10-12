module Datalog.Term exposing (Constant(..), Term(..), int, isGround, string, toString, variable)


type Term
    = Constant Constant
    | Variable String


type Constant
    = String String
    | Int Int


string : String -> Term
string =
    Constant << String


int : Int -> Term
int =
    Constant << Int


variable : String -> Term
variable =
    Variable


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
        Constant (String string_) ->
            "\"" ++ string_ ++ "\""

        Constant (Int int_) ->
            String.fromInt int_

        Variable variable_ ->
            variable_
