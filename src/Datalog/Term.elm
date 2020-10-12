module Datalog.Term exposing (Constant(..), Term(..), Variable(..), int, isGround, string, toString, variable, variableSorter)

import Sort exposing (Sorter)


type Term
    = Constant Constant
    | Variable Variable


type Constant
    = String String
    | Int Int


type Variable
    = Named String


variableSorter : Sorter Variable
variableSorter =
    Sort.by (\(Named name) -> name) Sort.alphabetical


string : String -> Term
string =
    Constant << String


int : Int -> Term
int =
    Constant << Int


variable : String -> Term
variable =
    Variable << Named


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

        Variable (Named variable_) ->
            variable_
