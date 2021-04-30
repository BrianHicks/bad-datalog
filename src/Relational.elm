module Relational exposing (..)

{-| Some relational algebra stuff.

Resources:

  - <https://en.wikipedia.org/wiki/Relational_algebra>
  - <https://cs.uwaterloo.ca/~tozsu/courses/CS338/lectures/5%20Rel%20Algebra.pdf>
  - <https://www.cs.ubc.ca/~laks/cpsc304/Unit05-FormalLanguages.pdf>

-}

import Array exposing (Array)
import Dict exposing (Dict)


type Constant
    = String String
    | Int Int


type alias Table =
    List (Array Constant)


type alias Database =
    Dict String Table


empty : Database
empty =
    Dict.empty


{-| -}
insert : String -> List Constant -> Database -> Result () Database
insert tableName row db =
    Dict.update tableName
        (\maybeTable ->
            case maybeTable of
                Just table ->
                    Just (Array.fromList row :: table)

                Nothing ->
                    Just [ Array.fromList row ]
        )
        db
        |> Ok
