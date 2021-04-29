module Relational exposing (..)

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
insert : String -> Array Constant -> Database -> Result () Database
insert tableName row db =
    Dict.update tableName
        (\maybeTable ->
            case maybeTable of
                Just table ->
                    Just (row :: table)

                Nothing ->
                    Just [ row ]
        )
        db
        |> Ok
