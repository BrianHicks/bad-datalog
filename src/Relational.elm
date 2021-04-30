module Relational exposing (Constant(..), Database, Field(..), Problem(..), QueryPlan(..), Relation, Schema, empty, insert, runPlan)

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


type Field
    = StringField
    | IntField


type alias Relation =
    { schema : Schema
    , rows : List (Array Constant)
    }


type alias Schema =
    List Field


rowToSchema : List Constant -> Schema
rowToSchema row =
    List.map
        (\value ->
            case value of
                String _ ->
                    StringField

                Int _ ->
                    IntField
        )
        row


type Database
    = Database (Dict String Relation)


type Problem
    = SchemaMismatch
        { wanted : Schema
        , got : Schema
        }
    | RelationNotFound String


empty : Database
empty =
    Database Dict.empty


{-| -}
insert : String -> List Constant -> Database -> Result Problem Database
insert relationName row (Database db) =
    case Dict.get relationName db of
        Nothing ->
            db
                |> Dict.insert relationName
                    { schema = rowToSchema row
                    , rows = [ Array.fromList row ]
                    }
                |> Database
                |> Ok

        Just relation ->
            if relation.schema == rowToSchema row then
                db
                    |> Dict.insert relationName
                        { schema = relation.schema
                        , rows = Array.fromList row :: relation.rows
                        }
                    |> Database
                    |> Ok

            else
                Err
                    (SchemaMismatch
                        { wanted = relation.schema
                        , got = rowToSchema row
                        }
                    )


type QueryPlan
    = Read String


runPlan : QueryPlan -> Database -> Result Problem Relation
runPlan plan (Database db) =
    case plan of
        Read relationName ->
            case Dict.get relationName db of
                Just relation ->
                    Ok relation

                Nothing ->
                    Err (RelationNotFound relationName)
