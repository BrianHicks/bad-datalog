module Relational exposing (Constant(..), Database, FieldOrConstant(..), FieldType(..), Op(..), Problem(..), QueryPlan(..), Relation, Schema, Selection(..), empty, insert, runPlan)

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


type FieldType
    = StringField
    | IntField


type alias Row =
    Array Constant


type alias Relation =
    { schema : Schema
    , rows : List Row
    }


type alias Schema =
    Array FieldType


rowToSchema : List Constant -> Schema
rowToSchema row =
    row
        |> List.map
            (\value ->
                case value of
                    String _ ->
                        StringField

                    Int _ ->
                        IntField
            )
        |> Array.fromList


type Database
    = Database (Dict String Relation)


type Problem
    = SchemaMismatch
        { wanted : Schema
        , got : Schema
        }
    | RelationNotFound String
    | UnknownField Field
    | IncompatibleComparison Constant Constant


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
    | Select Selection QueryPlan


runPlan : QueryPlan -> Database -> Result Problem Relation
runPlan plan ((Database db) as db_) =
    case plan of
        Read relationName ->
            case Dict.get relationName db of
                Just relation ->
                    Ok relation

                Nothing ->
                    Err (RelationNotFound relationName)

        Select selection inputPlan ->
            Result.andThen
                (\input ->
                    input.rows
                        |> filterWithResult (rowMatchesSelection selection)
                        |> Result.map (Relation input.schema)
                )
                (runPlan inputPlan db_)


type Selection
    = Predicate Field Op FieldOrConstant
    | Not Selection
    | And Selection Selection



-- | Or Selection Selection


type alias Field =
    Int


type FieldOrConstant
    = Field Field
    | Constant Constant


type Op
    = Eq
    | Gt
    | Lt


filterWithResult : (a -> Result problem Bool) -> List a -> Result problem (List a)
filterWithResult =
    filterWithResultHelp []


filterWithResultHelp : List a -> (a -> Result problem Bool) -> List a -> Result problem (List a)
filterWithResultHelp done filter todo =
    case todo of
        [] ->
            Ok (List.reverse done)

        next :: rest ->
            case filter next of
                Ok True ->
                    filterWithResultHelp (next :: done) filter rest

                Ok False ->
                    filterWithResultHelp done filter rest

                Err problem ->
                    Err problem


rowMatchesSelection : Selection -> Row -> Result Problem Bool
rowMatchesSelection selection row =
    let
        lookup field =
            case Array.get field row of
                Just constant ->
                    Ok constant

                Nothing ->
                    Err (UnknownField field)

        resolve fieldOrConstant =
            case fieldOrConstant of
                Field field ->
                    lookup field

                Constant constant ->
                    Ok constant
    in
    case selection of
        Predicate lRaw op rRaw ->
            Result.map2
                (applyOp op)
                (lookup lRaw)
                (resolve rRaw)
                |> Result.andThen identity

        Not inner ->
            rowMatchesSelection inner row
                |> Result.map not

        And left right ->
            Result.map2 (&&)
                (rowMatchesSelection left row)
                (rowMatchesSelection right row)


applyOp : Op -> Constant -> Constant -> Result Problem Bool
applyOp op lConstant rConstant =
    case ( lConstant, rConstant ) of
        ( String l, String r ) ->
            case op of
                Eq ->
                    Ok (l == r)

                Gt ->
                    Ok (l > r)

                Lt ->
                    Ok (l < r)

        ( Int l, Int r ) ->
            case op of
                Eq ->
                    Ok (l == r)

                Gt ->
                    Ok (l > r)

                Lt ->
                    Ok (l < r)

        _ ->
            Err (IncompatibleComparison lConstant rConstant)
