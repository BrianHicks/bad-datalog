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
    = StringType
    | IntType


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
        |> List.map fieldType
        |> Array.fromList


fieldType : Constant -> FieldType
fieldType constant =
    case constant of
        String _ ->
            StringType

        Int _ ->
            IntType


type Database
    = Database (Dict String Relation)


type Problem
    = SchemaMismatch
        { wanted : Schema
        , got : Schema
        }
    | RelationNotFound String
    | UnknownFields (List Field)
    | IncompatibleComparison FieldType FieldType


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
    | Project (List Field) QueryPlan
    | CrossProduct QueryPlan QueryPlan


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

        Project fields inputPlan ->
            let
                takeFields : Array a -> Array a
                takeFields arr =
                    List.filterMap
                        (\i -> Array.get i arr)
                        fields
                        |> Array.fromList
            in
            Result.andThen
                (\input ->
                    case List.filter (\i -> Array.get i input.schema == Nothing) fields of
                        [] ->
                            Ok
                                { schema = takeFields input.schema
                                , rows = List.map takeFields input.rows
                                }

                        unknownFields ->
                            Err (UnknownFields unknownFields)
                )
                (runPlan inputPlan db_)

        CrossProduct leftInputPlan rightInputPlan ->
            Result.map2
                (\left right ->
                    { schema = Array.append left.schema right.schema
                    , rows =
                        List.concatMap
                            (\leftRow ->
                                List.map
                                    (\rightRow -> Array.append leftRow rightRow)
                                    right.rows
                            )
                            left.rows
                    }
                )
                (runPlan leftInputPlan db_)
                (runPlan rightInputPlan db_)


type Selection
    = Predicate Field Op FieldOrConstant
    | Not Selection
    | And Selection Selection
    | Or Selection Selection


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
                    Err (UnknownFields [ field ])

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

        Or left right ->
            Result.map2 (||)
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
            Err (IncompatibleComparison (fieldType lConstant) (fieldType rConstant))
