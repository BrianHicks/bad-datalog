module Database exposing
    ( Database, empty, insert, insertRelation
    , Relation, read, rows
    , Schema, FieldType(..)
    , Constant(..), Problem(..)
    , QueryPlan(..), query
    , Selection(..), Op(..), FieldOrConstant(..)
    )

{-| Some relational algebra stuff.

Resources:

  - <https://en.wikipedia.org/wiki/Relational_algebra>
  - <https://cs.uwaterloo.ca/~tozsu/courses/CS338/lectures/5%20Rel%20Algebra.pdf>
  - <https://www.cs.ubc.ca/~laks/cpsc304/Unit05-FormalLanguages.pdf>

@docs Database, empty, insert, insertRelation

@docs Relation, read, rows

@docs Schema, FieldType

@docs Constant, Problem

@docs read

@docs QueryPlan, query

@docs Selection, Op, FieldOrConstant

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Sort exposing (Sorter)
import Sort.Dict


type Constant
    = String String
    | Int Int


constantSorter : Sorter Constant
constantSorter =
    Sort.custom
        (\a b ->
            case ( a, b ) of
                ( String _, Int _ ) ->
                    LT

                ( Int _, String _ ) ->
                    GT

                ( String s1, String s2 ) ->
                    compare s1 s2

                ( Int i1, Int i2 ) ->
                    compare i1 i2
        )


type FieldType
    = StringType
    | IntType


type alias Row =
    Array Constant


type Relation
    = Relation Schema (List Row)


rows : Relation -> List Row
rows (Relation _ rows_) =
    rows_


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
                    (Relation
                        (rowToSchema row)
                        [ Array.fromList row ]
                    )
                |> Database
                |> Ok

        Just (Relation schema rows_) ->
            if schema == rowToSchema row then
                db
                    |> Dict.insert relationName
                        (Relation
                            schema
                            (if List.member (Array.fromList row) rows_ then
                                rows_

                             else
                                Array.fromList row :: rows_
                            )
                        )
                    |> Database
                    |> Ok

            else
                Err
                    (SchemaMismatch
                        { wanted = schema
                        , got = rowToSchema row
                        }
                    )


{-| -}
insertRelation : String -> Relation -> Database -> Result Problem Database
insertRelation relationName (Relation schema newRows) (Database db) =
    case Dict.get relationName db of
        Just (Relation existingSchema existingRows) ->
            if schema /= existingSchema then
                Err (SchemaMismatch { wanted = existingSchema, got = schema })

            else
                Dict.insert relationName
                    (Relation schema
                        -- TODO: WOOF this is gonna be slow, and we're
                        -- doing it all the time in the datalog query
                        -- implementation. Benchmark and see if a `Set` gives
                        -- a huge speed boost!
                        (List.filter
                            (\row -> not (List.member row existingRows))
                            newRows
                            ++ existingRows
                        )
                    )
                    db
                    |> Database
                    |> Ok

        Nothing ->
            db
                |> Dict.insert relationName (Relation schema newRows)
                |> Database
                |> Ok


{-| -}
read : String -> Database -> Result Problem Relation
read relationName db =
    query (Read relationName) db


type QueryPlan
    = Read String
    | Select Selection QueryPlan
    | Project (List Field) QueryPlan
    | Join { left : QueryPlan, right : QueryPlan, fields : List ( Field, Field ) }


query : QueryPlan -> Database -> Result Problem Relation
query plan ((Database db) as db_) =
    case plan of
        Read relationName ->
            case Dict.get relationName db of
                Just relation ->
                    Ok relation

                Nothing ->
                    Err (RelationNotFound relationName)

        Select selection inputPlan ->
            Result.andThen
                (\(Relation schema rows_) ->
                    rows_
                        |> filterWithResult (rowMatchesSelection selection)
                        |> Result.map (Relation schema)
                )
                (query inputPlan db_)

        Project fields inputPlan ->
            Result.andThen
                (\(Relation schema rows_) ->
                    Result.map
                        (\() ->
                            Relation
                                (takeFields fields schema)
                                (List.map (takeFields fields) rows_)
                        )
                        (validateFieldsAreInSchema schema fields)
                )
                (query inputPlan db_)

        Join config ->
            let
                runInput : QueryPlan -> List Field -> Result Problem Relation
                runInput input fields =
                    query input db_
                        |> Result.andThen
                            (\((Relation schema _) as relation) ->
                                case validateFieldsAreInSchema schema fields of
                                    Ok _ ->
                                        Ok relation

                                    Err err ->
                                        Err err
                            )

                leftFields =
                    List.map Tuple.first config.fields

                rightFields =
                    List.map Tuple.second config.fields
            in
            Result.map2
                (\(Relation leftSchema leftRows) (Relation rightSchema rightRows) ->
                    if takeFields leftFields leftSchema /= takeFields rightFields rightSchema then
                        Err
                            (SchemaMismatch
                                { wanted = takeFields leftFields leftSchema
                                , got = takeFields rightFields rightSchema
                                }
                            )

                    else
                        let
                            leftIndex =
                                List.foldl
                                    (\row ->
                                        Sort.Dict.update (takeFields leftFields row)
                                            (\maybeRows ->
                                                case maybeRows of
                                                    Just rows_ ->
                                                        Just (row :: rows_)

                                                    Nothing ->
                                                        Just [ row ]
                                            )
                                    )
                                    (Sort.Dict.empty (arraySorter constantSorter))
                                    leftRows
                        in
                        Ok
                            (Relation
                                (Array.append leftSchema rightSchema)
                                (List.concatMap
                                    (\rightRow ->
                                        case Sort.Dict.get (takeFields rightFields rightRow) leftIndex of
                                            Just rows_ ->
                                                List.map (\leftRow -> Array.append leftRow rightRow) rows_

                                            Nothing ->
                                                []
                                    )
                                    rightRows
                                )
                            )
                )
                (runInput config.left leftFields)
                (runInput config.right rightFields)
                |> Result.andThen identity


validateFieldsAreInSchema : Schema -> List Int -> Result Problem ()
validateFieldsAreInSchema schema fields =
    case List.filter (\field -> Array.get field schema == Nothing) fields of
        [] ->
            Ok ()

        unknownFields ->
            Err (UnknownFields unknownFields)


takeFields : List Int -> Array a -> Array a
takeFields fields row =
    fields
        |> List.filterMap (\field -> Array.get field row)
        |> Array.fromList


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


arraySorter : Sorter a -> Sorter (Array a)
arraySorter inner =
    let
        sortPairs : Int -> Array a -> Array a -> Order
        sortPairs index a b =
            case ( Array.get index a, Array.get index b ) of
                ( Nothing, Nothing ) ->
                    EQ

                ( Just _, Nothing ) ->
                    GT

                ( Nothing, Just _ ) ->
                    LT

                ( Just left, Just right ) ->
                    case Sort.toOrder inner left right of
                        EQ ->
                            sortPairs (index + 1) a b

                        otherwise ->
                            otherwise
    in
    Sort.custom (sortPairs 0)
