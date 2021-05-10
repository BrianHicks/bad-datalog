module Database exposing
    ( Database, Relation, empty
    , Schema, FieldType(..)
    , Constant(..), insert, Problem(..)
    , read
    , QueryPlan(..), runPlan
    , Selection(..), Op(..), FieldOrConstant(..)
    )

{-| Some relational algebra stuff.

Resources:

  - <https://en.wikipedia.org/wiki/Relational_algebra>
  - <https://cs.uwaterloo.ca/~tozsu/courses/CS338/lectures/5%20Rel%20Algebra.pdf>
  - <https://www.cs.ubc.ca/~laks/cpsc304/Unit05-FormalLanguages.pdf>

@docs Database, Relation, empty

@docs Schema, FieldType

@docs Constant, insert, Problem

@docs read

@docs QueryPlan, runPlan

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


{-| -}
read : String -> Database -> Maybe (List (Array Constant))
read relationName (Database db) =
    Dict.get relationName db
        |> Maybe.map .rows


type QueryPlan
    = Read String
    | Select Selection QueryPlan
    | Project (List Field) QueryPlan
    | Join { left : QueryPlan, right : QueryPlan, fields : List ( Field, Field ) }


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
            Result.andThen
                (\input ->
                    Result.map
                        (\() ->
                            { schema = takeFields fields input.schema
                            , rows = List.map (takeFields fields) input.rows
                            }
                        )
                        (validateFieldsAreInSchema input.schema fields)
                )
                (runPlan inputPlan db_)

        Join config ->
            let
                runInput : QueryPlan -> List Field -> Result Problem Relation
                runInput input fields =
                    runPlan input db_
                        |> Result.andThen
                            (\relation ->
                                case validateFieldsAreInSchema relation.schema fields of
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
                (\left right ->
                    if takeFields leftFields left.schema /= takeFields rightFields right.schema then
                        Err
                            (SchemaMismatch
                                { wanted = takeFields leftFields left.schema
                                , got = takeFields rightFields right.schema
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
                                                    Just rows ->
                                                        Just (row :: rows)

                                                    Nothing ->
                                                        Just [ row ]
                                            )
                                    )
                                    (Sort.Dict.empty (arraySorter constantSorter))
                                    left.rows
                        in
                        Ok
                            { schema = Array.append left.schema right.schema
                            , rows =
                                List.concatMap
                                    (\rightRow ->
                                        case Sort.Dict.get (takeFields rightFields rightRow) leftIndex of
                                            Just rows ->
                                                List.map (\leftRow -> Array.append leftRow rightRow) rows

                                            Nothing ->
                                                []
                                    )
                                    right.rows
                            }
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
