module Database exposing
    ( Database, empty, insert, mergeRelations, replaceRelation
    , Relation, read, rows
    , Schema, FieldType(..)
    , Constant(..), Problem(..)
    , QueryPlan(..), query
    , Selection(..), Op(..), FieldOrConstant(..)
    , Field, Row
    )

{-| Some relational algebra stuff.

Resources:

  - <https://en.wikipedia.org/wiki/Relational_algebra>
  - <https://cs.uwaterloo.ca/~tozsu/courses/CS338/lectures/5%20Rel%20Algebra.pdf>
  - <https://www.cs.ubc.ca/~laks/cpsc304/Unit05-FormalLanguages.pdf>

@docs Database, empty, insert, mergeRelations, replaceRelation

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
import Sort.Set as Set exposing (Set)


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


rowSorter : Sorter Row
rowSorter =
    arraySorter constantSorter


type Relation
    = Relation Schema (Set Row)


rows : Relation -> List Row
rows (Relation _ rows_) =
    Set.toList rows_


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
                        (Set.singleton rowSorter (Array.fromList row))
                    )
                |> Database
                |> Ok

        Just (Relation schema rows_) ->
            if schema == rowToSchema row then
                db
                    |> Dict.insert relationName
                        (Relation schema (Set.insert (Array.fromList row) rows_))
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
mergeRelations : String -> Relation -> Database -> Result Problem Database
mergeRelations relationName (Relation schema newRows) (Database db) =
    case Dict.get relationName db of
        Just (Relation existingSchema existingRows) ->
            if schema /= existingSchema then
                Err (SchemaMismatch { wanted = existingSchema, got = schema })

            else
                Dict.insert
                    relationName
                    (Relation schema (Set.union rowSorter newRows existingRows))
                    db
                    |> Database
                    |> Ok

        Nothing ->
            db
                |> Dict.insert relationName (Relation schema newRows)
                |> Database
                |> Ok


replaceRelation : String -> Relation -> Database -> Database
replaceRelation relationName relation (Database db) =
    Database (Dict.insert relationName relation db)


{-| -}
read : String -> Database -> Result Problem Relation
read relationName db =
    query (Read relationName) db


type QueryPlan
    = Read String
    | Select Selection QueryPlan
    | Project (List Field) QueryPlan
    | JoinOn { left : QueryPlan, right : QueryPlan, fields : List ( Field, Field ) }
    | OuterJoin { keep : QueryPlan, drop : QueryPlan }
    | OuterJoinOn { keep : QueryPlan, drop : QueryPlan, fields : List ( Field, Field ) }


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
                        |> filterWithResult rowSorter (rowMatchesSelection selection)
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
                                (Set.map rowSorter (takeFields fields) rows_)
                        )
                        (validateFieldsAreInSchema schema fields)
                )
                (query inputPlan db_)

        JoinOn config ->
            let
                ( leftFields, rightFields ) =
                    List.unzip config.fields
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
                            leftIndex : Sort.Dict.Dict (Array Constant) (Set Row)
                            leftIndex =
                                Set.foldl
                                    (\row soFar ->
                                        Sort.Dict.update (takeFields leftFields row)
                                            (\maybeRows ->
                                                case maybeRows of
                                                    Just rows_ ->
                                                        Just (Set.insert row rows_)

                                                    Nothing ->
                                                        Just (Set.singleton rowSorter row)
                                            )
                                            soFar
                                    )
                                    (Sort.Dict.empty rowSorter)
                                    leftRows
                        in
                        Ok
                            (Relation
                                (Array.append leftSchema rightSchema)
                                (Set.foldl
                                    (\rightRow soFar ->
                                        case Sort.Dict.get (takeFields rightFields rightRow) leftIndex of
                                            Just rows_ ->
                                                Set.foldl
                                                    (\leftRow soFarInner -> Set.insert (Array.append leftRow rightRow) soFarInner)
                                                    soFar
                                                    rows_

                                            Nothing ->
                                                soFar
                                    )
                                    (Set.empty rowSorter)
                                    rightRows
                                )
                            )
                )
                (preparePlanForJoin config.left leftFields db_)
                (preparePlanForJoin config.right rightFields db_)
                |> Result.andThen identity

        OuterJoin { keep, drop } ->
            Result.map2
                (\(Relation keepSchema keepRows) (Relation dropSchema dropRows) ->
                    if keepSchema /= dropSchema then
                        Err
                            (SchemaMismatch
                                { wanted = keepSchema
                                , got = dropSchema
                                }
                            )

                    else
                        Ok (Relation keepSchema (Set.dropIf (Set.memberOf dropRows) keepRows))
                )
                (query keep db_)
                (query drop db_)
                |> Result.andThen identity

        OuterJoinOn config ->
            let
                ( keepFields, dropFields ) =
                    List.unzip config.fields
            in
            Result.map2
                (\(Relation keepSchema keepRows) (Relation dropSchema dropRows) ->
                    if takeFields keepFields keepSchema /= takeFields dropFields dropSchema then
                        Err
                            (SchemaMismatch
                                { wanted = takeFields keepFields keepSchema
                                , got = takeFields dropFields dropSchema
                                }
                            )

                    else
                        let
                            toDrop : Set (Array Constant)
                            toDrop =
                                Set.foldl
                                    (\row soFar -> Set.insert (takeFields dropFields row) soFar)
                                    (Set.empty rowSorter)
                                    dropRows
                        in
                        Relation
                            keepSchema
                            (Set.dropIf
                                (\keepRow -> Set.memberOf toDrop (takeFields keepFields keepRow))
                                keepRows
                            )
                            |> Ok
                )
                (preparePlanForJoin config.keep keepFields db_)
                (preparePlanForJoin config.drop dropFields db_)
                |> Result.andThen identity


preparePlanForJoin : QueryPlan -> List Field -> Database -> Result Problem Relation
preparePlanForJoin input fields db =
    query input db
        |> Result.andThen
            (\((Relation schema _) as relation) ->
                case validateFieldsAreInSchema schema fields of
                    Ok _ ->
                        Ok relation

                    Err err ->
                        Err err
            )


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


filterWithResult : Sorter a -> (a -> Result problem Bool) -> Set a -> Result problem (Set a)
filterWithResult sorter filter input =
    filterWithResultHelp (Set.empty sorter) filter (Set.toList input)


filterWithResultHelp : Set a -> (a -> Result problem Bool) -> List a -> Result problem (Set a)
filterWithResultHelp done filter todo =
    case todo of
        [] ->
            Ok done

        next :: rest ->
            case filter next of
                Ok True ->
                    filterWithResultHelp (Set.insert next done) filter rest

                Ok False ->
                    filterWithResultHelp done filter rest

                Err problem ->
                    Err problem


rowMatchesSelection : Selection -> Row -> Result Problem Bool
rowMatchesSelection selection row =
    let
        lookup : Field -> Result Problem Constant
        lookup field =
            case Array.get field row of
                Just constant ->
                    Ok constant

                Nothing ->
                    Err (UnknownFields [ field ])

        resolve : FieldOrConstant -> Result Problem Constant
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
