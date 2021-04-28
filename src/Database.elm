module Database exposing (..)

import Dict exposing (Dict)
import Set exposing (Set)


type Constant
    = String String
    | Int Int


type alias Relation =
    List (Dict String Constant)


type alias Database =
    Dict String Relation


empty : Database
empty =
    Dict.empty


insert : String -> List ( String, Constant ) -> Database -> Database
insert tableName row database =
    let
        rowArr =
            Dict.fromList row
    in
    Dict.update tableName
        (\existing ->
            case existing of
                Just rows ->
                    -- set semantics without having to do comparable tricks
                    -- to turn this into an actual set. Iffy but... fine.
                    if List.member rowArr rows then
                        Just rows

                    else
                        Just (rowArr :: rows)

                Nothing ->
                    Just [ rowArr ]
        )
        database


type QueryPlan
    = ReadTable String
    | FilterConstant { field : String, constant : Constant } QueryPlan
    | Project { fields : Set String } QueryPlan


type Problem
    = TableDoesNotExist String
    | FieldsDoNotExist (Set String)


runPlan : QueryPlan -> Database -> Result Problem Relation
runPlan plan database =
    case plan of
        ReadTable tableName ->
            case Dict.get tableName database of
                Just relation ->
                    Ok relation

                Nothing ->
                    Err (TableDoesNotExist tableName)

        FilterConstant { field, constant } input ->
            Result.map
                (List.filter (\row -> Dict.get field row == Just constant))
                (runPlan input database)

        Project { fields } input ->
            Result.andThen
                (List.foldl
                    (\row rowsResult ->
                        case rowsResult of
                            Ok rows ->
                                let
                                    projectedRow =
                                        Dict.filter (\field _ -> Set.member field fields) row

                                    missingFields =
                                        Set.diff fields (Set.fromList (Dict.keys projectedRow))
                                in
                                if Set.isEmpty missingFields then
                                    Ok (projectedRow :: rows)

                                else
                                    Err (FieldsDoNotExist missingFields)

                            _ ->
                                rowsResult
                    )
                    (Ok [])
                )
                (runPlan input database)
