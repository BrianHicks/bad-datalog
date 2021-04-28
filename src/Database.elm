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


type InsertProblem
    = IncompatibleFieldNames
    | DuplicateRow


insert : String -> List ( String, Constant ) -> Database -> Result InsertProblem Database
insert tableName row database =
    let
        rowDict =
            Dict.fromList row
    in
    case Dict.get tableName database of
        Nothing ->
            Ok (Dict.insert tableName [ rowDict ] database)

        Just [] ->
            Ok (Dict.insert tableName [ rowDict ] database)

        Just ((firstRow :: _) as rows) ->
            if Dict.keys firstRow /= Dict.keys rowDict then
                Err IncompatibleFieldNames

            else if List.member rowDict rows then
                Err DuplicateRow

            else
                Ok (Dict.insert tableName (rowDict :: rows) database)


type QueryPlan
    = ReadTable String
    | FilterConstant { field : String, constant : Constant } QueryPlan
    | Project { fields : Set String } QueryPlan


type Problem
    = TableDoesNotExist String
    | FieldsDoNotExist (Set String)
    | ProjectedWithEmptyFieldSet


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
            if Set.isEmpty fields then
                Err ProjectedWithEmptyFieldSet

            else
                case runPlan input database of
                    Err problem ->
                        Err problem

                    Ok [] ->
                        Ok []

                    Ok ((first :: _) as rows) ->
                        let
                            missingFields =
                                Set.diff fields (Set.fromList (Dict.keys first))
                        in
                        if not (Set.isEmpty missingFields) then
                            Err (FieldsDoNotExist missingFields)

                        else
                            rows
                                |> List.map (Dict.filter (\field _ -> Set.member field fields))
                                |> Ok
