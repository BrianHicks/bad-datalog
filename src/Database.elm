module Database exposing (..)

import Dict exposing (Dict)
import Set exposing (Set)
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
                        -- TODO: is this OK? The allowable field names of an
                        -- empty relation are just never checked. This would go
                        -- away if we would explicitly store the field names
                        -- on the first insert, but that causes all kinds of
                        -- headaches up and down the chain. It seems like it
                        -- might be worth it eventually, though?
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


listSorter : Sorter a -> Sorter (List a)
listSorter inner =
    let
        sortPairs : List a -> List a -> Order
        sortPairs a b =
            case ( a, b ) of
                ( [], [] ) ->
                    EQ

                ( _, [] ) ->
                    GT

                ( [], _ ) ->
                    LT

                ( lFirst :: lRest, rFirst :: rRest ) ->
                    case Sort.toOrder inner lFirst rFirst of
                        EQ ->
                            sortPairs lRest rRest

                        otherwise ->
                            otherwise
    in
    Sort.custom sortPairs
