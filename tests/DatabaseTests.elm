module DatabaseTests exposing (..)

import Database exposing (Database)
import Dict
import Expect
import Set
import Test exposing (..)


insertionTests : Test
insertionTests =
    describe "insertion"
        [ test "adds a row to the database" <|
            \_ ->
                Database.empty
                    |> Database.insert "human" socratesRow
                    -- !!! implementation details !!!
                    |> Result.map (Dict.get "human")
                    -- !!! implementation details !!!
                    |> Expect.equal (Ok (Just [ Dict.fromList socratesRow ]))
        , test "does not allow duplicate rows" <|
            \_ ->
                Database.empty
                    |> Database.insert "human" socratesRow
                    |> Result.andThen (Database.insert "human" socratesRow)
                    -- !!! implementation details !!!
                    |> Result.map (Dict.get "human")
                    -- !!! implementation details !!!
                    |> Expect.equal (Ok (Just [ Dict.fromList socratesRow ]))
        , test "does not allow the field names to vary" <|
            \_ ->
                Database.empty
                    |> Database.insert "pet" [ ( "name", Database.String "Axel" ) ]
                    |> Result.andThen (Database.insert "pet" [ ( "species", Database.String "Puffin" ) ])
                    |> Expect.equal (Err Database.IncompatibleFieldNames)
        ]


relationTests : Test
relationTests =
    describe "relations"
        [ describe "reading a table"
            [ test "from an empty database, returns an error" <|
                \_ ->
                    Database.empty
                        |> Database.runPlan (Database.ReadTable "human")
                        |> Expect.equal (Err (Database.TableDoesNotExist "human"))
            , test "from a non-empty database, returns the table" <|
                \_ ->
                    socratesDb
                        |> Database.runPlan (Database.ReadTable "human")
                        |> Expect.equal (Ok [ Dict.fromList [ ( "name", Database.String "Socrates" ) ] ])
            ]
        , describe "filtering a constant" <|
            [ test "keeps all rows matching the constant" <|
                \_ ->
                    socratesDb
                        |> Database.runPlan
                            (Database.ReadTable "human"
                                |> Database.FilterConstant { field = "name", constant = Database.String "Dave" }
                            )
                        |> Expect.equal (Ok [])
            , test "removes all rows not matching the constant" <|
                \_ ->
                    socratesDb
                        |> Database.runPlan
                            (Database.ReadTable "human"
                                |> Database.FilterConstant { field = "name", constant = Database.String "Socrates" }
                            )
                        |> Expect.equal (Ok [ Dict.fromList socratesRow ])
            ]
        , describe "projection"
            [ test "gets only the specified columns" <|
                \_ ->
                    Database.empty
                        |> unsafelyInsert "pet"
                            [ ( "name", Database.String "Axel" )
                            , ( "species", Database.String "Puffin" )
                            ]
                        |> Database.runPlan
                            (Database.ReadTable "pet"
                                |> Database.Project { fields = Set.fromList [ "name" ] }
                            )
                        |> Expect.equal (Ok [ Dict.fromList [ ( "name", Database.String "Axel" ) ] ])
            , test "raises an error if the column does not exist" <|
                \_ ->
                    socratesDb
                        |> Database.runPlan
                            (Database.ReadTable "human"
                                |> Database.Project { fields = Set.fromList [ "age" ] }
                            )
                        |> Expect.equal (Err (Database.FieldsDoNotExist (Set.singleton "age")))
            , test "raises an error if no fields are selected" <|
                \_ ->
                    socratesDb
                        |> Database.runPlan
                            (Database.ReadTable "human"
                                |> Database.Project { fields = Set.fromList [] }
                            )
                        |> Expect.equal (Err Database.ProjectedWithEmptyFieldSet)
            ]
        ]


socratesDb : Database
socratesDb =
    unsafelyInsert "human" socratesRow Database.empty


unsafelyInsert : String -> List ( String, Database.Constant ) -> Database -> Database
unsafelyInsert tableName row database =
    case Database.insert tableName row database of
        Ok db ->
            db

        Err problem ->
            Debug.todo
                ("There was a problem trying to insert `"
                    ++ Debug.toString row
                    ++ "` into the `"
                    ++ tableName
                    ++ "` table of `"
                    ++ Debug.toString database
                    ++ "`: "
                    ++ Debug.toString problem
                )


socratesRow : List ( String, Database.Constant )
socratesRow =
    [ ( "name", Database.String "Socrates" ) ]
