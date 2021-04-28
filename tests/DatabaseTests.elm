module DatabaseTests exposing (..)

import Database exposing (Database)
import Dict
import Expect
import Test exposing (..)


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
        ]


socratesDb : Database
socratesDb =
    Database.empty
        |> Database.insert "human" socratesRow


socratesRow : List ( String, Database.Constant )
socratesRow =
    [ ( "name", Database.String "Socrates" ) ]
