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
                        |> Database.runPlan (Database.ReadTable "mortal")
                        |> Expect.equal (Err (Database.TableDoesNotExist "mortal"))
            , test "from a non-empty database, returns the table" <|
                \_ ->
                    socratesDb
                        |> Database.runPlan (Database.ReadTable "mortal")
                        |> Expect.equal (Ok [ Dict.fromList [ ( "name", Database.String "Socrates" ) ] ])
            ]
        , describe "filtering a constant" <|
            [ test "removes all rows matching the constant" <|
                \_ ->
                    socratesDb
                        |> Database.runPlan
                            (Database.ReadTable "mortal"
                                |> Database.FilterConstant { field = "name", constant = Database.String "Socrates" }
                            )
                        |> Expect.equal (Ok [])
            , test "keeps all rows not matching the constant" <|
                \_ ->
                    socratesDb
                        |> Database.runPlan
                            (Database.ReadTable "mortal"
                                |> Database.FilterConstant { field = "name", constant = Database.String "Dave" }
                            )
                        |> Expect.equal (Ok [ Dict.fromList socratesRow ])
            ]
        ]


socratesDb : Database
socratesDb =
    Database.empty
        |> Database.insert "mortal" socratesRow


socratesRow : List ( String, Database.Constant )
socratesRow =
    [ ( "name", Database.String "Socrates" ) ]
