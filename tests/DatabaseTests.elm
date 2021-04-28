module DatabaseTests exposing (..)

import Array
import Database
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
                    Database.empty
                        |> Database.insert "mortal" [ Database.String "Socrates" ]
                        |> Database.runPlan (Database.ReadTable "mortal")
                        |> Expect.equal (Ok [ Array.fromList [ Database.String "Socrates" ] ])
            ]
        ]
