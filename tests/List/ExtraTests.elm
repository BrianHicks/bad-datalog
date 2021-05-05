module List.ExtraTests exposing (..)

import Expect
import List.Extra exposing (..)
import Test exposing (..)


indexOfTests : Test
indexOfTests =
    describe "indexOf"
        [ test "gives the index when an item is in the list" <|
            \_ ->
                [ 1 ]
                    |> indexOf 1
                    |> Expect.equal (Just 0)
        , test "gives the first index when the item is present multiple times" <|
            \_ ->
                [ 1, 1 ]
                    |> indexOf 1
                    |> Expect.equal (Just 0)
        , test "does not give an index if the item isn't in the list" <|
            \_ ->
                [ 1 ]
                    |> indexOf 2
                    |> Expect.equal Nothing
        ]
