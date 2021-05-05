module List.ExtraTests exposing (..)

import Expect
import Fuzz
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


foldrResultTests : Test
foldrResultTests =
    describe "foldrResult"
        [ test "works like a foldr when the function always returns OK" <|
            \_ ->
                foldrResult (\a b -> Ok (a + b)) 0 [ 1, 2, 3 ]
                    |> Expect.equal (Ok (List.foldr (+) 0 [ 1, 2, 3 ]))
        , test "returns an error when the provided function does" <|
            \_ ->
                foldrResult (\_ _ -> Err ()) () [ 1 ]
                    |> Expect.err
        , fuzz Fuzz.int "always returns the initial value for an empty list" <|
            \initial ->
                foldrResult (\_ _ -> Err ()) initial []
                    |> Expect.equal (Ok initial)
        ]
