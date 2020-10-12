module Datalog.TermTests exposing (..)

import Datalog.Term exposing (..)
import Expect
import Test exposing (..)


isGroundTest : Test
isGroundTest =
    describe "isGround"
        [ test "a string is ground" <|
            \_ -> string "a" |> isGround |> Expect.equal True
        , test "an integer is ground" <|
            \_ -> int 1 |> isGround |> Expect.equal True
        , test "a variable is not ground" <|
            \_ -> variable "X" |> isGround |> Expect.equal False
        ]
