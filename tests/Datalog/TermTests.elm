module Datalog.TermTests exposing (..)

import Datalog.Term exposing (..)
import Expect
import Test exposing (..)


isGroundTest : Test
isGroundTest =
    describe "isGround"
        [ test "a constant is ground" <|
            \_ -> String "a" |> isGround |> Expect.equal True
        , test "a variable is not ground" <|
            \_ -> Variable "X" |> isGround |> Expect.equal False
        ]
