module DatalogTest exposing (..)

import Datalog exposing (..)
import Expect
import Test exposing (..)


smokeTest : Test
smokeTest =
    describe "smoke tests (with the academic ancestor program)"
        [ test "finds intermediate steps in the recursive academicAncestor query" <|
            \_ ->
                ancestorProgram
                    |> query
                        [ Var "Intermediate" ]
                        [ Atom "academicAncestor" [ Sym "Robin Milner", Var "Intermediate" ]
                        , Atom "academicAncestor" [ Var "Intermediate", Sym "Mistral Contrastin" ]
                        ]
                    |> Expect.equal
                        [ [ Sym "Alan Mycroft" ]
                        , [ Sym "Dominic Orchard" ]
                        ]
        , test "does not find any connection between Alan Turing and Mistral Contrastin" <|
            \_ ->
                ancestorProgram
                    |> query []
                        [ Atom "academicAncestor" [ Sym "Alan Turing", Sym "Mistral Contrastin" ] ]
                    |> Expect.equal []
        , test "finds a connection between David Wheeler and Mistral Contrastin" <|
            \_ ->
                ancestorProgram
                    |> query []
                        [ Atom "academicAncestor" [ Sym "David Wheeler", Sym "Mistral Contrastin" ] ]
                    |> Expect.equal [ [] ]
        ]


ancestorProgram : Datalog.Program
ancestorProgram =
    [ -- Facts
      adviser "Andrew Rice" "Mistral Contrastin"
    , adviser "Dominic Orchard" "Mistral Contrastin"
    , adviser "Andy Hopper" "Andrew Rice"
    , adviser "Alan Mycroft" "Dominic Orchard"
    , adviser "David Wheeler" "Andy Hopper"
    , adviser "Rod Burstall" "Andy Mycroft"
    , adviser "Robin Milner" "Alan Mycroft"

    -- Rules
    , Rule (Atom "academicAncestor" [ Var "X", Var "Y" ])
        [ Atom "adviser" [ Var "X", Var "Y" ] ]
    , Rule (Atom "academicAncestor" [ Var "X", Var "Z" ])
        [ Atom "adviser" [ Var "X", Var "Y" ]
        , Atom "academicAncestor" [ Var "Y", Var "Z" ]
        ]

    -- Queries
    , Rule (Atom "query2" [])
        [ Atom "academicAncestor" [ Sym "Alan Turing", Sym "Mistral Contrastin" ] ]
    , Rule (Atom "query3" [])
        [ Atom "academicAncestor" [ Sym "David Wheeler", Sym "Mistral Contrastin" ] ]
    ]


adviser : String -> String -> Rule
adviser a b =
    Rule (Atom "adviser" [ Sym a, Sym b ]) []
