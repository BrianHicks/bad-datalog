module Datalog exposing (..)

import Sort exposing (Sorter)


type Program
    = Program (List Rule)


type Rule
    = Rule Atom (List Atom)


type Atom
    = Atom String (List Term)


type Term
    = Constant String
    | Variable String


{-| This is cheating a bit. A database is only ground atoms--that is, atoms
whose terms are all constants.
-}
type Database
    = Database (List Atom)



-- EXAMPLES


allPairsReachability : Program
allPairsReachability =
    Program
        [ -- base data
          Rule (Atom "link" [ Constant "a", Constant "b" ]) []
        , Rule (Atom "link" [ Constant "b", Constant "c" ]) []
        , Rule (Atom "link" [ Constant "c", Constant "c" ]) []
        , Rule (Atom "link" [ Constant "c", Constant "d" ]) []

        -- derivations
        , Rule
            (Atom "reachable" [ Variable "X", Variable "Y" ])
            [ Atom "link" [ Variable "X", Variable "Y" ] ]
        , Rule
            (Atom "reachable" [ Variable "X", Variable "Y" ])
            [ Atom "link" [ Variable "X", Variable "Z" ]
            , Atom "reachable" [ Variable "Z", Variable "Y" ]
            ]

        -- query
        , Rule
            (Atom "query" [ Variable "X", Variable "Y" ])
            [ Atom "reachable" [ Variable "X", Variable "Y" ] ]
        ]
