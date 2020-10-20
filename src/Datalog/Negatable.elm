module Datalog.Negatable exposing
    ( Negatable, positive, negative
    , unwrap, isPositive
    , not, map
    )

{-|

@docs Negatable, positive, negative
@docs unwrap, isPositive
@docs not, map

-}


type Negatable a
    = Negatable Bool a


positive : a -> Negatable a
positive =
    Negatable True


negative : a -> Negatable a
negative =
    Negatable False


unwrap : Negatable a -> a
unwrap (Negatable _ a) =
    a


isPositive : Negatable a -> Bool
isPositive (Negatable pos _) =
    pos


map : (a -> b) -> Negatable a -> Negatable b
map fn (Negatable pos a) =
    Negatable pos (fn a)


not : Negatable a -> Negatable a
not (Negatable pos a) =
    Negatable (Basics.not pos) a
