module Datalog.Negatable exposing
    ( Negatable(..), Direction(..), positive, negative
    , value, map
    )

{-|

@docs Negatable, Direction, positive, negative
@docs value, map

-}


type Negatable a
    = Negatable Direction a


type Direction
    = Positive
    | Negative


positive : a -> Negatable a
positive =
    Negatable Positive


negative : a -> Negatable a
negative =
    Negatable Negative


value : Negatable a -> a
value (Negatable _ a) =
    a


map : (a -> b) -> Negatable a -> Negatable b
map fn (Negatable direction_ a) =
    Negatable direction_ (fn a)
