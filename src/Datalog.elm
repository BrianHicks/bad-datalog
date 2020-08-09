module Datalog exposing (..)

{-| Like it says in the package name, this is a bad datalog.

<https://dodisturb.me/posts/2018-12-25-The-Essence-of-Datalog.html>

-}


{-| a head and body. All of these are rules:

    parent("Nate", "Brian"). // Brian is the parent of nate

    ancestor(X, Y) :- parent(X, Y).
    ancestor(X, Y) :- parent(X, Z), ancestory(Z, Y).

(The "parent" declaration is a rule with no body.)

-}
type Rule
    = Rule Atom (List Atom)


ruleToString : Rule -> String
ruleToString (Rule head body) =
    case body of
        [] ->
            atomToString head ++ "."

        _ ->
            atomToString head ++ " :- " ++ String.join ", " (List.map atomToString body) ++ "."


{-| a predicate name and list of terms.
-}
type Atom
    = Atom String (List Term)


atomToString : Atom -> String
atomToString (Atom name terms) =
    name ++ "(" ++ String.join ", " (List.map termToString terms) ++ ")."


{-| either a variable or a symbol.

    "symbol"

    Variable

-}
type Term
    = Var String
    | Sym String


termToString : Term -> String
termToString term =
    case term of
        Var var ->
            var

        Sym sym ->
            "\"" ++ sym ++ "\""


type alias Program =
    List Rule


type alias KnowledgeBase =
    List Atom


type alias Substitution =
    List ( Term, Term )


emptySubstitution : Substitution
emptySubstitution =
    []


lookup : Term -> Substitution -> Maybe Term
lookup term substitution =
    substitution
        |> List.filterMap
            (\( l, r ) ->
                if term == l then
                    Just r

                else
                    Nothing
            )
        |> List.head


{-| Cheats a lot. See the source article for how.
-}
substitute : Atom -> Substitution -> Atom
substitute (Atom name terms) substitution =
    let
        go : Term -> Term
        go term =
            case term of
                Sym _ ->
                    term

                Var _ ->
                    substitution
                        |> lookup term
                        |> Maybe.withDefault term
    in
    Atom name (List.map go terms)


{-| Cheats a lot. See the source article for how.
-}
unify : Atom -> Atom -> Maybe Substitution
unify (Atom predSym ts) (Atom predSym_ ts_) =
    let
        go : List ( Term, Term ) -> Maybe Substitution
        go pairs =
            case pairs of
                [] ->
                    Just emptySubstitution

                ( Sym s, Sym s_ ) :: rest ->
                    if s == s_ then
                        go rest

                    else
                        Nothing

                ( (Var _) as v, (Sym _) as s ) :: rest ->
                    let
                        incompleteSubstitution =
                            go rest
                    in
                    case Maybe.andThen (lookup v) incompleteSubstitution of
                        Just term ->
                            if term /= s then
                                Nothing

                            else
                                Maybe.map ((::) ( v, s )) incompleteSubstitution

                        Nothing ->
                            Maybe.map ((::) ( v, s )) incompleteSubstitution

                ( _, Var _ ) :: _ ->
                    Debug.todo "The second atom is assumed to be ground."
    in
    if predSym == predSym_ then
        go (List.map2 Tuple.pair ts ts_)

    else
        Nothing


evalAtom : KnowledgeBase -> Atom -> List Substitution -> List Substitution
evalAtom kb atom substitutions =
    List.concatMap
        (\substitution ->
            let
                downToEarthAtom =
                    substitute atom substitution
            in
            kb
                |> List.filterMap (unify downToEarthAtom)
                |> List.map (\extension -> substitution ++ extension)
        )
        substitutions


walk : KnowledgeBase -> List Atom -> List Substitution
walk kb =
    List.foldr (evalAtom kb) [ emptySubstitution ]


evalRule : KnowledgeBase -> Rule -> KnowledgeBase
evalRule kb (Rule head body) =
    List.map (substitute head) (walk kb body)


immediateConsequence : Program -> KnowledgeBase -> KnowledgeBase
immediateConsequence rules kb =
    nub << (++) kb << List.concatMap (evalRule kb) <| rules



-- stuff from ported Haskell implementation


nub : KnowledgeBase -> KnowledgeBase
nub kb =
    kb
        |> List.sortBy atomToString
        |> List.foldr
            (\next ( current, acc ) ->
                case current of
                    Just set ->
                        if set == next then
                            ( current, acc )

                        else
                            ( Just next, set :: acc )

                    Nothing ->
                        ( Just next, acc )
            )
            ( Nothing, [] )
        |> (\( final, acc ) ->
                case final of
                    Just set ->
                        set :: acc

                    Nothing ->
                        acc
           )
