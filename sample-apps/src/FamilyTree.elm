module FamilyTree exposing (..)

import Css
import Datalog exposing (Database)
import Datalog.Database as Database
import Dict exposing (Dict)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attrs exposing (css)
import Html.Styled.Events as Events
import Set


type alias Model =
    { db : Database
    , nextId : Int

    -- transient view state
    , newPersonField : String
    , lastError : Maybe Datalog.Problem
    , parentId : Maybe Int
    , childId : Maybe Int
    , activePerson : Maybe Int
    }


type Msg
    = UserTypedInNewPersonField String
    | UserClickedAddPerson
    | UserChoseParent (Maybe Int)
    | UserChoseChild (Maybe Int)
    | UserClickedAddParentChildRelationship
    | UserClickedShowPerson Int
    | UserClickedLoadSampleData Dataset


type Dataset
    = KennedyFamily


init : ( Model, Cmd Msg )
init =
    let
        dbResult =
            Datalog.empty
                |> Datalog.register "person" [ Database.IntType, Database.StringType ]
                |> Result.andThen (Datalog.register "parent" [ Database.IntType, Database.IntType ])
    in
    ( { db =
            case dbResult of
                Ok db ->
                    db

                Err problem ->
                    Datalog.empty
      , nextId = 0
      , newPersonField = ""
      , lastError =
            case dbResult of
                Ok _ ->
                    Nothing

                Err problem ->
                    Just problem
      , parentId = Nothing
      , childId = Nothing
      , activePerson = Nothing
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UserTypedInNewPersonField input ->
            ( { model | newPersonField = input }
            , Cmd.none
            )

        UserClickedAddPerson ->
            case
                Datalog.insert "person"
                    [ Datalog.int model.nextId
                    , Datalog.string model.newPersonField
                    ]
                    model.db
            of
                Ok newDb ->
                    ( { model
                        | db = newDb
                        , nextId = model.nextId + 1
                        , newPersonField = ""
                      }
                    , Cmd.none
                    )

                Err problem ->
                    ( { model | lastError = Just problem }
                    , Cmd.none
                    )

        UserChoseParent parentId ->
            ( { model | parentId = parentId }
            , Cmd.none
            )

        UserChoseChild childId ->
            ( { model | childId = childId }
            , Cmd.none
            )

        UserClickedAddParentChildRelationship ->
            case ( model.parentId, model.childId ) of
                ( Just parentId, Just childId ) ->
                    case
                        Datalog.insert "parent"
                            [ Datalog.int parentId
                            , Datalog.int childId
                            ]
                            model.db
                    of
                        Ok newDb ->
                            ( { model
                                | db = newDb
                                , parentId = Nothing
                                , childId = Nothing
                              }
                            , Cmd.none
                            )

                        Err problem ->
                            ( { model | lastError = Just problem }
                            , Cmd.none
                            )

                _ ->
                    ( model
                    , Cmd.none
                    )

        UserClickedShowPerson id ->
            ( { model | activePerson = Just id }
            , Cmd.none
            )

        UserClickedLoadSampleData dataset ->
            ( loadSampleData dataset
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    let
        allPeople =
            Datalog.read "person" personDecoder model.db
    in
    Html.div
        []
        [ Html.h1 [] [ Html.text "Family Tree" ]
        , Html.p [] [ Html.text "Enter names for the family tree below. Once you've added at least two names, you can set parent/child relationships. Click names you've already entered to see the relationships between that person and other family members." ]
        , Html.p [] [ Html.text "Alternatively, you can load a pre-made family tree:" ]
        , Html.ul []
            [ Html.li []
                [ linkButton (UserClickedLoadSampleData KennedyFamily) "Kennedy Family (U.S. Political Dynasty)"
                , Html.text " (Note: this is a HUGE family, and so if you know about the Kennedys you'll notice people missing. But it's enough for a demo!)"
                ]
            ]
        , Html.p []
            [ Html.text "View the Elm source for this page at "
            , Html.a
                [ Attrs.href "https://git.bytes.zone/brian/bad-datalog/src/branch/main/sample-apps/src/FamilyTree.elm" ]
                [ Html.text "sample-apps/src/FamilyTree.elm" ]
            , Html.text " in the "
            , Html.a
                [ Attrs.href "https://git.bytes.zone/brian/bad-datalog" ]
                [ Html.text "brian/bad-datalog repo" ]
            , Html.text "."
            ]
        , case model.lastError of
            Nothing ->
                Html.text ""

            Just problem ->
                viewError problem
        , Html.div
            [ css
                [ Css.displayFlex
                , Css.justifyContent Css.spaceAround
                ]
            ]
            [ viewAddNewPersonForm model
            , viewResult
                (\_ -> Html.text "")
                (viewAddParentChildForm model)
                allPeople
            ]
        , Html.div
            [ css [ Css.displayFlex ] ]
            [ Html.section [ css [ Css.flexGrow (Css.int 1) ] ]
                [ Html.h2 [] [ Html.text "People" ]
                , viewResult viewError (viewPeopleList model) allPeople
                ]
            , case model.activePerson of
                Nothing ->
                    Html.text ""

                Just personId ->
                    viewRelationships model personId
            ]
        ]


viewAddNewPersonForm : Model -> Html Msg
viewAddNewPersonForm model =
    Html.section []
        [ Html.h2 [] [ Html.text "Add a New Person" ]
        , viewForm
            { onSubmit = UserClickedAddPerson
            , fields =
                [ ( "Name"
                  , Html.input
                        [ Attrs.value model.newPersonField
                        , Events.onInput UserTypedInNewPersonField
                        ]
                        []
                  )
                ]
            , submitCaption = "Add Person"
            }
        ]


viewAddParentChildForm : Model -> List Person -> Html Msg
viewAddParentChildForm model people =
    let
        peopleOptions =
            people
                |> List.map
                    (\{ id, name } ->
                        Html.option
                            [ Attrs.value (String.fromInt id) ]
                            [ Html.text name ]
                    )
                |> (::)
                    (Html.option
                        [ Attrs.value "" ]
                        [ Html.text "---" ]
                    )
    in
    Html.section []
        [ Html.h2 [] [ Html.text "Add a Parent/Child Relationship" ]
        , case people of
            _ :: _ :: _ ->
                viewForm
                    { onSubmit = UserClickedAddParentChildRelationship
                    , fields =
                        [ ( "Parent"
                          , Html.select
                                [ Events.onInput (UserChoseParent << String.toInt)
                                , model.parentId
                                    |> Maybe.map String.fromInt
                                    |> Maybe.withDefault ""
                                    |> Attrs.value
                                , css [ Css.flexGrow (Css.int 1) ]
                                ]
                                peopleOptions
                          )
                        , ( "Child"
                          , Html.select
                                [ Events.onInput (UserChoseChild << String.toInt)
                                , model.childId
                                    |> Maybe.map String.fromInt
                                    |> Maybe.withDefault ""
                                    |> Attrs.value
                                , css [ Css.flexGrow (Css.int 1) ]
                                ]
                                peopleOptions
                          )
                        ]
                    , submitCaption = "Add parent/child relationship"
                    }

            _ ->
                Html.text "add at least two people and I'll let you set relationships!"
        ]


type alias Person =
    { id : Int
    , name : String
    }


personDecoder : Datalog.Decoder Person
personDecoder =
    Datalog.into Person
        |> Datalog.intField 0
        |> Datalog.stringField 1


viewPeopleList : Model -> List Person -> Html Msg
viewPeopleList model people =
    case people of
        [] ->
            Html.text "Nobody!"

        _ ->
            Html.div []
                [ people
                    |> List.map
                        (\person ->
                            Html.li []
                                [ Html.text person.name
                                , Html.text " ("
                                , linkButton (UserClickedShowPerson person.id) "Show relationships"
                                , Html.text ")"
                                ]
                        )
                    |> Html.ul []
                ]


viewRelationships : Model -> Int -> Html Msg
viewRelationships model personId =
    let
        derived =
            Datalog.derive
                [ {- me(id, name) :- person(id, name), id = personId -}
                  Datalog.rule "me"
                    [ "id", "name" ]
                    [ Datalog.with "person" [ Datalog.var "id", Datalog.var "name" ]
                    , Datalog.filter (Datalog.eq "id" (Datalog.int personId))
                    ]
                , {- parents(id, name) :- person(id, name), parent(id, personId). -}
                  Datalog.rule "parents"
                    [ "id", "name" ]
                    [ Datalog.with "person" [ Datalog.var "id", Datalog.var "name" ]
                    , Datalog.with "parent" [ Datalog.var "id", Datalog.int personId ]
                    ]
                , {- children(id, name) :- person(id, name), parent(personId, id). -}
                  Datalog.rule "children"
                    [ "id", "name" ]
                    [ Datalog.with "person" [ Datalog.var "id", Datalog.var "name" ]
                    , Datalog.with "parent" [ Datalog.int personId, Datalog.var "id" ]
                    ]
                , {-
                     siblings(siblingId, siblingName) :-
                        person(siblingId, siblingName),
                        parent(parentId, personId),
                        parent(parentId, siblingId),
                        siblingId /= personId.
                  -}
                  Datalog.rule "siblings"
                    [ "siblingId", "siblingName" ]
                    [ Datalog.with "person" [ Datalog.var "siblingId", Datalog.var "siblingName" ]
                    , Datalog.with "parent" [ Datalog.var "parentId", Datalog.int personId ]
                    , Datalog.with "parent" [ Datalog.var "parentId", Datalog.var "siblingId" ]
                    , Datalog.filter (Datalog.not_ (Datalog.eq "siblingId" (Datalog.int personId)))
                    ]
                , {-
                     grandparents(grandparentId, grandparentName) :-
                        person(grandparentId, grandparentName),
                        person(parentId, personId),
                        person(grandparentId, parentId).
                  -}
                  Datalog.rule "grandparents"
                    [ "grandparentId", "grandparentName" ]
                    [ Datalog.with "person" [ Datalog.var "grandparentId", Datalog.var "grandparentName" ]
                    , Datalog.with "parent" [ Datalog.var "parentId", Datalog.int personId ]
                    , Datalog.with "parent" [ Datalog.var "grandparentId", Datalog.var "parentId" ]
                    ]
                , Datalog.rule "grandchildren"
                    [ "grandchildId", "grandchildName" ]
                    [ Datalog.with "person" [ Datalog.var "grandchildId", Datalog.var "grandchildName" ]
                    , Datalog.with "parent" [ Datalog.var "childId", Datalog.var "grandchildId" ]
                    , Datalog.with "parent" [ Datalog.int personId, Datalog.var "childId" ]
                    ]
                , {-
                     auntsAndUncles(auId, auName) :-
                       person(auId, auName),
                       parent(grandparentId, auId),
                       parent(grandparentId, parentId),
                       parent(parentId, personId),
                       auId /= parentId.
                  -}
                  Datalog.rule "auntsAndUncles"
                    [ "auId", "auName" ]
                    [ Datalog.with "person" [ Datalog.var "auId", Datalog.var "auName" ]
                    , Datalog.with "parent" [ Datalog.var "grandparentId", Datalog.var "auId" ]
                    , Datalog.with "parent" [ Datalog.var "grandparentId", Datalog.var "parentId" ]
                    , Datalog.with "parent" [ Datalog.var "parentId", Datalog.int personId ]
                    , Datalog.filter (Datalog.not_ (Datalog.eq "auId" (Datalog.var "parentId")))
                    ]
                , Datalog.rule "niecesAndNephews"
                    [ "nnId", "nnName" ]
                    [ Datalog.with "person" [ Datalog.var "nnId", Datalog.var "nnName" ]
                    , Datalog.with "parent" [ Datalog.var "siblingId", Datalog.var "nnId" ]
                    , Datalog.with "parent" [ Datalog.var "parentId", Datalog.var "siblingId" ]
                    , Datalog.with "parent" [ Datalog.var "parentId", Datalog.int personId ]
                    , Datalog.filter (Datalog.not_ (Datalog.eq "siblingId" (Datalog.int personId)))
                    ]
                ]
                model.db

        viewSection : String -> List Person -> Html Msg
        viewSection name people =
            Html.section []
                [ Html.h3 [] [ Html.text name ]
                , viewPeopleList model people
                ]
    in
    derived
        |> Result.andThen (Datalog.readOne "me" personDecoder)
        |> Result.andThen
            (\me ->
                flattenResults
                    [ Ok (Html.h2 [] [ Html.text me.name ])
                    , derived
                        |> Result.andThen (Datalog.read "parents" personDecoder)
                        |> Result.map (viewSection "Parents")
                    , derived
                        |> Result.andThen (Datalog.read "grandparents" personDecoder)
                        |> Result.map (viewSection "Grandparents")
                    , derived
                        |> Result.andThen (Datalog.read "auntsAndUncles" personDecoder)
                        |> Result.map (viewSection "Aunts and Uncles")
                    , derived
                        |> Result.andThen (Datalog.read "siblings" personDecoder)
                        |> Result.map (viewSection "Siblings")
                    , derived
                        |> Result.andThen (Datalog.read "children" personDecoder)
                        |> Result.map (viewSection "Children")
                    , derived
                        |> Result.andThen (Datalog.read "niecesAndNephews" personDecoder)
                        |> Result.map (viewSection "Nieces and Nephews")
                    , derived
                        |> Result.andThen (Datalog.read "grandchildren" personDecoder)
                        |> Result.map (viewSection "Grandchildren")
                    ]
            )
        |> viewResult viewError (Html.div [ css [ Css.flexGrow (Css.int 1) ] ])


viewError : Datalog.Problem -> Html msg
viewError problem =
    Html.details
        []
        [ Html.summary [] [ Html.text "Something went wrong. Oh no! Here are more details." ]
        , Html.text (Debug.toString problem)
        ]


viewResult : (error -> Html msg) -> (success -> Html msg) -> Result error success -> Html msg
viewResult viewError_ viewSuccess result =
    case result of
        Ok success ->
            viewSuccess success

        Err error ->
            viewError_ error


viewForm :
    { onSubmit : msg
    , fields : List ( String, Html msg )
    , submitCaption : String
    }
    -> Html msg
viewForm { onSubmit, fields, submitCaption } =
    Html.form
        [ Events.onSubmit onSubmit ]
        (List.map
            (\( label, input ) ->
                Html.label
                    [ css
                        [ Css.displayFlex
                        , Css.marginBottom (Css.px 10)
                        ]
                    ]
                    [ Html.span [ css [ Css.minWidth (Css.px 75) ] ] [ Html.text label ]
                    , input
                    ]
            )
            fields
            ++ [ Html.button
                    [ css [ Css.width (Css.pct 100) ] ]
                    [ Html.text submitCaption ]
               ]
        )


flattenResults : List (Result x a) -> Result x (List a)
flattenResults =
    List.foldr (Result.map2 (::)) (Ok [])


linkButton : msg -> String -> Html msg
linkButton msg caption =
    Html.button
        [ Events.onClick msg
        , css
            [ Css.fontSize Css.unset
            , Css.border Css.zero
            , Css.backgroundColor Css.unset
            , Css.display Css.inline
            , Css.margin Css.unset
            , Css.padding Css.unset
            , Css.color (Css.hex "006064")
            , Css.textDecoration Css.underline
            , Css.cursor Css.pointer
            ]
        ]
        [ Html.text caption ]


loadSampleData : Dataset -> Model
loadSampleData dataset =
    let
        addPerson :
            Int
            -> String
            -> Result Datalog.Problem Datalog.Database
            -> Result Datalog.Problem Datalog.Database
        addPerson id name =
            Result.andThen (Datalog.insert "person" [ Datalog.int id, Datalog.string name ])
    in
    case dataset of
        KennedyFamily ->
            let
                familyTree : Dict String (List String)
                familyTree =
                    Dict.fromList
                        [ ( "Joseph P 'Joe' Kennedy Sr.", [ "Joseph Patrick Kennedy Jr.", "John Fitzgerald 'Jack' Kennedy", "Rose Marie Kennedy", "Kathleen Agnes (Marchioness of Hartington) Kennedy", "Eunice Mary Kennedy", "Patricia Helen Kennedy", "Robert Francis 'Bobby' Kennedy", "Jean Ann Kennedy", "Edward Moore 'Ted' Kennedy" ] )
                        , ( "Joseph P 'Joe' Kennedy Sr.", [ "Joseph Patrick Kennedy Jr.", "John Fitzgerald 'Jack' Kennedy", "Rose Marie Kennedy", "Kathleen Agnes (Marchioness of Hartington) Kennedy", "Eunice Mary Kennedy", "Patricia Helen Kennedy", "Robert Francis 'Bobby' Kennedy", "Jean Ann Kennedy", "Edward Moore 'Ted' Kennedy" ] )
                        , ( "John Fitzgerald 'Jack' Kennedy", [ "Arabella Kennedy", "Caroline Bouvier Kennedy", "John Fitzgerald Kennedy Jr.", "Patrick Bouvier Kennedy" ] )
                        , ( "Jacqueline Lee Bouvier", [ "Arabella Kennedy", "Caroline Bouvier Kennedy", "John Fitzgerald Kennedy Jr.", "Patrick Bouvier Kennedy" ] )
                        , ( "Eunice Mary Kennedy", [ "Robert Sargent Shriver III", "Maria Owings Shriver", "Timothy Perry Shriver", "Mark Kennedy Shriver", "Anthony Paul Kennedy Shriver" ] )
                        , ( "Robert Sargent Shriver Jr.", [ "Robert Sargent Shriver III", "Maria Owings Shriver", "Timothy Perry Shriver", "Mark Kennedy Shriver", "Anthony Paul Kennedy Shriver" ] )
                        , ( "Patricia Helen Kennedy", [ "Christopher Kennedy Lawford", "Sydney Maleaia Kennedy Lawford", "Victoria Francis Lawford", "Robin Elizabeth Lawford" ] )
                        , ( "Peter Sydney Ernest Lawford", [ "Christopher Kennedy Lawford", "Sydney Maleaia Kennedy Lawford", "Victoria Francis Lawford", "Robin Elizabeth Lawford" ] )
                        , ( "Robert Francis 'Bobby' Kennedy", [ "Kathleen Hartington Kennedy", "Joseph Patrick Kennedy II", "Robert Francis Kennedy Jr.", "David Anthony Kennedy", "Mary Courtney Kennedy", "Michael LeMoyne Kennedy", "Mary Kerry 'Kerry' Kennedy", "Christopher George Kennedy", "Matthew Maxwell Taylor 'Max' Kennedy", "Douglass Harriman Kennedy", "Rory Elizabeth Katherine Kennedy" ] )
                        , ( "Ethel Skakel", [ "Kathleen Hartington Kennedy", "Joseph Patrick Kennedy II", "Robert Francis Kennedy Jr.", "David Anthony Kennedy", "Mary Courtney Kennedy", "Michael LeMoyne Kennedy", "Mary Kerry 'Kerry' Kennedy", "Christopher George Kennedy", "Matthew Maxwell Taylor 'Max' Kennedy", "Douglass Harriman Kennedy", "Rory Elizabeth Katherine Kennedy" ] )
                        , ( "Jean Ann Kennedy", [ "Stephen Edward Smith Jr.", "William Kennedy Smith", "Amanda Mary Smith", "Kym Maria Smith" ] )
                        , ( "Stephen Edward Smith", [ "Stephen Edward Smith Jr.", "William Kennedy Smith", "Amanda Mary Smith", "Kym Maria Smith" ] )
                        , ( "Edward Moore 'Ted' Kennedy", [ "Kara Anne Kennedy", "Edward Moore Kennedy Jr.", "Patrick Joseph Kennedy II" ] )
                        , ( "Virginia Joan Bennet", [ "Kara Anne Kennedy", "Edward Moore Kennedy Jr.", "Patrick Joseph Kennedy II" ] )
                        , ( "Maria Owings Shriver", [ "Katherine Eunice Schwarzenegger", "Christina Maria Aurelia Schwarzenegger", "Patrick Arnold Shriver Schwarzenegger", "Christopher Sargent Shriver Schwarzenegger" ] )
                        , ( "Arnold Alois Schwarzenegger", [ "Katherine Eunice Schwarzenegger", "Christina Maria Aurelia Schwarzenegger", "Patrick Arnold Shriver Schwarzenegger", "Christopher Sargent Shriver Schwarzenegger" ] )
                        , ( "Katherine Eunice Schwarzenegger", [ "Lyla Maria Pratt" ] )
                        , ( "Christopher Michael Pratt", [ "Lyla Maria Pratt" ] )
                        ]

                people : Dict String Int
                people =
                    familyTree
                        |> Dict.foldl
                            (\parent children soFar ->
                                soFar
                                    |> Set.insert parent
                                    |> Set.union (Set.fromList children)
                            )
                            Set.empty
                        |> Set.toList
                        |> List.indexedMap (\i person -> ( person, i ))
                        |> Dict.fromList

                parents : List ( Int, Int )
                parents =
                    familyTree
                        |> Dict.toList
                        |> List.concatMap (\( parent, children ) -> List.map (Tuple.pair parent) children)
                        |> List.filterMap
                            (\( parent, child ) ->
                                Maybe.map2 Tuple.pair
                                    (Dict.get parent people)
                                    (Dict.get child people)
                            )

                dbResult =
                    Ok Datalog.empty
                        |> (\dbResultWithoutPeople ->
                                Dict.foldl
                                    (\person id ->
                                        Result.andThen
                                            (Datalog.insert "person"
                                                [ Datalog.int id
                                                , Datalog.string person
                                                ]
                                            )
                                    )
                                    dbResultWithoutPeople
                                    people
                           )
                        |> (\dbResultWithoutParents ->
                                List.foldl
                                    (\( parent, child ) ->
                                        Result.andThen
                                            (Datalog.insert "parent"
                                                [ Datalog.int parent
                                                , Datalog.int child
                                                ]
                                            )
                                    )
                                    dbResultWithoutParents
                                    parents
                           )
            in
            { db =
                case dbResult of
                    Ok db ->
                        db

                    Err problem ->
                        Datalog.empty
            , nextId = 0
            , newPersonField = ""
            , lastError =
                case dbResult of
                    Ok _ ->
                        Nothing

                    Err problem ->
                        Just problem
            , parentId = Nothing
            , childId = Nothing
            , activePerson = Dict.get "John Fitzgerald 'Jack' Kennedy" people
            }
