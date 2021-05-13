module FamilyTree exposing (..)

import Css
import Datalog exposing (Database)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attrs exposing (css)
import Html.Styled.Events as Events


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


init : ( Model, Cmd Msg )
init =
    let
        dbResult : Result Datalog.Problem Datalog.Database
        dbResult =
            Datalog.empty
                |> Datalog.register "person"
                |> Datalog.insert "person" [ Datalog.int 0, Datalog.string "Brian" ]
                |> Result.andThen (Datalog.insert "person" [ Datalog.int 1, Datalog.string "Anne" ])
                |> Result.andThen (Datalog.insert "person" [ Datalog.int 2, Datalog.string "Nate" ])
    in
    ( { db =
            case dbResult of
                Ok db ->
                    db

                Err _ ->
                    Datalog.empty
      , nextId = 3
      , newPersonField = ""
      , lastError =
            case dbResult of
                Ok _ ->
                    Nothing

                Err problem ->
                    Just problem
      , parentId = Nothing
      , childId = Nothing
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


view : Model -> Html Msg
view model =
    Html.div
        []
        [ Html.h1 [] [ Html.text "Family Tree" ]
        , Html.p [] [ Html.text "Enter names for the family tree below. Once you've added at least two names, you can set parent/child relationships. Click names you've already entered to see the realtionships between that person and other family members." ]
        , addNewPersonForm model
        , Html.h2 [] [ Html.text "People" ]
        , Datalog.read "person" personDecoder model.db
            |> viewResult viewError (viewFamily model)
        , case model.activePerson of
            Nothing ->
                Html.text ""

            Just personId ->
                viewPerson model personId
        ]


addNewPersonForm : Model -> Html Msg
addNewPersonForm model =
    Html.section []
        [ Html.h2 [] [ Html.text "Add a New Person" ]
        , Html.form
            [ Events.onSubmit UserClickedAddPerson
            , css [ Css.width (Css.px 300) ]
            ]
            [ Html.label [ css [ Css.displayFlex ] ]
                [ Html.text "Person's name: "
                , Html.input
                    [ Attrs.value model.newPersonField
                    , Events.onInput UserTypedInNewPersonField
                    , css
                        [ Css.flexGrow (Css.int 1)
                        , Css.marginLeft (Css.px 5)
                        ]
                    ]
                    []
                ]
            , Html.button
                [ css
                    [ Css.display Css.block
                    , Css.width (Css.pct 100)
                    , Css.marginTop (Css.px 20)
                    ]
                ]
                [ Html.text "Add Person" ]
            ]
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


viewFamily : Model -> List Person -> Html Msg
viewFamily model people =
    case people of
        [] ->
            Html.text "Nobody has been added yet!"

        _ ->
            Html.div []
                [ people
                    |> List.map
                        (\person ->
                            Html.li []
                                [ Html.text person.name
                                , Html.button
                                    [ Events.onClick (UserClickedShowPerson person.id) ]
                                    [ Html.text "Show Relationships" ]
                                ]
                        )
                    |> Html.ul []
                , viewParentChildForm model people
                ]


viewParentChildForm : Model -> List Person -> Html Msg
viewParentChildForm model people =
    case people of
        _ :: _ :: _ ->
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
            Html.form
                [ Events.onSubmit UserClickedAddParentChildRelationship ]
                [ Html.label
                    [ css [ Css.display Css.block ] ]
                    [ Html.text "Parent: "
                    , Html.select
                        [ Events.onInput (UserChoseParent << String.toInt)
                        , model.parentId
                            |> Maybe.map String.fromInt
                            |> Maybe.withDefault ""
                            |> Attrs.value
                        ]
                        peopleOptions
                    ]
                , Html.label
                    [ css [ Css.display Css.block ] ]
                    [ Html.text "Child: "
                    , Html.select
                        [ Events.onInput (UserChoseChild << String.toInt)
                        , model.childId
                            |> Maybe.map String.fromInt
                            |> Maybe.withDefault ""
                            |> Attrs.value
                        ]
                        peopleOptions
                    ]
                , Html.button
                    []
                    [ Html.text "Add parent/child relationship" ]
                ]

        _ ->
            Html.text "add at least two people and I'll let you set relationships!"


viewPerson : Model -> Int -> Html Msg
viewPerson model personId =
    let
        derived =
            Datalog.derive
                [ Datalog.rule "me" [ "id", "name" ]
                    |> Datalog.with "person" [ Datalog.var "id", Datalog.var "name" ]
                    |> Datalog.filter (Datalog.eq "id" (Datalog.int personId))
                , Datalog.rule "parents" [ "id", "name" ]
                    |> Datalog.with "person" [ Datalog.var "id", Datalog.var "name" ]
                    |> Datalog.with "parent" [ Datalog.var "id", Datalog.int personId ]
                , Datalog.rule "children" [ "id", "name" ]
                    |> Datalog.with "person" [ Datalog.var "id", Datalog.var "name" ]
                    |> Datalog.with "parent" [ Datalog.int personId, Datalog.var "id" ]
                , Datalog.rule "siblings" [ "siblingId", "siblingName" ]
                    |> Datalog.with "person" [ Datalog.var "siblingId", Datalog.var "siblingName" ]
                    |> Datalog.with "parent" [ Datalog.var "parentId", Datalog.int personId ]
                    |> Datalog.with "parent" [ Datalog.var "parentId", Datalog.var "siblingId" ]
                    |> Datalog.filter (Datalog.not_ (Datalog.eq "siblingId" (Datalog.int personId)))
                , Datalog.rule "grandparents" [ "grandparentId", "grandparentName" ]
                    |> Datalog.with "person" [ Datalog.var "grandparentId", Datalog.var "grandparentName" ]
                    |> Datalog.with "parent" [ Datalog.var "parentId", Datalog.int personId ]
                    |> Datalog.with "parent" [ Datalog.var "grandparentId", Datalog.var "parentId" ]
                ]
                model.db
    in
    Result.map5
        (\self parents children auntsAndUncles grandparents ->
            Html.div
                []
                (self ++ parents ++ children ++ auntsAndUncles ++ grandparents)
        )
        (derived
            |> Result.andThen (Datalog.read "me" personDecoder)
            |> Result.map
                (\me ->
                    [ Html.h3 [] [ Html.text "Me" ]
                    , Html.text (Debug.toString me)
                    ]
                )
        )
        (derived
            |> Result.andThen (Datalog.read "parents" personDecoder)
            |> Result.map
                (\parents ->
                    [ Html.h3 [] [ Html.text "Parents" ]
                    , Html.text (Debug.toString parents)
                    ]
                )
        )
        (derived
            |> Result.andThen (Datalog.read "children" personDecoder)
            |> Result.map
                (\children ->
                    [ Html.h3 [] [ Html.text "Children" ]
                    , Html.text (Debug.toString children)
                    ]
                )
        )
        (derived
            |> Result.andThen (Datalog.read "siblings" personDecoder)
            |> Result.map
                (\siblings ->
                    [ Html.h3 [] [ Html.text "Siblings" ]
                    , Html.text (Debug.toString siblings)
                    ]
                )
        )
        (derived
            |> Result.andThen (Datalog.read "grandparents" personDecoder)
            |> Result.map
                (\grandparents ->
                    [ Html.h3 [] [ Html.text "Grandparents" ]
                    , Html.text (Debug.toString grandparents)
                    ]
                )
        )
        |> viewResult viewError identity


viewError : Datalog.Problem -> Html msg
viewError problem =
    Html.details
        []
        [ Html.summary [] [ Html.text "Somthing went wrong. Oh no! Here are more details." ]
        , Html.text (Debug.toString problem)
        ]


viewResult : (error -> Html msg) -> (success -> Html msg) -> Result error success -> Html msg
viewResult viewError_ viewSuccess result =
    case result of
        Ok success ->
            viewSuccess success

        Err error ->
            viewError_ error
