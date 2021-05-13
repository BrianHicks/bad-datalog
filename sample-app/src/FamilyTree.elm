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
    ( { db =
            Datalog.empty
                |> Datalog.register "person"
      , nextId = 0
      , newPersonField = ""
      , lastError = Nothing
      , parentId = Nothing
      , childId = Nothing
      , activePerson = Just 0
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
    let
        allPeople =
            Datalog.read "person" personDecoder model.db
    in
    Html.div
        []
        [ Html.h1 [] [ Html.text "Family Tree" ]
        , Html.p [] [ Html.text "Enter names for the family tree below. Once you've added at least two names, you can set parent/child relationships. Click names you've already entered to see the relationships between that person and other family members." ]
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
        , Html.h2 [] [ Html.text "People" ]
        , viewResult viewError (viewPeopleList model) allPeople
        , case model.activePerson of
            Nothing ->
                Html.text ""

            Just personId ->
                viewPerson model personId
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
                                , Html.a
                                    [ Events.onClick (UserClickedShowPerson person.id)
                                    , Attrs.href "#"
                                    ]
                                    [ Html.text "Show Relationships" ]
                                , Html.text ")"
                                ]
                        )
                    |> Html.ul []
                ]


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
