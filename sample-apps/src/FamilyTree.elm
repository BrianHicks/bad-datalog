module FamilyTree exposing (..)

import Css
import Datalog exposing (Database)
import Datalog.Database as Database
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attrs exposing (css)
import Html.Styled.Events as Events
import Url.Parser as Parser exposing ((</>), Parser)


type alias Model =
    { db : Database
    , nextId : Int

    -- transient view state
    , newPersonField : String
    , lastError : Maybe Datalog.Problem
    , parentId : Maybe Int
    , childId : Maybe Int
    }


type Msg
    = UserTypedInNewPersonField String
    | UserClickedAddPerson
    | UserChoseParent (Maybe Int)
    | UserChoseChild (Maybe Int)
    | UserClickedAddParentChildRelationship
    | UserClickedShowPerson Int


type Route
    = Index
    | ShowPerson Int


pathFor : Route -> String
pathFor route =
    case route of
        Index ->
            "/"

        ShowPerson id ->
            "/" ++ String.fromInt id


routeParser : Parser (Route -> a) a
routeParser =
    Parser.oneOf
        [ Parser.map Index Parser.top
        , Parser.map ShowPerson (Parser.top </> Parser.int)
        ]


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
      }
    , Cmd.none
    )


type Effect
    = Navigate Route


update : Msg -> Model -> ( Model, Maybe Effect )
update msg model =
    case msg of
        UserTypedInNewPersonField input ->
            ( { model | newPersonField = input }
            , Nothing
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
                    , Nothing
                    )

                Err problem ->
                    ( { model | lastError = Just problem }
                    , Nothing
                    )

        UserChoseParent parentId ->
            ( { model | parentId = parentId }
            , Nothing
            )

        UserChoseChild childId ->
            ( { model | childId = childId }
            , Nothing
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
                            , Nothing
                            )

                        Err problem ->
                            ( { model | lastError = Just problem }
                            , Nothing
                            )

                _ ->
                    ( model
                    , Nothing
                    )

        UserClickedShowPerson id ->
            ( model
            , Just (Navigate (ShowPerson id))
            )


view : Route -> Model -> Html Msg
view route model =
    let
        allPeople =
            Datalog.read "person" personDecoder model.db
    in
    Html.div
        []
        [ Html.h1 [] [ Html.text "Family Tree" ]
        , Html.p [] [ Html.text "Enter names for the family tree below. Once you've added at least two names, you can set parent/child relationships. Click names you've already entered to see the relationships between that person and other family members." ]
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
            , case route of
                Index ->
                    Html.text ""

                ShowPerson personId ->
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
                                , Html.a
                                    [ -- TODO: this has too much knowlege about
                                      -- where it is. Would it be possible to
                                      -- lessen that knowledge somewhat?
                                      Attrs.href ("/family-tree/" ++ String.fromInt person.id)
                                    ]
                                    [ Html.text "Show Relationships" ]
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
                , Datalog.rule "grandchildren" [ "grandchildId", "grandchildName" ]
                    |> Datalog.with "person" [ Datalog.var "grandchildId", Datalog.var "grandchildName" ]
                    |> Datalog.with "parent" [ Datalog.var "childId", Datalog.var "grandchildId" ]
                    |> Datalog.with "parent" [ Datalog.int personId, Datalog.var "childId" ]
                , Datalog.rule "auntsAndUncles" [ "auId", "auName" ]
                    |> Datalog.with "person" [ Datalog.var "auId", Datalog.var "auName" ]
                    |> Datalog.with "parent" [ Datalog.var "grandparentId", Datalog.var "auId" ]
                    |> Datalog.with "parent" [ Datalog.var "grandparentId", Datalog.var "parentId" ]
                    |> Datalog.with "parent" [ Datalog.var "parentId", Datalog.int personId ]
                    |> Datalog.filter (Datalog.not_ (Datalog.eq "auId" (Datalog.var "parentId")))
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
