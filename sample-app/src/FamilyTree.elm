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
    }


type Msg
    = UserTypedInNewPersonField String
    | UserClickedAddPerson


init : ( Model, Cmd Msg )
init =
    ( { db =
            Datalog.empty
                |> Datalog.register "person"
      , nextId = 0
      , newPersonField = ""
      , lastError = Nothing
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


view : Model -> Html Msg
view model =
    Html.div
        []
        [ Html.h1 [] [ Html.text "Family Tree" ]
        , Html.p [] [ Html.text "Enter names for the family tree below. Once you've added at least two names, you can set parent/child relationships. Click names you've already entered to see the realtionships between that person and other family members." ]
        , addNewPersonForm model
        , Html.h2 [] [ Html.text "People" ]
        , Datalog.read "person" personDecoder model.db
            |> viewResult viewError viewFamily
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


viewFamily : List Person -> Html msg
viewFamily people =
    case people of
        [] ->
            Html.text "Nobody has been added yet!"

        _ ->
            Html.div []
                [ people
                    |> List.map (\person -> Html.li [] [ Html.text person.name ])
                    |> Html.ul []
                , viewParentChildForm people
                ]


viewParentChildForm : List Person -> Html msg
viewParentChildForm people =
    case people of
        _ :: _ :: _ ->
            Html.form
                []
                [ Html.label []
                    [ Html.text "Parent: "
                    , Html.select [] []
                    ]
                , Html.label []
                    [ Html.text "Child: "
                    , Html.select [] []
                    ]
                ]

        _ ->
            Html.text "add at least two people and I'll let you set relationships!"


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
