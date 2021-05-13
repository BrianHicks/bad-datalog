module FamilyTree exposing (..)

import Datalog exposing (Database)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attrs
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
    ( { db = Datalog.empty
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
        [ Html.form
            [ Events.onSubmit UserClickedAddPerson ]
            [ Html.label [] [ Html.text "New contact name" ]
            , Html.input
                [ Attrs.value model.newPersonField
                , Events.onInput UserTypedInNewPersonField
                ]
                []
            ]
        , Html.h2 [] [ Html.text "People" ]
        , Datalog.read "person" personDecoder model.db
            |> Debug.toString
            |> Html.text
        , Html.h2 [] [ Html.text "Debug" ]
        , Html.text (Debug.toString model)
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
