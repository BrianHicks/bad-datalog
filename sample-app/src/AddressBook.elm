module AddressBook exposing (..)

import Database
import Database.Datalog as Datalog exposing (Database)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attrs
import Html.Styled.Events as Events


type alias Model =
    { db : Database

    -- transient view state
    , newContactField : String
    , lastError : Maybe Datalog.Problem
    }


type Msg
    = UserTypedInNewContactField String
    | UserClickedAddContact


init : ( Model, Cmd Msg )
init =
    ( { db = Datalog.empty
      , newContactField = ""
      , lastError = Nothing
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UserTypedInNewContactField input ->
            ( { model | newContactField = input }
            , Cmd.none
            )

        UserClickedAddContact ->
            case Datalog.insert "contact" [ Datalog.string model.newContactField ] model.db of
                Ok newDb ->
                    ( { model
                        | db = newDb
                        , newContactField = ""
                      }
                    , Cmd.none
                    )

                Err problem ->
                    ( { model | lastError = Just problem }
                    , Cmd.none
                    )


view : Model -> Html Msg
view model =
    let
        data =
            Datalog.query [] model.db
    in
    Html.div
        []
        [ Html.form
            [ Events.onSubmit UserClickedAddContact ]
            [ Html.label [] [ Html.text "New contact name" ]
            , Html.input
                [ Attrs.value model.newContactField
                , Events.onInput UserTypedInNewContactField
                ]
                []
            ]
        , Html.h2 [] [ Html.text "Contacts" ]
        , data
            |> Result.map (Database.read "contact")
            |> Debug.toString
            |> Html.text
        , Html.h2 [] [ Html.text "Debug" ]
        , Html.text (Debug.toString model)
        ]
