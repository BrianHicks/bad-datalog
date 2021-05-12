module AddressBook exposing (..)

import Database.Datalog as Datalog exposing (Database)
import Html.Styled as Html exposing (Html)


type alias Model =
    { db : Database
    , newContact : String
    }


type alias Msg =
    ()


init : ( Model, Cmd Msg )
init =
    ( { db = Datalog.empty
      , newContact = ""
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Html.text (Debug.toString model)
