module Main exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation as Navigation
import Url exposing (Url)


type alias Model =
    { key : Navigation.Key }


type Msg
    = UrlChanged Url
    | UrlRequested UrlRequest


type alias Flags =
    ()


init : Flags -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { key = key }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "Datalog Sample Apps"
    , body = []
    }


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        , onUrlChange = UrlChanged
        , onUrlRequest = UrlRequested
        }
