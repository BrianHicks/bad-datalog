module Main exposing (..)

import Browser
import Datalog.Parser
import Html exposing (..)
import Html.Attributes as Attributes
import Html.Events as Events


type alias Model =
    String


type Msg
    = NewSource String


update : Msg -> Model -> Model
update msg model =
    case msg of
        NewSource source ->
            source


view : Model -> Html Msg
view model =
    main_
        []
        [ textarea
            [ Events.onInput NewSource
            , Attributes.value model
            , Attributes.rows 25
            , Attributes.cols 80
            ]
            []
        , case Datalog.Parser.parse model of
            Ok program ->
                pre [] [ text (Debug.toString program) ]

            Err errors ->
                ul [] (List.map (\err -> li [] [ pre [] [ text err ] ]) errors)
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = ""
        , update = update
        , view = view
        }
