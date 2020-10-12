module Main exposing (..)

import Browser
import Datalog.Parser
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events


type alias Model =
    { source : String }


type Msg
    = NewSource String


update : Msg -> Model -> Model
update msg model =
    case msg of
        NewSource source ->
            { model | source = source }


view : Model -> Html Msg
view model =
    Html.main_
        []
        [ Html.textarea
            [ Events.onInput NewSource
            , Attributes.value model.source
            , Attributes.rows 25
            , Attributes.cols 80
            ]
            []
        , case Datalog.Parser.parse model.source of
            Ok program ->
                Html.pre [] [ Html.text (Debug.toString program) ]

            Err errors ->
                Html.ul [] (List.map (\err -> Html.li [] [ Html.pre [] [ Html.text err ] ]) errors)
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = { source = "" }
        , update = update
        , view = Html.toUnstyled << view
        }
