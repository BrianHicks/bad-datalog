module Main exposing (..)

import Browser
import Css
import Datalog.Parser
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes exposing (css)
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
        [ css
            [ Css.maxWidth (Css.px 800)
            , Css.minHeight (Css.vh 100)
            , Css.margin2 Css.zero Css.auto
            , Css.padding (Css.px 20)
            , Css.boxSizing Css.borderBox
            , Css.boxShadow5 Css.zero Css.zero (Css.px 10) (Css.px 1) (Css.rgba 0 0 0 0.25)
            ]
        ]
        [ Html.h1
            [ css [ Css.fontFamily Css.sansSerif ] ]
            [ Html.text "Datalog Time!" ]
        , Html.textarea
            [ Events.onInput NewSource
            , Attributes.value model.source
            , css
                [ Css.width (Css.pct 100)
                , Css.height (Css.px 300)
                , Css.border3 (Css.px 1) Css.solid (Css.hex "AAA")
                , Css.borderRadius (Css.px 10)
                ]
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
